### generating dataset for community analyses Chpt.1
# Load required libraries

library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(vegan)
library(purrr)
library(ggplot2)
library(glmmTMB)

### Building dataframe of species abundances ######
# Set your working directory to the folder containing your CSV files
setwd("/Users/user/Desktop/melville_biigle_feb/melville_transects")

## Define a function to process each file
process_file <- function(file_name) {
  data <- read_csv(file_name)
  
  # Aggregate species count by depth zone and label name
  aggregated_data <- data %>%
    group_by(depth_zone, label_hierarchy) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = label_hierarchy, values_from = n, values_fill = list(n = 0)) %>%
    ungroup() %>%
    mutate(transect = file_name) # Add the file name as a transect identifier
  
  return(aggregated_data)
}

# Apply the function to each file and combine the results
transect_data_list <- lapply(csv_files, process_file)
combined_data <- bind_rows(transect_data_list)

# Move 'transect' column to the first position
reordered_data <- combined_data %>% 
  select(transect, everything())

# Now 'transect' will be the first column, and the rest of the columns will follow in their original order

# Make all NA values 0

reordered_data[is.na(reordered_data )] <- 0

#### Just selecting VME species #####

# Find column indices that contain 'Cnidaria' or 'Porifera'
cnidaria_porifera_indices <- grep("Cnidaria|Porifera", names(reordered_data))

# Always include the first two columns (transect and depth_zone)
initial_columns <- 1:2

# Combine the column indices
final_column_indices <- c(initial_columns, cnidaria_porifera_indices)

# Subset the dataframe to include only the desired columns
final_df <- reordered_data[, final_column_indices]

# View the first few rows of the new dataframe to confirm it's correct
head(final_df)



## Adding columns ### 

# Calculate total abundance per depth zone
depth_zone_abundance <- aggregate(comm_data, by=list(reordered_data$depth_zone), sum)

# Identify the dominant species in each depth zone
dominant_species <- lapply(depth_zone_abundance[, -1], function(zone_abundance) {
  max_species <- colnames(depth_zone_abundance)[which.max(zone_abundance)]
  return(max_species)
})

# Combine the results into a dataframe
dominant_species_df <- data.frame(depth_zone = depth_zone_abundance$Group.1, dominant_species = unlist(dominant_species))

# Print or view the dominant species dataframe
print(dominant_species_df)


#### Adding a richness column 


# Step 1: Subset the DataFrame to include only the species columns
species_data <- final_df[,3:74]  # Adjust this as necessary based on your DataFrame structure

# Step 2: Calculate species richness per row
# Assuming species presence is indicated by a number greater than 0
species_richness <- rowSums(species_data > 0)

# Step 3: Add this as a new column to your original DataFrame
final_df$species_richness <- species_richness


# Assuming your dataframe is named df

# Step 1: Calculate mean species richness per depth zone
mean_richness <- final_df %>%
  group_by(depth_zone) %>%
  summarise(mean_species_richness = mean(species_richness, na.rm = TRUE))

# Step 2: Plotting
ggplot(mean_richness, aes(x = depth_zone, y = mean_species_richness)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(x = "Depth Zone", y = "Mean Species Richness", title = "Mean Species Richness by Depth Zone") +
  geom_text(aes(label = round(mean_species_richness, 2)), vjust = -0.5)  # Optional: Add text labels to the bars

hist(melville_data$species_richness)


# Move richness column forward 


# Names of all columns, assuming 'species_richness' is already in the dataframe
current_names <- names(final_df)

# Remove 'species_richness' from its current position
current_names <- current_names[current_names != "species_richness"]

# Insert 'species_richness' as the third column
new_order <- c(current_names[1:2], "species_richness", current_names[3:length(current_names)])

# Reorder columns in the dataframe
final_df <- final_df[, new_order]

# Check the new order of columns
head(final_df)


### Adding an abundance column 

# This remains unchanged as you're still working with the same species data columns
species_data <- final_df [,4:75]  # Adjust this based on your DataFrame

# Step 2: Calculate species abundance per row
# Sum up the values directly for abundance
species_abundance <- rowSums(species_data)

# Step 3: Add this as a new column to your original DataFrame
final_df$species_abundance <- species_abundance


# move abundance column 

# Step 1: Get all column names
columns <- names(final_df)

# Step 2: Remove 'species_richness' from its current place and insert it as the third column
# Remove 'species_richness' from its current position
columns <- columns[columns != "species_abundance"]

# Insert 'species_richness' at the third position
new_order <- c(columns[1:2], "species_abundance", columns[4:length(columns)])

# Step 3: Reorder the dataframe columns
melville_data <- final_df[, new_order]

head(melville_data)
## Step 1: Calculate mean species abundance per depth zone
mean_abundance<- melville_data %>%
  group_by(depth_zone) %>%
  summarise(mean_species_abundance= mean(species_abundance, na.rm = TRUE))

# Step 2: Plotting
ggplot(mean_abundance, aes(x = depth_zone, y = mean_species_abundance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(x = "Depth Zone", y = "Mean Species Abundance", title = "Mean Species Abundnace by Depth Zone") +
  geom_text(aes(label = round(mean_species_abundance, 2)), vjust = -0.5)  # Optional: Add text labels to the bars

hist(melville_data$species_richness)



#### SIMPER ANALYSIS #### 
# Assuming your dataframe is named 'abundance_data'

simper_results <- simper(comm_data, group = final_df$depth_zone)
summary(simper_results)
print(simper_results)

bray


## Investigating abundance distribution of soft yellow fan 

# Summarize the abundance of "DIO Acanthogorgia soft yellow fan" by depth zone
abundance_summary <- final_df%>%
  group_by(depth_zone) %>%
  summarise(Acanthogorgia_Abundance = sum(`Biota > Animalia > Cnidaria > Anthozoa > Octocorallia > Alcyonacea > Acanthogorgiidae > DIO Acanthogorgia soft yellow fan`, na.rm = TRUE))

# Print the summary
print(abundance_summary)



### ANOSIM  #####

ano <- anosim(comm_data,final_df$depth_zone, distance = "jaccard", permutations = 9999)
summary(ano)
# results suggest there is a significant difference in similarities based on depth zone 

### Visualising ANOSIM using NMDs clustering ####

# Perform NMDS as before
comm_dist <- vegdist(comm_data, method = "jaccard")
nmds <- metaMDS(comm_dist, trymax = 100)
# Assuming nmds is your metaMDS object
nmds_scores <- nmds$points

# Convert NMDS scores to a data frame
df_nmds <- as.data.frame(nmds_scores)
df_nmds$depth_zone <- final_df$depth_zone


# Plot NMDS results with ggplot2
ggplot(df_nmds, aes(x = MDS1, y = MDS2, color = depth_zone)) +
  geom_point() +  # Plot points
  theme_minimal() +
  labs(x = "NMDS Axis 1", y = "NMDS Axis 2", color = "Depth Zone") +
  scale_color_brewer(palette = "Set1")  # Use a predefined color palette



#### INDICATOR SPECIES ANALYSIS ####

library(indicspecies)

inv <- multipatt(comm_data, reordered_data$depth_zone, func = "r.g", control = how(nperm=9999))
summary(inv)
dist <- vegdist(comm_data, method = "jaccard")
p <- hclust(dist)
plot(p)


# which species dominate each depth zone





### INVESTIGATING SAI EVIDENCE #####

# add SAI evidence column 

final_dfSAI_Evidence <- NA

# Create or update the 'SAI_Evidence' column based on 'transect' values
final_df$SAI_Evidence <- ifelse(grepl("dive9|dive8", final_df$transect), "Y", "N")


# Assuming 'df' is your DataFrame

# Assign "Y" to rows 46 to 49 in the 'SAI_Evidence' column
final_df$SAI_Evidence[46:49] <- "Y"


# Fitting the model using glmmTMB
m <- glmmTMB(species_richness ~ SAI_Evidence + (1|depth_zone) + (1|dive_number),
             family=nbinom2, data=final_df)

# View the summary of the model
summary(m)



## add a dive number transcet 

# Extract dive number using sub
final_df$dive_number <- as.integer(sub(".*dive(\\d+).*", "\\1", final_df$transect))

# View the first few rows to confirm the new column
colnames(final_df)

# now plot SAI Evidence against richness


# Assuming your dataframe is named df and 
# it contains 'richness' and 'SAI_Evidence' columns

# Enhanced plot code with larger axis label names
ggplot(final_df, aes(x = SAI_Evidence, y = species_richness, fill = SAI_Evidence)) +
  geom_boxplot() +
  labs(x = "SAI Evidence", y = "Morphospecies Richness", title = "") +
  theme_minimal() +
  scale_fill_manual(values = c("Y" = "red2", "N" = "#117733")) +
  theme(axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        axis.title.x = element_text(size = 14), # Increase x-axis label name size
        axis.title.y = element_text(size = 14)) # Increase y-axis label name size



###Plotting high level VME groups per depth zone###

high_level_melville <- read_csv("~/Desktop/final_filtered_aggregated.csv")


colnames(high_level_melville)






# Boxplot of depth zones and sai evidence 



# Load required library
library(ggplot2)

# Assuming your dataframe is named 'df'
# You may need to install 'ggplot2' package if you haven't already: install.packages("ggplot2")

# Filter the dataframe to include only 'Y' values for SAI Evidence
df_y <- final_df[final_df$SAI_Evidence == 'Y', ]

# Create a bar plot
ggplot(df_y, aes(x = depth_zone)) +
  geom_bar(fill = "red2") +
  labs(title = "",
       x = "Depth Zone",
       y = "Transect Number") + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        axis.title.x = element_text(size = 14), # Increase x-axis label name size
        axis.title.y = element_text(size = 14)) + # Increase y-axis label name size
  theme(panel.background = element_rect(fill = "white"),
      panel.border = element_blank())


