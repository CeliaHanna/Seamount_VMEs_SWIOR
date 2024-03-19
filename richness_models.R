#### CHAPTER 2 REVISED ANALYSIS #####

library(dplyr)
library(ggplot2)
library(glmmTMB)

##### filter transects for just those within 600-800m depth range ######

all_transects <- read.csv("/Users/user/Desktop/all_transects.csv")
all_transects_600_800 <- all_transects[all_transects$AverageDepth >= 600 & all_transects$AverageDepth <= 800, ]

# Standardise env variables 
# Adding standardized columns to the dataframe

all_transects_600_800 $StandardisedDepth <- scale(all_transects_600_800$AverageDepth, center = TRUE, scale = TRUE)
all_transects_600_800 $StandardisedSalinity <- scale(all_transects_600_800$AverageSalinity, center = TRUE, scale = TRUE)
all_transects_600_800 $StandardisedTemperature<- scale(all_transects_600_800$AverageTemperature, center = TRUE, scale = TRUE)
all_transects_600_800 $StandardisedProductivity<- scale(all_transects_600_800$AverageProductivity, center = TRUE, scale = TRUE)
all_transects_600_800 $StandardisedGradient <- scale(all_transects_600_800$AverageGradient, center = TRUE, scale = TRUE)

# Adding dive number

all_transects_600_800$dive <- as.numeric(gsub(".*[Dd][Ii][Vv][Ee]([0-9]+).*", "\\1",all_transects_600_800$Transect))

# Checking distribution of response variable

hist(all_transects_600_800$Richness)

# See how many transects per depth zone 

seamount_counts <- all_transects_600_800 %>%
  group_by(Seamount) %>%
  summarise(Count = n())

all_transects_600_800 <- read.csv('/Users/user/Desktop/all_600_800.csv') ## FROM DESKTOP DONT UPDATE 
#### Initial boxplots ######

par(mfrow=c(2,1))
par(mar = c(4,4, 2, 3))
dev.off()

boxplot(Richness ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all_transects_600_800,
        xlab = "Seamount", ylab = "VME Morphospecies Richness",
        main = "",
        col = "#56B4E9", # Change color as needed
        border = "darkblue",
        cex.lab = 0.8) # Change border color as needed

boxplot(Abundance ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all_transects_600_800,
        xlab = "Seamount", ylab = "VME Morphospecies Abundance",
        main = "",
        col = "#CC79A7", # Change color as needed
        border = "darkblue",
        cex.lab = 0.8) # Change border color as needed



dev.off()

##### Create a balanced dataset and test the patterns in richness and abundance found ####

balanced_data_set <- all_transects_600_800 %>%
  group_by(Seamount) %>%
  slice_head(n=11) %>%
  ungroup()



### Boxplots using balanced dataset 


boxplot(Richness ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = balanced_data_set ,
        xlab = "Seamount", ylab = "VME Morphospecies Richness",
        main = "",
        col = "skyblue", # Change color as needed
        border = "darkblue") # Change border color as needed

boxplot(Abundance ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = balanced_data_set ,
        xlab = "Seamount", ylab = "VME Morphospecies Richness",
        main = "",
        col = "skyblue", # Change color as needed
        border = "darkblue") # Change border color as needed


### Trend of increasing richness stil holds for this dataset 







#### SCALE ENVIRONMENTAL VARIABLES ####


all_transects_600_800$StandardisedDepth <- scale(all_transects_600_800AverageDepth, center = TRUE, scale = TRUE)
all_transects_600_800$StandardisedSalinity <- scale(all_transects_600_800$AverageSalinity, center = TRUE, scale = TRUE)
all_transects_600_800$StandardisedTemperature<- scale(all_transects_600_800$AverageTemperature, center = TRUE, scale = TRUE)
all_transects_600_800$StandardisedProductivity<- scale(all_transects_600_800$AverageProductivity, center = TRUE, scale = TRUE)
all_transects_600_800$StandardisedGradient <- scale(all_transects_600_800$AverageGradient, center = TRUE, scale = TRUE)
all_transects_600_800$StandardisedDensity <- scale(all_transects_600_800$density, center = TRUE, scale = TRUE)


#### ADDING DENSITY AND PLOTTING ######


# Load the necessary library
library(gsw)

# Assuming you've read your dataset into a dataframe named 'data'
# data <- read.csv("path_to_your_file/all_600_800.csv")

# Calculate Absolute Salinity (SA) and Conservative Temperature (CT) which are required for density calculation
SA <- gsw_SA_from_SP(all_transects_600_800$AverageSalinity, all_transects_600_800$AveragePressure, longitude = 0, latitude = 0) # Assuming a generic longitude and latitude
CT <- gsw_CT_from_t(SA, all_transects_600_800$AverageTemperature, all_transects_600_800$AveragePressure)

# Calculate density
density <- gsw_rho(SA, CT, all_transects_600_800$AveragePressure)

# You can add the density back to your dataframe if you want
all_transects_600_800$density <- density

# View the modified dataframe
head(all_transects_600_800)



#### CONVERT SUBSTRATES TO NUMERIC ####

# Convert substrate type to numeric values 

all_transects_600_800 <- all_transects_600_800 %>%
  mutate(substrate_numbers = case_when(
    substrate == "Volcanic" ~ 1,
    substrate == "volcanic_sediment" ~ 2,
    substrate == "Sediment" ~ 3,
    substrate == "biogenic_matrix" ~ 4,
    substrate == "coral_rubble" ~ 5,
    TRUE ~ NA_integer_  # Assign NA to any Substrate not listed above
  ))

# make into factor 

all_transects_600_800$substrate<- as.factor(all_transects_600_800$substrate)


### plotting substrates per seamount 

# Assuming your dataframe is named df
# Plotting the number of transects per substrate type
ggplot(data =all_transects_600_800, aes(x = substrate)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Number of Transects per Substrate Type",
       x = "Substrate Type", 
       y = "Count of Transects") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # This helps if substrate names are long/overlap


#### BOXPLOTS OF TEMPERATURE, SALINITY AND DENSITY PER SEAMOUNT #####
dev.off()
par(mfrow=c(3,1))
par(mar = c(4, 4, 2, 2))


boxplot(AverageTemperature ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all_transects_600_800,
        xlab = "", ylab = "Temperature",
        main = "",
        col = "skyblue", # Change color as needed
        border = "black") # Change border color as needed


boxplot(AverageSalinity ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all_transects_600_800,
        xlab = "", ylab = "Salinity",
        main = "",
        col = "orange", # Change color as needed
        border = "black") # Change border color as needed


boxplot(density ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all_transects_600_800,
        xlab = "Seamount", ylab = "Density",
        main = "",
        col = "pink", # Change color as needed
        border = "black") # Change border color as needed


boxplot(AverageProductivity ~ factor(Seamount, levels = c("Atlantis", "Sapmer", "Melville Bank", "Coral")), 
        data = all_transects_600_800,
        xlab = "Seamount", ylab = "Density",
        main = "",
        col = "pink", # Change color as needed
        border = "black") # Change border color as needed



### ANOVA TO DETECT DENSITY, TEMP and SALINITY DIFFERENCES BETWEEN SEAMOUNTS ####


# Ensure 'seamount' is a factor
all_transects_600_800$Seamount <- as.factor(all_transects_600_800$Seamount)

# Perform ANOVA
anova_result_density <- aov(density ~ Seamount, data = all_transects_600_800)
anova_result_salinity <- aov(AverageSalinity ~ Seamount, data = all_transects_600_800)
anova_result_temperature <- aov(AverageTemperature ~ Seamount, data = all_transects_600_800)
anova_result_productivity <- aov(AverageProductivity ~ Seamount, data = all_transects_600_800)


# View the summary ANOVA table
summary(anova_result_density)
summary(anova_result_salinity)
summary(anova_result_temperature)
summary(anova_result_productivity) 

posthoc_result_density <- TukeyHSD(anova_result_density)
posthoc_result_salinity <- TukeyHSD(anova_result_salinity)
posthoc_result_temperature <- TukeyHSD(anova_result_temperature)
posthoc_result_productivity <- TukeyHSD(anova_result_productivity)


print(posthoc_result_density)
print(posthoc_result_productivity)
print(posthoc_result_salinity)
print(posthoc_result_temperature)



###### ANOVA TO DETECT RICHNESS DIFFERENCES #######

hist(all_transects_600_800$Richness)

hist(sqrt(all_transects_600_800$Richness)) # trying to make it more normal 

# ANOVA


# Convert seamount to a factor if it's not already
all_transects_600_800$Seamount <- as.factor(all_transects_600_800$Seamount)

# Perform ANOVA
anova_result <- aov(Richness~ Seamount, data = all_transects_600_800)

anova_result <- aov(Richness ~ Seamount, data = balanced_data_set) # not significant but we still see same relationships 

# View the ANOVA table
summary(anova_result)   # significant difference detected in at least one group

# Perform Tukey HSD test to see where differences are 

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Most significant difference is between Coral and Atlantis 
# Inkeeping that richness increases from north to south 


##### ENV CORRELATION TEST #####

# make a column for seamount thats numeric (number assigned to seamount instead of names)

all_transects_600_800 <- all_transects_600_800 %>%
  mutate(seamount_number = case_when(
    Seamount == "Atlantis" ~ 1,
    Seamount == "Sapmer" ~ 2,
    Seamount == "Melville Bank" ~ 3,
    Seamount == "Coral" ~ 4,
    TRUE ~ NA_real_ # Assign NA to any seamount not listed above
  ))


# convert substrate tyoe and SAI_evidence to a factors

all_transects_600_800$substrate <- factor(all_transects_600_800$substrate)
all_transects_600_800$SAI_Evidence<- factor(all_transects_600_800$SAI_Evidence)

env_variables_600_800 <- dplyr::select(all_transects_600_800,seamount_number, StandardisedTemperature, StandardisedSalinity, StandardisedGradient,
                                       StandardisedDepth, StandardisedProductivity) 

dev.off()

cor_matrix <- cor(env_variables_600_800, method = "pearson")

pairs(env_variables_600_800)

corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         addCoef.col = "black", # Add correlation coefficients to the plot
         tl.cex = 0.6) # Adjust text label size


pairs(env_variables_all_transects)

# Seamount and productivity are too correlated, so remove from the model


###### MODEL SENSITIVITY ANALYSIS #####


###     410.0  

all_transects_600_800 <- read.csv('/Users/user/Desktop/all_600_800.csv') ## FROM DESKTOP DONT UPDATE 

N_model <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + StandardisedDepth + SAI_Evidence + substrate) 
                   + (1|dive), data = all_transects_600_800, family = nbinom2)


summary(N_model)


### USING POISSON

## without zero inflation: AIC =      450.6 

p_model <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + StandardisedDepth + StandardisedGradient 
                               + StandardisedProductivity + SAI_Evidence + substrate) + 
                              (1|dive), data = all_transects_600_800, family = poisson)

summary(p_model)

## with zero inflation: AIC =  443.2  

p_model_zero_inflated <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + StandardisedDepth +
                                               StandardisedGradient + StandardisedProductivity + SAI_Evidence + substrate)
                                 + (1|dive), ziformula = ~1, data = all_transects_600_800, family = poisson)

summary(p_model_zero_inflated)

### USING NEGATIVE BINOMIAL 

# not accounting for 0 inflation: AIC =     411.1  
N_model <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + 
                                 StandardisedDepth + StandardisedGradient 
                               + SAI_Evidence + substrate)
                   + (1|dive), data = all_transects_600_800, family = nbinom2)

summary(N_model)


# Let's say "desired_reference_level" is the level you want as the new reference
all_transects_600_800$substrate <- relevel(all_transects_600_800$substrate, ref = "Sediment")

# Now, fit the model with the new reference level for substrate
N_model <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + 
                                 StandardisedDepth + StandardisedGradient 
                               + SAI_Evidence + substrate)
                   + (1|dive), data = all_transects_600_800, family = nbinom2)

# Summary of the model to see the effects with the new reference level
summary(N_model)

# Accounting for 0 inflation: AIC =    414    

N_model_zero_inflated <- glmmTMB(Richness ~ (StandardisedTemperature + StandardisedSalinity + 
                                               StandardisedDepth + StandardisedGradient + substrate
                                             + SAI_Evidence ) + (1|dive) , data = all_transects_600_800,  ziformula = ~1, family = nbinom2)

summary(N_model_zero_inflated)

##### COMMUNITY COMPOSITION ANALYSIS #####

folder_path <- "/Users/user/Desktop/all_transects_new"

# making a species abundance matrix 

### select data from 600-800m 


csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Function to process each file
process_file <- function(file_path) {
  data <- read_csv(file_path, col_types = cols(depth = col_double(), seamount = col_character()))
  
  # Filter rows where depth is within the specified range
  filtered_data <- data %>% filter(depth >= 600 & depth <= 800)
  
  # If the filtered dataset is not empty, return it with an additional 'file_name' column
  if(nrow(filtered_data) > 0) {
    file_name <- basename(file_path)
    return(mutate(filtered_data, file_name = file_name))
  } else {
    return(NULL)
  }
}

# Apply the function to each file and combine the results
filtered_data <- bind_rows(lapply(csv_files, process_file), .id = "id")

# To know how many transects there are per seamount 

files_per_seamount <- filtered_data %>%
  group_by(seamount) %>%
  summarise(NumberOfFiles = n_distinct(file_name))


# Find the depth range of each 

depth_range_per_seamount <- filtered_data_VME %>%
  group_by(seamount) %>%
  summarise(
    MinDepth = round(min(depth, na.rm = TRUE),2),
    MaxDepth = round(max(depth, na.rm = TRUE),2)
  )

depth_range_per_seamount


# filter for VME 

filtered_data_VME <- filtered_data %>%
  filter(str_detect(label_hierarchy, "Cnidaria") |
           str_detect(label_hierarchy, "Porifera") | 
           str_detect(label_hierarchy, "Bryozoa") |
           str_detect(label_hierarchy, "Stalked crinoids"))


# make species abunance matrix 

# Step 2: Aggregate data
species_counts <- filtered_data_VME %>%
  group_by(seamount, label_hierarchy) %>%
  summarise(count = n(), .groups = 'drop')

# Step 3: Create species abundance matrix
species_abundance_matrix <- species_counts %>%
  pivot_wider(names_from = label_hierarchy, values_from = count, values_fill = list(count = 0))

# View the resulting matrix
print(species_abundance_matrix)

# See sampling completeness 

species_abundance_numeric <- species_abundance_matrix[, -1]

# Assuming diversity metrics have been calculated as shannon, simpson, and evenness

# Convert diversity metrics to a data frame
diversity_df <- data.frame(
  Sample = rownames(species_abundance_matrix), # This assumes row names are your labels
  Shannon = shannon,
  Simpson = simpson,
  Evenness = evenness,
)

# Melting the data frame to use with ggplot2
library(tidyr)
diversity_long <- pivot_longer(diversity_df, -Sample, names_to = "Metric", values_to = "Value")

# Plotting with ggplot2
library(ggplot2)
ggplot(diversity_long, aes(x = Sample, y = Value, color = Metric)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Adjust text angle for better readability
  labs(title = "Diversity Metrics", x = "Sample", y = "Value") +
  geom_line(aes(group = Sample), alpha = 0.5) # Optional: Connect points of the same sample with lines

# Show the plot





####
rowSums(species_abundance_numeric)

rarecurve(species_abundance_numeric)

rarefied <- rrarefy(species_abundance_numeric, 67)

rarefied_pa <- species_abundance_numeric > 0 

dist_rarefied <- vegdist(rarefied_pa, method = "jaccard")

## insufficient data for NDMS
# Perform PCoA
pcoa_results <- cmdscale(dist_rarefied, eig = TRUE, k = 2) # Assuming 2 dimensions for simplicity

# Convert to data frame
pcoa_df <- as.data.frame(pcoa_results$points)
names(pcoa_df) <- c("PCoA1", "PCoA2")

# Add row labels from the distance matrix as a new column for plotting
pcoa_df$Labels = rownames(species_abundance_matrix)

# Calculate the percentage of variation explained for axis labels
variation_explained <- pcoa_results$eig / sum(pcoa_results$eig) * 100

ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, label = Labels)) +
  geom_point() + # Plot points
  geom_text(aes(label = Labels), vjust = -1, hjust = 0.5, size = 3) + # Add labels
  xlab(paste("PCoA1 -", round(variation_explained[1], 2), "%")) + # Label x-axis with % variation
  ylab(paste("PCoA2 -", round(variation_explained[2], 2), "%")) + # Label y-axis with % variation
  theme_minimal() + # Use a minimal theme
  ggtitle("PCoA of Bray-Curtis Distance with Labels") # Add a title







#### PCA OF ENVIRONMENTAL VARIBALES #####


env_data_600_800 <-  all_transects_600_800[c("AverageTemperature", "AverageSalinity", "Seamount", "Richness",
                                             "AverageProductivity", "AverageGradient", 'seamount_number')]


# Selecting only the relevant variables for PCA
pca_data <- env_data_600_800 %>%
  dplyr::select(AverageTemperature, AverageSalinity, Richness, AverageProductivity, AverageGradient) %>%
  scale()    # Standardises the variables to a mean of 0 and a standard deviation of one

# Performing PCA
PCA = princomp(pca_data)

pca_scores <- data.frame(PCA$scores) %>%
  mutate(Seamount = env_data_600_800$Seamount)
pca_scores

# Define your custom color palette
custom_colors <- c('Atlantis' = 'red','Coral' = 'chartreuse4', 'Melville Bank' = 'cornflowerblue','Sapmer' = 'orange') 

# Plot PCA with ellipses
ggplot(pca_scores, aes(x = Comp.1, y = Comp.2, color = Seamount)) +
  geom_point() + # Add points
  stat_ellipse(type = "t", level = 0.95, linetype = "dashed", size = 0.5) + # Add ellipses
  theme_minimal() +
  labs(title = "",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  scale_color_manual(values = custom_colors)

# Accessing the loadings
loadings <- PCA$loadings

# Viewing the loadings
print(loadings)

rarecurve(species_abundance_numeric)

