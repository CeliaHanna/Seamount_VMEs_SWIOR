library(tidyverse)
library(readr)
library(dplyr)
library(purrr)
library(lme4)
library(MASS)
library(glmmTMB)
library(gridExtra)


# Directory containing the transect CSV files
transect_dir <- ("/Users/user/Desktop/metadata_flow/melville_50m_transects")

# Updated function to process each CSV file
process_transect_file <- function(file_path) {
  # Read the CSV file
  transect_data <- read_csv(file_path, col_types = cols())
  
  # Calculate species richness
  richness <- length(unique(transect_data$label_name))
  
  # Calculate species abundance (total number of observations)
  species_abundance <- nrow(transect_data)
  
  # Calculate frequencies for Shannon diversity if there are any observations
  if(species_abundance > 0) {
    species_frequencies <- table(transect_data$label_name) / species_abundance
    shannon_diversity <- -sum(species_frequencies * log(species_frequencies))
    
    # Calculate Pielou's evenness (J')
    pielou_evenness <- shannon_diversity / log(richness)
  } else {
    # Assign default values if there are no observations
    shannon_diversity <- NA
    pielou_evenness <- NA
  }
  
  # Determine depth zone based on the mean depth
  depth_zone <- cut(mean(transect_data$depth, na.rm = TRUE),
                    breaks = c(100, 300, 500, 700, 900, 1100, 1310),
                    labels = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6"),
                    include.lowest = TRUE, right = FALSE)
  
  # Extract dive number from the file name
  dive_number <- gsub(".*dive([0-9]+).*", "\\1", basename(file_path))
  
  # Return a tibble with the calculated metrics and depth zone
  tibble(dive_number = dive_number,
         richness = richness,
         abundance = species_abundance,
         shannon_diversity = shannon_diversity,
         pielou_evenness = pielou_evenness,
         depth_zone = as.character(depth_zone)) # Ensure depth_zone is treated as character for consistency
}

# Directory containing CSV files
transect_dir <- "/Users/user/Desktop/metadata_flow/melville_50m_transects"

# List all CSV files in the directory
csv_files <- list.files(transect_dir, pattern = "\\.csv$", full.names = TRUE)

# Process each file and combine the results into a single dataframe
transect_info <- map_dfr(csv_files, process_transect_file)

# Print the resulting dataframe
print(transect_info)


### PLOTTING RICHNESS 


# Assuming your data frame is named transect_info
average_richness <- transect_info %>%
  filter(!is.na(depth_zone)) %>% 
  group_by(depth_zone) %>%
  summarise(
    mean_richness = mean(richness, na.rm = TRUE),
    sd_richness = sd(richness, na.rm = TRUE),
    n = n(),
    se_richness = sd_richness / sqrt(n)
  ) %>%
  select(depth_zone, mean_richness, se_richness)

p1 <- ggplot(average_richness, aes(x = depth_zone, y = mean_richness, fill = depth_zone)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_richness - se_richness, ymax = mean_richness + se_richness),
                width = 0.25, position = position_dodge(0.7)) +
  labs(title = "a)",
       x = "Depth Zone",
       y = "Mean Morphospecies Richness") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(legend.title = element_blank())





## PLOTTING ABUNDANCE 


average_abundance <- transect_info %>%
  filter(!is.na(depth_zone)) %>%  # Exclude rows where depth_zone is NA
  group_by(depth_zone) %>%
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    sd_abundance = sd(abundance, na.rm = TRUE),
    n = n(),
    se_abundance = sd_abundance / sqrt(n)
  ) %>%
  select(depth_zone, mean_abundance, se_abundance)


p2 <- ggplot(average_abundance, aes(x = depth_zone, y = mean_abundance, fill = depth_zone)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_abundance - se_abundance, ymax = mean_abundance + se_abundance),
                width = 0.25, position = position_dodge(0.7)) +
  labs(title = "b)",
       x = "Depth Zone",
       y = "Mean Morphospecies Abundance") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(legend.title = element_blank())



# combine the plot 


# Combine plots into a single panel plot with 1 column and 2 rows
panel_plot <- p1 / p2

# Print the combined plot
panel_plot


### Plotting evenness

evenness_summary <- transect_info %>%
  group_by(depth_zone) %>%
  summarise(
    mean_pielou_evenness = mean(pielou_evenness, na.rm = TRUE),
    sd_pielou_evenness = sd(pielou_evenness, na.rm = TRUE)
  )

ggplot(evenness_summary, aes(x = depth_zone, y = mean_pielou_evenness, fill = depth_zone)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_pielou_evenness - sd_pielou_evenness, ymax = mean_pielou_evenness + sd_pielou_evenness),
                width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Average Pielou's Evenness by Depth Zone",
       x = "Depth Zone", y = "Average Pielou's Evenness") +
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.title = element_blank())


