##### BAR GRAPHS Chapter2 #####

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)

# Examine how abundance and richness of Cnidaria and Porifera change across seamounts
# Per transect

# Base path to the directory containing seamount folders
base_folder_path <- "/Users/user/Desktop/"

# List of seamount folder names
seamount_folders <- c("coral_biigle_feb/8.coral_biigle_50m_transects", 
                      "sapmer_biigle_feb/8.sapmer_50m_transects",
                      "atlantis_biigle_feb/8.atlantis_50m_transects", 
                      "melville_biigle_feb/8.melville_biigle_50m_transects"
                      
) # Add more as needed

# Define the taxonomic groups of interest
taxa <- c("Porifera", "Cnidaria", "Bryozoa", "Stalked crinoids")




#####

process_seamount <- function(folder_name, base_folder_path, taxa) {
  folder_path <- file.path(base_folder_path, folder_name)
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  transect_data <- map_dfr(files, function(file) {
    data <- read_csv(file)
    
    data <- data %>%
      filter(depth >= 0 & depth <= 2000, 
             str_detect(label_hierarchy, paste(taxa, collapse = "|"))) %>%
      mutate(taxonomic_group = case_when(
        str_detect(label_hierarchy, "Cnidaria") ~ "Cnidaria",
        str_detect(label_hierarchy, "Porifera") ~ "Porifera", 
        str_detect(label_hierarchy, "Bryozoa") ~ "Bryozoa", 
        str_detect(label_hierarchy, "Stalked crinoids") ~ "Stalked crinoids"),
        transect = tools::file_path_sans_ext(basename(file)), # Calculate transect here, before summarising
        seamount = str_replace(folder_name, ".+/([^.]+)$", "\\1") # Extract seamount name from folder path
      )
    
    if (nrow(data) > 0) {
      data %>%
        group_by(transect) %>%
        summarise(
          richness = n_distinct(label_name), # Counts unique species names
          abundance = n(), # Counts the number of individuals
          .groups = 'drop'
        ) %>%
        mutate(seamount = str_extract(folder_name, "[^/]+$")) # Ensure seamount name is included
    } else {
      return(tibble())  # Return an empty tibble if the depth condition is not met
    }
  })
  
  # Adjust summary calculation here to focus on overall richness without grouping by taxonomic_group
  summary_data <- transect_data %>%
    group_by(seamount) %>%
    summarise(
      average_richness = mean(richness),
      sd_richness = sd(richness),
      sem_richness = sd_richness / sqrt(n()),
      average_abundance = mean(abundance),
      sd_abundance = sd(abundance),
      sem_abundance = sd_abundance / sqrt(n()),
      .groups = 'drop'
    )
  
  return(summary_data)
}




# Define a wrapper function to pass all arguments to 'process_seamount'
process_seamount_wrapper <- function(folder_name) {
  process_seamount(folder_name, base_folder_path, taxa)
}

# Apply the 'process_seamount_wrapper' function to each folder in 'seamount_folders'
all_data <- lapply(seamount_folders, process_seamount_wrapper)


# Combine the _ata from all seamounts into one data frame
combined_data <- bind_rows(all_data)


# clean seamount names 

combined_data <- combined_data %>%
  mutate(seamount = case_when(
    str_detect(seamount, "atlantis") ~ "Atlantis",
    str_detect(seamount, "sapmer") ~ "Sapmer",
    str_detect(seamount, "melville") ~ "Melville Bank",
    str_detect(seamount, "coral") ~ "Coral",
    TRUE ~ seamount # Default case to keep original value if none of the above conditions are met
  ))



ggplot(combined_data, aes(x = seamount, y = average_richness, fill = seamount)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) + # Draw bars for average richness
  geom_errorbar(aes(ymin = average_richness - sem_richness, ymax = average_richness + sem_richness), 
                width = 0.25, position = position_dodge(0.7)) + # Add error bars
  scale_x_discrete(limits = c("Atlantis", "Sapmer", "Melville Bank", "Coral")) +
  scale_fill_manual(values = c("Sapmer" = "purple4", "Coral" = "royalblue", "Atlantis" = "orange", "Melville Bank" = "palevioletred1")) +
  labs(title = "", x = "Seamount", y = "Average Morphospecies Richness Per Transect") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve label readability


ggplot(combined_data, aes(x = seamount, y = average_abundance, fill = seamount)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) + # Draw bars for average richness
  geom_errorbar(aes(ymin = average_abundance - sem_abundance, ymax = average_abundance + sem_abundance), 
                width = 0.25, position = position_dodge(0.7)) + # Add error bars
  scale_x_discrete(limits = c("Atlantis", "Sapmer", "Melville Bank", "Coral")) +
  scale_fill_manual(values = c("Sapmer" = "purple4", "Coral" = "royalblue", "Atlantis" = "orange", "Melville Bank" = "palevioletred1")) +
  # Optional: Use a color palette for visual distinction
  labs(title = "", x = "Seamount", y = "Average Morphospecies Abundance Per Transect") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve label readability

