library(tidyverse)

# Set the working directory to the folder containing the CSV files
setwd("/Users/user/Desktop/melville_biigle_feb/melville_transects")

## Read all CSV files in the directory
file_list <- list.files(pattern="*.csv")
transects_data <- file_list %>%
  map_df(~read_csv(.))

# Check for any necessary data cleaning or renaming if 'depth_zone' is not standardized
transects_data <- transects_data %>%
  mutate(depth_zone = as.factor(depth_zone))

# set panel plot


# Plot boxplots for Salinity, Temperature, and Pressure
transects_data %>%
  pivot_longer(cols = c(CTD.Salinity, CTD.Temperature, CTD.Pressure), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = depth_zone, y = Value, fill = depth_zone)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "", x = "Depth Zone", y = "Value") +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
        axis.text.y = element_text(size = 14))  # Increase size of y-axis labels
  



