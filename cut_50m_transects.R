process_file_transects <- function(filename, output_directory) {
  # Read the CSV file
  data <- read.csv(filename)
  
  # Initialize variables
  last_transect_start <- 0
  transect_counts <- 0
  transect_starts <- c(0) # Keep track of where each transect starts
  
  # Ensure data is sorted by cumulative_distance
  data <- data[order(data$cumulative_distance),]
  
  # Calculate transects
  for (i in 1:nrow(data)) {
    if (data$cumulative_distance[i] - last_transect_start >= 50) {
      transect_counts <- transect_counts + 1
      last_transect_start <- data$cumulative_distance[i]
      transect_starts <- c(transect_starts, last_transect_start)
    }
  }
  
  # Handle each transect
  for (transect_index in 1:length(transect_starts)) {
    if (transect_index < length(transect_starts)) {
      transect_data <- data[data$cumulative_distance >= transect_starts[transect_index] & data$cumulative_distance < transect_starts[transect_index + 1], ]
    } else {
      # For the last transect, include all remaining data
      transect_data <- data[data$cumulative_distance >= transect_starts[transect_index], ]
    }
    
    if (nrow(transect_data) > 0) {
      transect_name <- paste0("transect", transect_index, ".csv")
      write.csv(transect_data, file = file.path(output_directory, transect_name), row.names = FALSE)
    }
  }
  
  # Return the total count of complete transects
  return(transect_counts)
}

# Example usage
filename <- "/Users/user/Desktop/coral_biigle_feb/7.coral_biigle_interpolated/coral-dive1-file4_prod_interpolated.csv" # Replace with your actual file path
output_directory <- "/Users/user/Desktop/coral_biigle_feb/8.coral_biigle_50m_transects"

 # Replace with your desired output directory
transect_count <- process_file_transects(filename, output_directory)

print(paste("Total complete transects:", transect_count))

``
