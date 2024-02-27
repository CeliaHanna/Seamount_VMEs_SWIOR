# Load necessary libraries

library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(geosphere)
library(hms)

#### ADDING CAPTURE TIME TO EACH ANNOTATION ON BIIGLE OUTPUT REPORTS #####

# Define the directory where your CSV files are located
directory_path <- "/Users/user/Desktop/coral_biigle_feb"

# Function to clean 'frames' column, add 'capture time' column with only time part, sort by 'capture time', and optionally return the dataframe
add_timestamp_to_csv_and_sort <- function(file_name, start_time, return_df = FALSE) {
  # Construct the full file path
  file_path <- file.path(directory_path, file_name)
  
  # Read the CSV file
  data <- read_csv(file_path)
  
  # Clean the 'frames' column by removing square brackets and converting to numeric
  data$frames <- as.numeric(gsub("\\[|\\]", "", data$frames))
  
  # Convert start_time to POSIXct
  start_time_posix <- as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Add 'capture time' column with only time part and sort by 'capture time'
  data <- data %>%
    mutate(capture_time = format(start_time_posix + seconds(frames), "%H:%M:%S")) %>%
    arrange(capture_time)
  
  # Save the modified DataFrame back to CSV or return it
  if (return_df) {
    return(data)
  } else {
    write_csv(data, file_path)
  }
}

# Files and their corresponding start times
files_and_start_times <- list(
  list("coral-dive1-file4.csv", "2011-11-12 12:00:00"),
  list("coral-dive1-file5.csv", "2011-11-12 14:01:00"),
  list("coral-dive2-file2.csv", "2011-11-13 07:31:00"),
  list("coral-dive2-file4.csv", "2011-11-13 11:34:00"),
  list("coral-dive2-file5.csv", "2011-11-13 13:49:00"),
  list("coral-dive3-file4.csv", "2011-11-14 11:00:00"),
  list("coral-dive3-file5.csv", "2011-11-14 13:13:00"),
  list("coral-dive4-file2.csv", "2011-11-16 05:14:00"),
  list("coral-dive5-file1.csv", "2011-11-20 03:19:00"),
  list("coral-dive5-file2.csv", "2011-11-20 05:22:00")
)

# Loop through each file name and its corresponding start time
for (file_start_time in files_and_start_times) {
  add_timestamp_to_csv_and_sort(file_start_time[[1]], file_start_time[[2]])
}


##### ADDING LATITUDE AND LONGITUDE VALUES TO EACH BIIGLE ANNOTATION BASED ON CAPTURE TIMES #####

## This script takes the capture_time column and matches it to UTC_Time column in metadata files
## so that the nearest lat lon for each capture time is added to dive files 

# Directories
data_dir <- "/Users/user/Desktop/coral_biigle_feb/1.coral_biigle_w/capture_times"
metadata_dir <- "/Users/user/Desktop/metadata_flow/original_CORAL"

# List files in each directory
data_files <- list.files(data_dir, full.names = TRUE)
metadata_files <- list.files(metadata_dir, full.names = TRUE)

# Process each data file
for (data_file_path in data_files) {
  # Extract dive number from data file name
  dive_number <- str_extract(basename(data_file_path), "dive[1-5]")
  
  # Find the corresponding metadata file
  metadata_file_path <- grep(dive_number, metadata_files, value = TRUE)
  
  if (length(metadata_file_path) == 1) { # Ensure there is exactly one match
    # Read the data and metadata files
    data <- read_csv(data_file_path)
    metadata <- read_csv(metadata_file_path)
    
    # Convert 'capture_time' to seconds since the start of the day
    data$time_seconds <- as.numeric(hms::as.hms(data$capture_time))
    metadata$time_seconds <- as.numeric(hms::as.hms(metadata$capture_time))
    
    # Find the closest 'capture_time' and merge 'lat' and 'lon'
    closest_indices <- sapply(data$time_seconds, find_closest_index, metadata = metadata)
    closest_indices <- as.numeric(closest_indices)
    data$lat <- metadata$lat[closest_indices]
    data$lon <- metadata$lon[closest_indices]
    
    # Remove the temporary 'time_seconds' column
    data <- dplyr::select(data, -time_seconds)
    
    # Save the updated data to a new file
    new_file_path <- gsub(".csv", "_with_lat_lon.csv", data_file_path)
    write_csv(data, new_file_path)
  } else {
    message(paste("No unique metadata file found for", dive_number))
  }
}

## Now latitudes and longitudes have been added to the biigle files.
## Dive routes double checked on QGIS

########## ADDING TEMPERATURE, PRESSURE, SALINITY AND DEPTH VALUES FOR EACH ANNOTATION ##########

# Environmental variables TPSD will now be added from cruise metadata files
# Get data from MERGED TELEMETRIES 
# Using Timestamp column and capture_time 
# These two columns must be parsed to a common format 
## 'CTD.Temperature', 'CTD.Salinity', 'CTD.Pressure', 'ROV.RovDepth'

# Function to merge environmental data based on closest capture times
merge_environmental_data <- function(data_filepath, env_data_filepath) {
  data <- read_csv(data_filepath, show_col_types = FALSE)
  env_data <- read_csv(env_data_filepath, show_col_types = FALSE)
  
  # Correctly convert 'Timestamp' to datetime object
  env_data <- env_data %>%
    mutate(Timestamp = dmy_hms(Timestamp),
           time_only = format(Timestamp, "%H:%M:%S"))
  
  # Convert capture_time to HMS object for compatibility
  data <- data %>%
    mutate(capture_time = as.character(capture_time),
           capture_time = strptime(capture_time, format = "%H:%M:%S"),
           capture_time_hms = format(capture_time, "%H:%M:%S"))
  
  # Find closest matching times
  closest_indices <- sapply(data$capture_time_hms, function(ct) {
    time_differences <- abs(as.numeric(difftime(strptime(env_data$time_only, format="%H:%M:%S"), strptime(ct, format="%H:%M:%S"), units="secs")))
    which.min(time_differences)
  })
  
  # Convert closest_indices to a numeric vector if it's not already
  closest_indices <- as.numeric(closest_indices)
  
  # Adding environmental data to the main data file
  data$CTD.Temperature <- env_data$CTD.Temperature[closest_indices]
  data$CTD.Salinity <- env_data$CTD.Salinity[closest_indices]
  data$CTD.Pressure <- env_data$CTD.Pressure[closest_indices]
  data$ROV.RovDepth <- env_data$ROV.RovDepth[closest_indices]
  
  # Optionally: Remove temporary columns or adjust as needed
  data <- data %>%
    dplyr::select(-capture_time_hms)
  
  # Save the updated dataset
  write_csv(data, gsub(".csv", "_with_env_data.csv", data_filepath))
}


### running it with multiple csvs 

# Define the directories where your files are located

data_directory <- "/Users/user/Desktop/coral_biigle_feb/2.coral_biigle_latlon"
metadata_directory <- "/Users/user/Desktop/metadata_flow/3.MERGEDTELEMETRIES" 

# List all CSV files in the data directory
data_files <- list.files(data_directory, pattern = "\\.csv$", full.names = TRUE)

# Function to find the matching metadata file based on dive number
find_matching_metadata_file <- function(data_file, metadata_directory) {
  dive_number <- str_extract(basename(data_file), "dive[1-5]")
  metadata_files <- list.files(metadata_directory, pattern = paste0(dive_number, ".*\\.csv$"), full.names = TRUE)
  if (length(metadata_files) == 1) {
    return(metadata_files)
  } else {
    warning(paste("No unique metadata file found for", dive_number, "in", data_file))
    return(NULL)
  }
}

# Iterate over each data file, find the matching metadata file, and process
for (data_file_path in data_files) {
  metadata_file_path <- find_matching_metadata_file(data_file_path, metadata_directory)
  
  if (!is.null(metadata_file_path)) {
    message(paste("Processing", basename(data_file_path), "with", basename(metadata_file_path)))
    merge_environmental_data(data_file_path, metadata_file_path)
  }
}

message("All files processed.")


####### ADDING SLOPE VALUES (DEGREES) TO EACH ANNOTATION #########

# Gradient values have been extracted from Tiff files for all dive routes 

add_slope_values <- function(data_filepath, slope_data_filepath, output_dir) {
  data <- read_csv(data_filepath)
  slope_data <- read_csv(slope_data_filepath)
  
  # Ensure the column names here match your actual data
  data_coords <- data %>% dplyr::select(lat, lon)
  slope_coords <- slope_data %>% dplyr::select(lat, lon, Slope)  # Ensure 'Slope' matches your column name
  
  # Compute nearest indices, ignoring NAs
  nearest_indices <- apply(data_coords, 1, function(data_row) {
    if(all(is.na(data_row))) {
      return(NA)
    } else {
      distances <- distHaversine(matrix(c(data_row['lon'], data_row['lat']), ncol = 2),
                                 slope_coords %>% dplyr::select(lon, lat) %>% as.matrix())
      which.min(distances)
    }
  })
  
  # Directly assign slope values where matches are found
  data$slope <- NA_real_  # Initialize with NAs
  valid_indices <- !is.na(nearest_indices)  # Identify non-NA indices
  if(any(valid_indices)) {  # Check if there are any valid matches
    # Assign matched slopes to valid indices
    data$slope[valid_indices] <- slope_data$Slope[nearest_indices[valid_indices]]
  }
  
  # Construct the output file path
  output_filename <- gsub("^.*/", "", data_filepath)  # Extracts the filename without the path
  output_filepath <- file.path(output_dir, gsub(".csv", "_with_slope.csv", output_filename))
  
  write_csv(data, output_filepath)
}


# Paths to your directories
data_dir <- "/Users/user/Desktop/coral_biigle_feb/3.coral_biigle_TSPD" # Update this path
metadata_dir <- "/Users/user/Desktop/metadata_flow/gradient_info" # Update this path
output_dir <- "/Users/user/Desktop/coral_biigle_feb/4.coral_biigle_slope" # Output directory


# List CSV files in both directories
data_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
metadata_files <- list.files(metadata_dir, pattern = "\\.csv$", full.names = TRUE)

# Extract dive numbers from file names
get_dive_number <- function(filename) {
  str_extract(filename, "dive\\d+")
}

# Pair data files with corresponding metadata files based on dive number
for (data_file in data_files) {
  dive_number <- get_dive_number(data_file)
  metadata_file <- metadata_files[sapply(metadata_files, get_dive_number) == dive_number]
  
  if (length(metadata_file) == 1) {
    add_slope_values(data_file, metadata_file, output_dir)
    cat("Processed file:", data_file, "with metadata:", metadata_file, "\n")
  } else {
    cat("No matching metadata file found for", data_file, "\n")
  }
}


###### ADDING COLUMN FOR VIDEO DURATION ######

# Custom function to format seconds into H:M:S
format_video_duration <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- round(seconds %% 60)
  sprintf("%02d:%02d:%02d", hours, minutes, secs)
}


# Update these paths to reflect your actual directories
data_dir <- "/Users/user/Desktop/coral_biigle_feb/4.coral_biigle_slope"
output_dir <- "/Users/user/Desktop/coral_biigle_feb/5.coral_biigle_duration"


# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# List CSV files in the data directory
data_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

# Function to add video_duration to each CSV file
add_video_duration <- function(data_filepath, output_dir) {
  data <- read_csv(data_filepath)
  
  # Convert 'frames' to 'video_duration' in H:M:S format
  data$video_duration <- format_video_duration(data$frames)
  
  # Construct the output file path
  output_filename <- gsub("^.*/", "", data_filepath)
  output_filepath <- file.path(output_dir, gsub(".csv", "_with_video_duration.csv", output_filename))
  
  write_csv(data, output_filepath)
}

# Apply the function to each CSV file in the directory
for (data_file in data_files) {
  add_video_duration(data_file, output_dir)
}





