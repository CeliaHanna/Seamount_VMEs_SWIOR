library(raster)
library(sp)
library(readr)
library(dplyr)

# This code reprojects the productivity data to 50m resolution
# Then adds it to dive route csvs 

# Read the TIFF file
ocean_prod <- raster("/Users/user/Desktop/Mean_productivity.tif")

desired_res_deg <- 0.00045  # 50 meters in degrees
template_raster <- raster(extent(ocean_prod), res=desired_res_deg)
projection(template_raster) <- projection(ocean_prod)

# Resample using bilinear interpolation
high_res_raster <- resample(ocean_prod, template_raster, method='bilinear')

# Save the resampled productivity raster
writeRaster(high_res_raster, "/Users/user/Desktop/high_res_raster.tif", format="GTiff", overwrite=TRUE)

high_res_raster <- raster("/Users/user/Desktop/high_res_raster.tif") 

# Define the main and output folder paths
main_folder_path <- "/Users/user/Desktop/coral_biigle_feb/5.coral_biigle_duration"
output_folder_path <- "/Users/user/Desktop/coral_biigle_feb/6.coral_biigle_productivity"

# Function to add productivity data to a CSV file
add_productivity_data <- function(csv_file, raster_file, output_folder) {
  print(paste("Processing file:", csv_file))
  data <- read_csv(csv_file)
  
  # Check for NA values before conversion
  if (any(is.na(data$lon)) || any(is.na(data$lat))) {
    print("Warning: NA values found in longitude or latitude")
    return()
  }
  
  # Ensure lat and long are numeric
  data$lon <- as.numeric(data$lon)
  data$lat <- as.numeric(data$lat)
  
  coordinates <- data.frame(lon = data$lon, lat = data$lat)
  coordinates_sp <- SpatialPoints(coordinates, proj4string = crs(raster_file))
  
  # Extract productivity values
  values <- raster::extract(raster_file, coordinates_sp)
  data$productivity_mean <- values
  
  # Save the updated CSV file
  output_file_path <- file.path(output_folder, basename(csv_file))
  write_csv(data, output_file_path)
  print(paste("File written:", output_file_path))
}

# List and process all CSV files
csv_files <- list.files(main_folder_path, pattern = "\\.csv$", full.names = TRUE)

for (csv_file in csv_files) {
  add_productivity_data(csv_file, high_res_raster, output_folder_path)
}





