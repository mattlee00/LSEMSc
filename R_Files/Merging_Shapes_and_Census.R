#### counting GISJOIN

# Load required libraries
library(sf)

# Set the directory containing the shapefiles
shapefile_dir <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/Final_Counts_Shapefiles"

# List all shapefiles in the directory
shapefiles <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE)

# Initialize a variable to store the maximum length of GISJOIN
max_gisjoin_length <- 0
longest_gisjoin <- NULL
file_with_longest_gisjoin <- NULL

# Loop through each shapefile and find the longest GISJOIN
for (shapefile in shapefiles) {
  # Read the shapefile
  sf_data <- st_read(shapefile, quiet = TRUE)
  
  # Check if the GISJOIN column exists
  if ("GISJOIN" %in% names(sf_data)) {
    # Find the maximum length of GISJOIN in this shapefile
    current_max_length <- max(nchar(as.character(sf_data$GISJOIN)), na.rm = TRUE)
    
    # Update the overall maximum length if the current one is greater
    if (current_max_length > max_gisjoin_length) {
      max_gisjoin_length <- current_max_length
      longest_gisjoin <- sf_data$GISJOIN[which.max(nchar(as.character(sf_data$GISJOIN)))]
      file_with_longest_gisjoin <- shapefile
    }
  }
}

# Print the results
cat("The longest GISJOIN input is:", longest_gisjoin, "\n")
cat("Number of characters:", max_gisjoin_length, "\n")
cat("Found in file:", file_with_longest_gisjoin, "\n")







#### internal long and lat for 1970 
library(sf)

# Define the file paths
file_paths <- list("/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/Shapes_and_Addresses/1970_Shapes_and_Addresses/filtered_US_tract_1970_with_counts.shp")

# Function to add latitude and longitude

add_lat_long <- function(file_path) {
  # Read the shapefile
  shp <- st_read(file_path)
  
  # Calculate the centroid for each polygon
  shp$centroid <- st_centroid(shp$geometry)
  
  # Extract latitude and longitude
  shp$latitude <- st_coordinates(shp$centroid)[,2]
  shp$longitude <- st_coordinates(shp$centroid)[,1]
  
  # Remove the centroid column
  shp <- st_drop_geometry(shp)
  
  # \Write the updated shapefile
  st_write(shp, file_path, delete_layer = TRUE)
}

# Apply the function to each file
lapply(file_paths, add_lat_long)








#### shapefiles to DTAs 1980-2020

# Load necessary libraries
library(sf)
library(haven)
library(dplyr)

# Set the folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/Final_Counts_Shapefiles"

# List all shapefiles in the folder
shapefiles <- list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE)

# Function to convert shapefile to DTA
convert_shapefile_to_dta <- function(shapefile) {
  # Read the shapefile
  shp <- st_read(shapefile)
  
  # Convert to a data frame and remove the geometry column
  shp_df <- st_drop_geometry(shp)
  
  # Get the base name of the shapefile (without extension)
  base_name <- tools::file_path_sans_ext(basename(shapefile))
  
  # Create the output file path
  output_file <- file.path(folder_path, paste0(base_name, ".dta"))
  
  # Write the data frame to a DTA file
  write_dta(shp_df, output_file)
}

# Apply the function to all shapefiles in the folder
lapply(shapefiles, convert_shapefile_to_dta)

# Print completion message
cat("Conversion completed. DTA files have been saved in the same folder.\n")











#### merging, formatting DTAs 1980-2020



install.packages("haven")
install.packages("dplyr")
library(haven)  # For reading DTA files
library(dplyr)  # For data manipulation

# Set folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/Final_Counts_Shapefiles"

# List all DTA files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.dta$", full.names = TRUE)

# Function to process each file
process_file <- function(file_path) {
  # Read the DTA file
  df <- read_dta(file_path)
  
  # Extract year from the file name
  file_name <- basename(file_path)
  year <- gsub(".*_(\\d{4})_with_counts\\.dta", "\\1", file_name)
  
  # Ensure GISJOIN is 14 characters long by padding with zeros at the end
  df <- df %>%
    mutate(GISJOIN = ifelse(nchar(as.character(GISJOIN)) < 14,
                            paste0(as.character(GISJOIN), strrep("0", 14 - nchar(as.character(GISJOIN)))),
                            as.character(GISJOIN))) %>%
    mutate(Year = as.character(year)) %>%  # Convert Year to string
    mutate(GISJoinYear = paste0(GISJOIN, Year))  # Create GISJoinYear variable
  
  return(df)
}

# Read and process all files
data_list <- lapply(file_list, process_file)

# Combine all data frames
combined_df <- bind_rows(data_list)

# Save the combined data frame to a new DTA file
write_dta(combined_df, file.path(folder_path, "combined_data.dta"))

# Inform user
cat("All DTA files have been processed and combined into 'combined_data.dta'.")







#### shapefiles to DTAs 1970

# Load necessary libraries
library(sf)
library(haven)
library(dplyr)

# Set the folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/Shapes_and_Addresses/1970_Shapes_and_Addresses/1970_with_counts"

# List all shapefiles in the folder
shapefiles <- list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE)

# Function to convert shapefile to DTA
convert_shapefile_to_dta <- function(shapefile) {
  # Read the shapefile
  shp <- st_read(shapefile)
  
  # Convert to a data frame and remove the geometry column
  shp_df <- st_drop_geometry(shp)
  
  # Get the base name of the shapefile (without extension)
  base_name <- tools::file_path_sans_ext(basename(shapefile))
  
  # Create the output file path
  output_file <- file.path(folder_path, paste0(base_name, ".dta"))
  
  # Write the data frame to a DTA file
  write_dta(shp_df, output_file)
}

# Apply the function to all shapefiles in the folder
lapply(shapefiles, convert_shapefile_to_dta)

# Print completion message
cat("Conversion completed. DTA files have been saved in the same folder.\n")















#### merging, formatting DTAs 1970



install.packages("haven")
install.packages("dplyr")
library(haven)  # For reading DTA files
library(dplyr)  # For data manipulation

# Set folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/Shapes_and_Addresses/1970_Shapes_and_Addresses/1970_with_counts"

# List all DTA files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.dta$", full.names = TRUE)

# Function to process each file
process_file <- function(file_path) {
  # Read the DTA file
  df <- read_dta(file_path)
  
  # Extract year from the file name
  file_name <- basename(file_path)
  year <- gsub(".*_(\\d{4})_with_counts\\.dta", "\\1", file_name)
  
  # Ensure GISJOIN is 14 characters long by padding with zeros at the end
  df <- df %>%
    mutate(GISJOIN = ifelse(nchar(as.character(GISJOIN)) < 14,
                            paste0(as.character(GISJOIN), strrep("0", 14 - nchar(as.character(GISJOIN)))),
                            as.character(GISJOIN))) %>%
    mutate(Year = as.character(year)) %>%  # Convert Year to string
    mutate(GISJoinYear = paste0(GISJOIN, Year))  # Create GISJoinYear variable
  
  return(df)
}

# Read and process all files
data_list <- lapply(file_list, process_file)

# Combine all data frames
combined_df <- bind_rows(data_list)

# Save the combined data frame to a new DTA file
write_dta(combined_df, file.path(folder_path, "1970_count_data.dta"))

# Inform user
cat("All DTA files have been processed and combined into '1970_count_data.dta'.")

















###### merging yearly census data variables 

# Load necessary libraries
library(dplyr)
library(haven)

# Define the folder path containing the DTA files
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Merged_Census_Data/Census_Ready_To_Merge"

# Get a list of all DTA files in the folder
dta_files <- list.files(folder_path, pattern = "\\.dta$", full.names = TRUE)

# Initialize an empty list to store the data frames
data_list <- list()

# Loop through each file and read it into a data frame
for (file in dta_files) {
  # Read the DTA file
  data <- read_dta(file)
  
  # Append the data frame to the list
  data_list <- append(data_list, list(data))
}

# Combine all data frames into one
combined_data <- bind_rows(data_list)

# Write the combined data to a new DTA file
write_dta(combined_data, file.path(folder_path, "combined_data.dta"))

# Optionally, write the combined data to a CSV file
write.csv(combined_data, file.path(folder_path, "combined_data.csv"), row.names = FALSE)

