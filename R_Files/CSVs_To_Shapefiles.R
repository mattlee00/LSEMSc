
##### 1970 CSV to shapefile 










##### 1970 Cleaned CSVs -> shapefiles 




# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1970_Images/1970_Combined_Cleaned")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("1970_compiled_addresses_cleaned.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Ensure all required columns are present
  required_columns <- c("Name", "HouseNumberAndStreet", "City", "ZipCode", "state", "country", "FullAddress", "lat", "lon", "latitude", "longitude")
  for (col in required_columns) {
    if (!(col %in% colnames(geocoded_data))) {
      geocoded_data[[col]] <- NA
    }
  }
  
  # Reorder columns to ensure consistency
  geocoded_data <- geocoded_data %>%
    select(all_of(required_columns))
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1970_Images/1970_Combined_Cleaned/1970_compiled_addresses_cleaned.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")


















##### 1980 Cleaned CSVs -> shapefiles 

# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("Atlanta_compiled_addresses.csv",
               "Boston_compiled_addresses.csv",
               "DC_compiled_addresses.csv",
               "Denver_compiled_addresses.csv",
               "Houston_compiled_addresses.csv",
               "Seattle_compiled_addresses.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns
  geocoded_data <- geocoded_data %>%
    mutate(latitude = lat, longitude = lon)
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")












# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("Atlanta_compiled_addresses.csv",
               "Boston_compiled_addresses.csv",
               "DC_compiled_addresses.csv",
               "Denver_compiled_addresses.csv",
               "Houston_compiled_addresses.csv",
               "Seattle_compiled_addresses.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns
  geocoded_data <- geocoded_data %>%
    mutate(latitude = lat, longitude = lon)
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Standardize column names
  sf_data <- sf_data %>%
    select(Name, HouseNumberAndStreet, City, ZipCode, state, country, FullAddress, lat, lon, latitude, longitude)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")








# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("Atlanta_compiled_addresses.csv",
               "Boston_compiled_addresses.csv",
               "DC_compiled_addresses.csv",
               "Denver_compiled_addresses.csv",
               "Houston_compiled_addresses.csv",
               "Seattle_compiled_addresses.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")















# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("Atlanta_compiled_addresses.csv",
               "Boston_compiled_addresses.csv",
               "DC_compiled_addresses.csv",
               "Denver_compiled_addresses.csv",
               "Houston_compiled_addresses.csv",
               "Seattle_compiled_addresses.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country", "Type") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Ensure all required columns are present
  required_columns <- c("Name", "HouseNumberAndStreet", "City", "ZipCode", "state", "country", "FullAddress", "lat", "lon", "latitude", "longitude")
  for (col in required_columns) {
    if (!(col %in% colnames(geocoded_data))) {
      geocoded_data[[col]] <- NA
    }
  }
  
  # Reorder columns to ensure consistency
  geocoded_data <- geocoded_data %>%
    select(all_of(required_columns))
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images/1980_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")


















##### 1990 Cleaned CSVs -> shapefiles 




# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1990_Images/1990_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("Atlanta_compiled_addresses.csv",
               "Boston_compiled_addresses.csv",
               "DC_compiled_addresses.csv",
               "Denver_compiled_addresses.csv",
               "Houston_compiled_addresses.csv",
               "Seattle_compiled_addresses.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country", "Type") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Ensure all required columns are present
  required_columns <- c("Name", "HouseNumberAndStreet", "City", "ZipCode", "state", "country", "FullAddress", "lat", "lon", "latitude", "longitude")
  for (col in required_columns) {
    if (!(col %in% colnames(geocoded_data))) {
      geocoded_data[[col]] <- NA
    }
  }
  
  # Reorder columns to ensure consistency
  geocoded_data <- geocoded_data %>%
    select(all_of(required_columns))
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1990_Images/1990_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")


















# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1990_Images/1990_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("ATL1990_compiled_addresses_cleaned.csv",
               "BOS1990_compiled_addresses_cleaned.csv",
               "DC1990_compiled_addresses_cleaned.csv",
               "DEN1990_compiled_addresses_cleaned.csv",
               "HOU1990_compiled_addresses_cleaned.csv",
               "SEA1990_compiled_addresses_cleaned.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country", "Type") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Ensure all required columns are present
  required_columns <- c("Name", "HouseNumberAndStreet", "City", "ZipCode", "state", "country", "FullAddress", "lat", "lon", "latitude", "longitude")
  for (col in required_columns) {
    if (!(col %in% colnames(geocoded_data))) {
      geocoded_data[[col]] <- NA
    }
  }
  
  # Reorder columns to ensure consistency
  geocoded_data <- geocoded_data %>%
    select(all_of(required_columns))
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1990_Images/1990_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")



















# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2000_Images/2000_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("ATL2000_compiled_addresses_cleaned.csv",
               "BOS2000_compiled_addresses_cleaned.csv",
               "DC2000_compiled_addresses_cleaned.csv",
               "DEN2000_compiled_addresses_cleaned.csv",
               "HOU2000_compiled_addresses_cleaned.csv",
               "SEA2000_compiled_addresses_cleaned.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country", "Type") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Ensure all required columns are present
  required_columns <- c("Name", "HouseNumberAndStreet", "City", "ZipCode", "state", "country", "FullAddress", "lat", "lon", "latitude", "longitude")
  for (col in required_columns) {
    if (!(col %in% colnames(geocoded_data))) {
      geocoded_data[[col]] <- NA
    }
  }
  
  # Reorder columns to ensure consistency
  geocoded_data <- geocoded_data %>%
    select(all_of(required_columns))
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2000_Images/2000_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")












# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2010_Images/2010_Cleaned_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("ATL2010_compiled_addresses_cleaned.csv",
               "BOS2010_compiled_addresses_cleaned.csv",
               "DC2010_compiled_addresses_cleaned.csv",
               "DEN2010_compiled_addresses_cleaned.csv",
               "HOU2010_compiled_addresses_cleaned.csv",
               "SEA2010_compiled_addresses_cleaned.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country", "Type") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Ensure all required columns are present
  required_columns <- c("Name", "HouseNumberAndStreet", "City", "ZipCode", "state", "country", "FullAddress", "lat", "lon", "latitude", "longitude")
  for (col in required_columns) {
    if (!(col %in% colnames(geocoded_data))) {
      geocoded_data[[col]] <- NA
    }
  }
  
  # Reorder columns to ensure consistency
  geocoded_data <- geocoded_data %>%
    select(all_of(required_columns))
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2010_Images/2010_Cleaned_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")
















# Set the working directory to the folder containing the CSV files
setwd("/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2020_Images/Cleaned_2020_Addresses")

# Load required libraries and handle potential errors
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("readr")
install_and_load("dplyr")
install_and_load("ggmap")
install_and_load("sf")

# Register Google Maps API key
register_google(key = "AIzaSyD7OB988maN4cSn1_a0qdMWYbylt7zlllE")

# Define the CSV file names
csv_files <- c("Atlanta_compiled_addresses.csv",
                "Boston_compiled_addresses.csv",
                "DC_compiled_addresses.csv",
                "Denver_compiled_addresses.csv",
                "Houston_compiled_addresses.csv",
                "Seattle_compiled_addresses.csv")

# Initialize an empty list to store geocoded data
geocoded_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read CSV
  data <- read_csv(csv_file)
  
  # Remove rows where the first column is empty
  data_cleaned <- data %>% filter(!is.na(Name) & Name != "")
  
  # Check for correct column names
  if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country", "Type") %in% colnames(data_cleaned))) {
    stop(paste("One or more required columns are missing in the CSV file:", csv_file))
  }
  
  # Concatenate address elements
  data_cleaned <- data_cleaned %>%
    mutate(FullAddress = paste(HouseNumberAndStreet, City, ZipCode, state, country, sep = ", "))
  
  # Geocode addresses
  geocoded_data <- data_cleaned %>%
    mutate_geocode(FullAddress, output = "latlon", source = "google")
  
  # Filter out rows with missing coordinates
  geocoded_data <- geocoded_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  # Add lat and lon as separate columns if they don't exist
  if (!("latitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(latitude = lat)
  }
  if (!("longitude" %in% colnames(geocoded_data))) {
    geocoded_data <- geocoded_data %>% mutate(longitude = lon)
  }
  
  # Ensure all required columns are present
  required_columns <- c("Name", "HouseNumberAndStreet", "City", "ZipCode", "state", "country", "FullAddress", "lat", "lon", "latitude", "longitude")
  for (col in required_columns) {
    if (!(col %in% colnames(geocoded_data))) {
      geocoded_data[[col]] <- NA
    }
  }
  
  # Reorder columns to ensure consistency
  geocoded_data <- geocoded_data %>%
    select(all_of(required_columns))
  
  # Convert to sf object
  sf_data <- st_as_sf(geocoded_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Append to the list
  geocoded_list[[csv_file]] <- sf_data
}

# Combine all sf objects into a single sf object
combined_sf <- do.call(rbind, geocoded_list)

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2020_Images/Cleaned_2020_Addresses/Combined_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(combined_sf, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")
















