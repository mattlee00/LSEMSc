
######### 1990

setwd("/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Seattle")

# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)


# Path to your folder containing PNG files
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Seattle"

# Get list of all PNG files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.heic$", full.names = TRUE)

# Initialize an empty string to store all extracted texts
all_text <- ""

# Iterate over each file and process
for (file_path in file_list) {
  # Read the image
  img <- image_read(file_path)
  
  # Initialize Tesseract OCR engine
  ocr_engine <- tesseract()
  
  # Perform OCR on the image
  ocr_text <- ocr(img, engine = ocr_engine)
  
  # Append the extracted text to the combined text string
  all_text <- paste(all_text, ocr_text, sep = "\n")
}

# Write the combined text to a TXT file
writeLines(all_text, con = "SEA1990_compiled_addresses.txt")

# Print confirmation
cat("Data saved to SEA1990compiled_addresses.txt\n")

# Define the file path
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Seattle/SEA1990_compiled_addresses.txt"

# Read the TXT file
addresses <- readLines(file_path)

# Initialize vectors to store the parts of the addresses
names <- c()
house_numbers_and_streets <- c()
cities <- c()
zip_codes <- c()

# Initialize a variable to store the combined address
combined_address <- ""

# Loop through each address and parse the parts
for (i in 1:length(addresses)) {
  address <- addresses[i]
  
  # Combine with the previous address if it ends with "-"
  if (endsWith(combined_address, "-")) {
    combined_address <- paste0(combined_address, address)
  } else {
    combined_address <- address
  }
  
  # Check if the combined address still ends with "-" and continue if true
  if (endsWith(combined_address, "-")) {
    next
  }
  
  # Assuming the format is "name, house number and street, city, zip code"
  parts <- unlist(strsplit(combined_address, "[,;:] "))
  
  # Check if the parts contain only "NA" or "POB"/"PO Box" and skip if true
  if (all(parts == "NA") || any(grepl("PO Box|POB", parts, ignore.case = TRUE))) {
    combined_address <- ""
    next
  }
  
  # Assign each part to the respective vectors
  names <- c(names, parts[1])
  house_numbers_and_streets <- c(house_numbers_and_streets, parts[2])
  zip_codes <- c(zip_codes, parts[3])
  cities <- c(cities, parts[4])

  
  # Reset the combined address
  combined_address <- ""
}

# Create a data frame
address_df <- data.frame(
  Name = names,
  HouseNumberAndStreet = house_numbers_and_streets,
  City = cities,
  ZipCode = zip_codes,
  stringsAsFactors = FALSE
)

# Replace "NA" or empty values in City with "Atlanta"
address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- "Seattle"

# Add state and country columns
address_df$state <- "WA"
address_df$country <- "USA"

# Remove rows with all "NA" values
address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]

# Write the data frame to a CSV file
output_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Seattle/SEA1990_compiled_addresses.csv"
write.csv(address_df, output_path, row.names = FALSE)





######### 2000 

setwd("/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Seattle")

# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)


# Path to your folder containing PNG files
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Seattle"

# Get list of all PNG files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.png$", full.names = TRUE)

# Initialize an empty string to store all extracted texts
all_text <- ""

# Iterate over each file and process
for (file_path in file_list) {
  # Read the image
  img <- image_read(file_path)
  
  # Initialize Tesseract OCR engine
  ocr_engine <- tesseract()
  
  # Perform OCR on the image
  ocr_text <- ocr(img, engine = ocr_engine)
  
  # Append the extracted text to the combined text string
  all_text <- paste(all_text, ocr_text, sep = "\n")
}

# Write the combined text to a TXT file
writeLines(all_text, con = "SEA2000_compiled_addresses.txt")

# Print confirmation
cat("Data saved to SEA2000compiled_addresses.txt\n")

# Define the file path
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Seattle/SEA2000_compiled_addresses.txt"

# Read the TXT file
addresses <- readLines(file_path)

# Initialize vectors to store the parts of the addresses
names <- c()
house_numbers_and_streets <- c()
cities <- c()
zip_codes <- c()

# Initialize a variable to store the combined address
combined_address <- ""

# Loop through each address and parse the parts
for (i in 1:length(addresses)) {
  address <- addresses[i]
  
  # Combine with the previous address if it ends with "-"
  if (endsWith(combined_address, "-")) {
    combined_address <- paste0(combined_address, address)
  } else {
    combined_address <- address
  }
  
  # Check if the combined address still ends with "-" and continue if true
  if (endsWith(combined_address, "-")) {
    next
  }
  
  # Assuming the format is "name, house number and street, city, zip code"
  parts <- unlist(strsplit(combined_address, "[,;:] "))
  
  # Check if the parts contain only "NA" or "POB"/"PO Box" and skip if true
  if (all(parts == "NA") || any(grepl("PO Box|POB", parts, ignore.case = TRUE))) {
    combined_address <- ""
    next
  }
  
  # Assign each part to the respective vectors
  names <- c(names, parts[1])
  house_numbers_and_streets <- c(house_numbers_and_streets, parts[2])
  zip_codes <- c(zip_codes, parts[3])
  cities <- c(cities, parts[4])
  
  # Reset the combined address
  combined_address <- ""
}

# Create a data frame
address_df <- data.frame(
  Name = names,
  HouseNumberAndStreet = house_numbers_and_streets,
  City = cities,
  ZipCode = zip_codes,
  stringsAsFactors = FALSE
)

# Replace "NA" or empty values in City with "Atlanta"
address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- "Seattle"

# Add state and country columns
address_df$state <- "WA"
address_df$country <- "USA"

# Remove rows with all "NA" values
address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]

# Write the data frame to a CSV file
output_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Seattle/SEA2000_compiled_addresses.csv"
write.csv(address_df, output_path, row.names = FALSE)





######### 2010

setwd("/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Seattle")

# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)


# Path to your folder containing PNG files
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Seattle"

# Get list of all PNG files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.png$", full.names = TRUE)

# Initialize an empty string to store all extracted texts
all_text <- ""

# Iterate over each file and process
for (file_path in file_list) {
  # Read the image
  img <- image_read(file_path)
  
  # Initialize Tesseract OCR engine
  ocr_engine <- tesseract()
  
  # Perform OCR on the image
  ocr_text <- ocr(img, engine = ocr_engine)
  
  # Append the extracted text to the combined text string
  all_text <- paste(all_text, ocr_text, sep = "\n")
}

# Write the combined text to a TXT file
writeLines(all_text, con = "SEA2010_compiled_addresses.txt")

# Print confirmation
cat("Data saved to SEA2010compiled_addresses.txt\n")

# Define the file path
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Seattle/SEA2010_compiled_addresses.txt"

# Read the TXT file
addresses <- readLines(file_path)

# Initialize vectors to store the parts of the addresses
names <- c()
house_numbers_and_streets <- c()
cities <- c()
zip_codes <- c()

# Initialize a variable to store the combined address
combined_address <- ""

# Loop through each address and parse the parts
for (i in 1:length(addresses)) {
  address <- addresses[i]
  
  # Combine with the previous address if it ends with "-"
  if (endsWith(combined_address, "-")) {
    combined_address <- paste0(combined_address, address)
  } else {
    combined_address <- address
  }
  
  # Check if the combined address still ends with "-" and continue if true
  if (endsWith(combined_address, "-")) {
    next
  }
  
  # Assuming the format is "name, house number and street, city, zip code"
  parts <- unlist(strsplit(combined_address, "[,;:] "))
  
  # Check if the parts contain only "NA" or "POB"/"PO Box" and skip if true
  if (all(parts == "NA") || any(grepl("PO Box|POB", parts, ignore.case = TRUE))) {
    combined_address <- ""
    next
  }
  
  # Assign each part to the respective vectors
  names <- c(names, parts[1])
  house_numbers_and_streets <- c(house_numbers_and_streets, parts[2])
  zip_codes <- c(zip_codes, parts[3])
  cities <- c(cities, parts[4])
  
  # Reset the combined address
  combined_address <- ""
}

# Create a data frame
address_df <- data.frame(
  Name = names,
  HouseNumberAndStreet = house_numbers_and_streets,
  City = cities,
  ZipCode = zip_codes,
  stringsAsFactors = FALSE
)

# Replace "NA" or empty values in City with "Atlanta"
address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- "Seattle"

# Add state and country columns
address_df$state <- "WA"
address_df$country <- "USA"

# Remove rows with all "NA" values
address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]

# Write the data frame to a CSV file
output_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Seattle/SEA2010_compiled_addresses.csv"
write.csv(address_df, output_path, row.names = FALSE)















































########## geocoding 

setwd("/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Seattle")

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

# Define file path and read CSV
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Seattle/SEA1990_compiled_addresses_cleaned.csv"
data <- read_csv(file_path)

# Inspect column names to ensure they match
print(colnames(data))

# Remove rows where the first column is empty
data_cleaned <- data %>% filter(!is.na(Name) & Name != "")

# Check for correct column names
if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country") %in% colnames(data_cleaned))) {
  stop("One or more required columns are missing in the CSV file.")
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

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Seattle/SEA_1990_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(sf_data, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")






setwd("/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Seattle")

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

# Define file path and read CSV
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Seattle/SEA2000_compiled_addresses_cleaned.csv"
data <- read_csv(file_path)

# Inspect column names to ensure they match
print(colnames(data))

# Remove rows where the first column is empty
data_cleaned <- data %>% filter(!is.na(Name) & Name != "")

# Check for correct column names
if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country") %in% colnames(data_cleaned))) {
  stop("One or more required columns are missing in the CSV file.")
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

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Seattle/SEA_2000_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(sf_data, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")









setwd("/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Seattle")

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

# Define file path and read CSV
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Seattle/SEA2010_compiled_addresses_cleaned.csv"
data <- read_csv(file_path)

# Inspect column names to ensure they match
print(colnames(data))

# Remove rows where the first column is empty
data_cleaned <- data %>% filter(!is.na(Name) & Name != "")

# Check for correct column names
if (!all(c("HouseNumberAndStreet", "City", "ZipCode", "state", "country") %in% colnames(data_cleaned))) {
  stop("One or more required columns are missing in the CSV file.")
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

# Define the shapefile path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Seattle/SEA_2010_Geocodes.shp"

# Write to shapefile, overwriting if it exists
st_write(sf_data, shapefile_path, delete_layer = TRUE)

cat("Shapefile has been created at:", shapefile_path, "\n")







