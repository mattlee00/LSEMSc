######### 1990

setwd("/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Houston")

# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)


# Path to your folder containing PNG files
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Houston"

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
writeLines(all_text, con = "HOU1990_compiled_addresses.txt")

# Print confirmation
cat("Data saved to HOU1990compiled_addresses.txt\n")

# Define the file path
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Houston/HOU1990_compiled_addresses.txt"

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
  cities <- c(cities, parts[3])
  zip_codes <- c(zip_codes, parts[4])
  
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
address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- "Houston"

# Add state and country columns
address_df$state <- "TX"
address_df$country <- "USA"

# Remove rows with all "NA" values
address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]

# Write the data frame to a CSV file
output_path <- "/Users/matthewlee/Desktop/RUPS_Dis/1990_Images/Houston/HOU1990_compiled_addresses.csv"
write.csv(address_df, output_path, row.names = FALSE)







######### 2000

setwd("/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Houston")

# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)


# Path to your folder containing PNG files
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Houston"

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
writeLines(all_text, con = "HOU2000_compiled_addresses.txt")

# Print confirmation
cat("Data saved to HOU2000compiled_addresses.txt\n")

# Define the file path
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Houston/HOU2000_compiled_addresses.txt"

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
  cities <- c(cities, parts[3])
  zip_codes <- c(zip_codes, parts[4])
  
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
address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- "Houston"

# Add state and country columns
address_df$state <- "TX"
address_df$country <- "USA"

# Remove rows with all "NA" values
address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]

# Write the data frame to a CSV file
output_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2000_Images/Houston/HOU2000_compiled_addresses.csv"
write.csv(address_df, output_path, row.names = FALSE)










######### 2010

setwd("/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Houston")

# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)


# Path to your folder containing PNG files
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Houston"

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
writeLines(all_text, con = "HOU2010_compiled_addresses.txt")

# Print confirmation
cat("Data saved to HOU2010compiled_addresses.txt\n")

# Define the file path
file_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Houston/HOU2010_compiled_addresses.txt"

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
  cities <- c(cities, parts[3])
  zip_codes <- c(zip_codes, parts[4])
  
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
address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- "Houston"

# Add state and country columns
address_df$state <- "TX"
address_df$country <- "USA"

# Remove rows with all "NA" values
address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]

# Write the data frame to a CSV file
output_path <- "/Users/matthewlee/Desktop/RUPS_Dis/2010_Images/Houston/HOU2010_compiled_addresses.csv"
write.csv(address_df, output_path, row.names = FALSE)




