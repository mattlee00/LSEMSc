
##### 1970 images -> CSVs


# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)

# Base path to the folder containing city folders
base_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1970_Images"

# List of city folder names
city_names <- c("Boston", "Atlanta", "DC", "Denver", "Houston", "Seattle")

# Function to process and extract text from images
process_images <- function(city_folder, city_name) {
  # Get list of all HEIC files in the city folder
  file_list <- list.files(path = city_folder, pattern = "(?i)\\.HEIC$", full.names = TRUE)
  
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
  txt_file_path <- file.path(city_folder, paste0(city_name, "_compiled_addresses.txt"))
  writeLines(all_text, con = txt_file_path)
  
  # Print confirmation
  cat(paste("Data saved to", txt_file_path, "\n"))
  
  return(txt_file_path)
}

# Function to parse addresses and create a CSV
parse_addresses <- function(txt_file_path, city_name) {
  # Read the TXT file
  addresses <- readLines(txt_file_path)
  
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
    parts <- unlist(strsplit(combined_address, "[.,;:] "))
    
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
  
  # Replace "NA" or empty values in City with the current city name
  address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- city_name
  
  if (nrow(address_df) > 0) {
    # Add state and country columns (assuming all addresses are in the same state and country)
    address_df$state <- "MA"  # Adjust state as needed
    address_df$country <- "USA"
  }
  
  # Remove rows with all "NA" values
  address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]
  
  # Write the data frame to a CSV file
  csv_file_path <- file.path(dirname(txt_file_path), paste0(city_name, "_compiled_addresses.csv"))
  write.csv(address_df, csv_file_path, row.names = FALSE)
  
  # Print confirmation
  cat(paste("Data saved to", csv_file_path, "\n"))
}

# Iterate over each city folder and process images and addresses
for (city_name in city_names) {
  city_folder <- file.path(base_path, city_name)
  if (dir.exists(city_folder)) {
    txt_file_path <- process_images(city_folder, city_name)
    parse_addresses(txt_file_path, city_name)
  } else {
    cat(paste("Folder does not exist:", city_folder, "\n"))
  }
}









#### 1970 images to csvs attempt 2

# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)

# Base path to the folder containing city folders
base_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1970_Images"

# List of city folder names
city_names <- c("Boston", "Atlanta", "DC", "Denver", "Houston", "Seattle")

# Function to process and extract text from images
process_images <- function(city_folder, city_name) {
  # Get list of all HEIC files in the city folder
  file_list <- list.files(path = city_folder, pattern = "(?i)\\.HEIC$", full.names = TRUE)
  
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
  txt_file_path <- file.path(city_folder, paste0(city_name, "_compiled_addresses.txt"))
  writeLines(all_text, con = txt_file_path)
  
  # Print confirmation
  cat(paste("Data saved to", txt_file_path, "\n"))
  
  return(txt_file_path)
}

# Function to parse addresses and return a data frame
parse_addresses <- function(txt_file_path, city_name) {
  # Read the TXT file
  addresses <- readLines(txt_file_path)
  
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
    parts <- unlist(strsplit(combined_address, "[.,;:] "))
    
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
  
  # Replace "NA" or empty values in City with the current city name
  address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- city_name
  
  if (nrow(address_df) > 0) {
    # Add state and country columns (assuming all addresses are in the same state and country)
    address_df$state <- "MA"  # Adjust state as needed
    address_df$country <- "USA"
  }
  
  # Remove rows with all "NA" values
  address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]
  
  return(address_df)
}

# Initialize an empty data frame to store all addresses
all_addresses_df <- data.frame()

# Iterate over each city folder and process images and addresses
for (city_name in city_names) {
  city_folder <- file.path(base_path, city_name)
  if (dir.exists(city_folder)) {
    txt_file_path <- process_images(city_folder, city_name)
    address_df <- parse_addresses(txt_file_path, city_name)
    
    # Append the city's data frame to the combined data frame
    all_addresses_df <- bind_rows(all_addresses_df, address_df)
  } else {
    cat(paste("Folder does not exist:", city_folder, "\n"))
  }
}

# Write the combined data frame to a single CSV file
combined_csv_file_path <- file.path(base_path, "combined_addresses.csv")
write.csv(all_addresses_df, combined_csv_file_path, row.names = FALSE)

# Print confirmation
cat(paste("All data saved to", combined_csv_file_path, "\n"))



















##### 1980 images -> CSVs


# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)

# Base path to the folder containing city folders
base_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/1980_Images"

# List of city folder names
city_names <- c("Boston", "Atlanta", "Chicago", "DC", "Denver", "Detroit", "Houston", "NewOrleans", "Philly", "SanDiego", "Seattle")

# Function to process and extract text from images
process_images <- function(city_folder, city_name) {
  # Get list of all HEIC files in the city folder
  file_list <- list.files(path = city_folder, pattern = "(?i)\\.HEIC$", full.names = TRUE)
  
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
  txt_file_path <- file.path(city_folder, paste0(city_name, "_compiled_addresses.txt"))
  writeLines(all_text, con = txt_file_path)
  
  # Print confirmation
  cat(paste("Data saved to", txt_file_path, "\n"))
  
  return(txt_file_path)
}

# Function to parse addresses and create a CSV
parse_addresses <- function(txt_file_path, city_name) {
  # Read the TXT file
  addresses <- readLines(txt_file_path)
  
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
    parts <- unlist(strsplit(combined_address, "[.,;:] "))
    
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
  
  # Replace "NA" or empty values in City with the current city name
  address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- city_name
  
  if (nrow(address_df) > 0) {
    # Add state and country columns (assuming all addresses are in the same state and country)
    address_df$state <- "MA"  # Adjust state as needed
    address_df$country <- "USA"
  }
  
  # Remove rows with all "NA" values
  address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]
  
  # Write the data frame to a CSV file
  csv_file_path <- file.path(dirname(txt_file_path), paste0(city_name, "_compiled_addresses.csv"))
  write.csv(address_df, csv_file_path, row.names = FALSE)
  
  # Print confirmation
  cat(paste("Data saved to", csv_file_path, "\n"))
}

# Iterate over each city folder and process images and addresses
for (city_name in city_names) {
  city_folder <- file.path(base_path, city_name)
  if (dir.exists(city_folder)) {
    txt_file_path <- process_images(city_folder, city_name)
    parse_addresses(txt_file_path, city_name)
  } else {
    cat(paste("Folder does not exist:", city_folder, "\n"))
  }
}









##### 2020 images -> CSVs




# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)

# Base path to the folder containing city folders
base_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2020_Images"

# List of city folder names
city_names <- c("Boston", "Atlanta", "Chicago", "DC", "Denver", "Detroit", "Houston", "NewOrleans", "Philly", "SanDiego", "Seattle")

# Function to process and extract text from images
process_images <- function(city_folder, city_name) {
  # Get list of all HEIC files in the city folder
  file_list <- list.files(path = city_folder, pattern = "(?i)\\.HEIC$", full.names = TRUE)
  
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
  txt_file_path <- file.path(city_folder, paste0(city_name, "_compiled_addresses.txt"))
  writeLines(all_text, con = txt_file_path)
  
  # Print confirmation
  cat(paste("Data saved to", txt_file_path, "\n"))
  
  return(txt_file_path)
}

# Function to parse addresses and create a CSV
parse_addresses <- function(txt_file_path, city_name) {
  # Read the TXT file
  addresses <- readLines(txt_file_path)
  
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
  
  # Replace "NA" or empty values in City with the current city name
  address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- city_name
  
  if (nrow(address_df) > 0) {
    # Add state and country columns (assuming all addresses are in the same state and country)
    address_df$state <- "MA"  # Adjust state as needed
    address_df$country <- "USA"
  }
  
  # Remove rows with all "NA" values
  address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]
  
  # Write the data frame to a CSV file
  csv_file_path <- file.path(dirname(txt_file_path), paste0(city_name, "_compiled_addresses.csv"))
  write.csv(address_df, csv_file_path, row.names = FALSE)
  
  # Print confirmation
  cat(paste("Data saved to", csv_file_path, "\n"))
}

# Iterate over each city folder and process images and addresses
for (city_name in city_names) {
  city_folder <- file.path(base_path, city_name)
  if (dir.exists(city_folder)) {
    txt_file_path <- process_images(city_folder, city_name)
    parse_addresses(txt_file_path, city_name)
  } else {
    cat(paste("Folder does not exist:", city_folder, "\n"))
  }
}


















# Load libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidygeocoder)
library(sf)

# Base path to the folder containing city folders
base_path <- "/Users/matthewlee/Desktop/RUPS_Dis/GayellowPages_Images/2000_Images"

# List of city folder names
city_names <- c("Houston")

# Function to process and extract text from images
process_images <- function(city_folder, city_name) {
  # Get list of all HEIC files in the city folder
  file_list <- list.files(path = city_folder, pattern = "(?i)\\.HEIC$", full.names = TRUE)
  
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
  txt_file_path <- file.path(city_folder, paste0(city_name, "_compiled_addresses.txt"))
  writeLines(all_text, con = txt_file_path)
  
  # Print confirmation
  cat(paste("Data saved to", txt_file_path, "\n"))
  
  return(txt_file_path)
}

# Function to parse addresses and create a CSV
parse_addresses <- function(txt_file_path, city_name) {
  # Read the TXT file
  addresses <- readLines(txt_file_path)
  
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
    parts <- unlist(strsplit(combined_address, "[.,;:] "))
    
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
  
  # Replace "NA" or empty values in City with the current city name
  address_df$City[address_df$City == "NA" | address_df$City == "" | is.na(address_df$City)] <- city_name
  
  if (nrow(address_df) > 0) {
    # Add state and country columns (assuming all addresses are in the same state and country)
    address_df$state <- "MA"  # Adjust state as needed
    address_df$country <- "USA"
  }
  
  # Remove rows with all "NA" values
  address_df <- address_df[apply(address_df, 1, function(row) !all(is.na(row))), ]
  
  # Write the data frame to a CSV file
  csv_file_path <- file.path(dirname(txt_file_path), paste0(city_name, "_compiled_addresses.csv"))
  write.csv(address_df, csv_file_path, row.names = FALSE)
  
  # Print confirmation
  cat(paste("Data saved to", csv_file_path, "\n"))
}

# Iterate over each city folder and process images and addresses
for (city_name in city_names) {
  city_folder <- file.path(base_path, city_name)
  if (dir.exists(city_folder)) {
    txt_file_path <- process_images(city_folder, city_name)
    parse_addresses(txt_file_path, city_name)
  } else {
    cat(paste("Folder does not exist:", city_folder, "\n"))
  }
}











###### merging census shapefiles 


install.packages("sf")
install.packages("dplyr")
# Load necessary packages
library(sf)
library(dplyr)

# Define the folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/CensusTractShapefiles"

# Get a list of all shapefiles in the folder
shapefiles <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)

# Read and combine all shapefiles
all_shapes <- lapply(shapefiles, st_read)

# Combine all shapes into one data frame
combined_shapes <- do.call(rbind, all_shapes)

# Define the state FIPS codes to keep (three digits)
state_fips <- c("013", "025", "011", "008", "048", "053", "060", "017")

# Create a unified STATEFP variable, handling both `STATEFP` and `NHGISST`
combined_shapes <- combined_shapes %>%
  mutate(STATEFP = coalesce(STATEFP, NHGISST))

# Filter the combined shapefile to keep only the specified states
filtered_shapes <- combined_shapes %>% filter(STATEFP %in% state_fips)

# Define the path for the new shapefile
output_path <- file.path(folder_path, "Filtered_States_Shapefile.shp")

# Save the filtered shapefile
st_write(filtered_shapes, output_path)

cat("Filtered shapefile saved at:", output_path)











# Load necessary packages
library(sf)
library(dplyr)

# Define the folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/CensusTractShapefiles"

# Get a list of all shapefiles in the folder
shapefiles <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)

# Read and standardize all shapefiles
all_shapes <- lapply(shapefiles, function(file) {
  shape <- st_read(file)
  # Ensure both STATEFP and NHGISST columns exist
  if (!("STATEFP" %in% colnames(shape))) {
    shape$STATEFP <- NA
  }
  if (!("NHGISST" %in% colnames(shape))) {
    shape$NHGISST <- NA
  }
  return(shape)
})

# Standardize columns to match across all shapefiles
all_shapes <- lapply(all_shapes, function(shape) {
  shape %>%
    mutate(STATEFP = coalesce(STATEFP, NHGISST)) %>%
    select(-NHGISST)  # Remove NHGISST if no longer needed
})

# Append all shapes into one data frame
combined_shapes <- do.call(rbind, all_shapes)

# Define the state FIPS codes to keep (three digits)
state_fips <- c("013", "025", "011", "008", "048", "053", "060", "017")

# Filter the combined shapefile to keep only the specified states
filtered_shapes <- combined_shapes %>% filter(STATEFP %in% state_fips)

# Define the path for the new shapefile
output_path <- file.path(folder_path, "Filtered_States_Shapefile.shp")

# Save the filtered shapefile
st_write(filtered_shapes, output_path)

cat("Filtered shapefile saved at:", output_path)








# Load necessary packages
library(sf)
library(dplyr)

# Define the folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/CensusTractShapefiles"

# Get a list of all shapefiles in the folder
shapefiles <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)

# Function to standardize columns across all shapefiles
standardize_columns <- function(shape) {
  # Ensure both STATEFP and NHGISST columns exist
  if (!("STATEFP" %in% colnames(shape))) {
    shape$STATEFP <- NA
  }
  if (!("NHGISST" %in% colnames(shape))) {
    shape$NHGISST <- NA
  }
  
  # Create a unified STATEFP column
  shape <- shape %>%
    mutate(STATEFP = coalesce(STATEFP, NHGISST)) %>%
    select(-NHGISST)  # Remove NHGISST if no longer needed
  
  return(shape)
}

# Read, standardize, and combine all shapefiles
all_shapes <- lapply(shapefiles, function(file) {
  shape <- st_read(file)
  standardized_shape <- standardize_columns(shape)
  return(standardized_shape)
})

# Find the union of all column names
all_columns <- unique(unlist(lapply(all_shapes, colnames)))

# Function to ensure all shapes have the same columns
ensure_columns <- function(shape, all_columns) {
  missing_columns <- setdiff(all_columns, colnames(shape))
  for (col in missing_columns) {
    shape[[col]] <- NA
  }
  return(shape)
}

# Apply ensure_columns to all shapes
all_shapes <- lapply(all_shapes, ensure_columns, all_columns = all_columns)

# Combine all shapes into one data frame
combined_shapes <- do.call(rbind, all_shapes)

# Define the state FIPS codes to keep (three digits)
state_fips <- c("013", "025", "011", "008", "048", "053", "060", "017")

# Filter the combined shapefile to keep only the specified states
filtered_shapes <- combined_shapes %>% filter(STATEFP %in% state_fips)

# Define the path for the new shapefile
output_path <- file.path(folder_path, "Filtered_States_Shapefile.shp")

# Save the filtered shapefile
st_write(filtered_shapes, output_path)

cat("Filtered shapefile saved at:", output_path)













# Load necessary packages
library(sf)
library(dplyr)

# Define the folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/CensusTractShapefiles"

# Get a list of all shapefiles in the folder
shapefiles <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)

# Function to standardize columns across all shapefiles
standardize_columns <- function(shape) {
  # Ensure both STATEFP and NHGISST columns exist
  if (!("STATEFP" %in% colnames(shape))) {
    shape$STATEFP <- NA
  }
  if (!("NHGISST" %in% colnames(shape))) {
    shape$NHGISST <- NA
  }
  
  # Create a unified STATEFP column
  shape <- shape %>%
    mutate(STATEFP = coalesce(STATEFP, NHGISST)) %>%
    select(-NHGISST)  # Remove NHGISST if no longer needed
  
  return(shape)
}

# Read, standardize, and combine all shapefiles
all_shapes <- lapply(shapefiles, function(file) {
  shape <- st_read(file)
  standardized_shape <- standardize_columns(shape)
  return(standardized_shape)
})

# Find the union of all column names
all_columns <- unique(unlist(lapply(all_shapes, colnames)))

# Function to ensure all shapes have the same columns
ensure_columns <- function(shape, all_columns) {
  missing_columns <- setdiff(all_columns, colnames(shape))
  for (col in missing_columns) {
    shape[[col]] <- NA
  }
  return(shape)
}

# Apply ensure_columns to all shapes
all_shapes <- lapply(all_shapes, ensure_columns, all_columns = all_columns)

# Resolve duplicate column names by making them unique
all_shapes <- lapply(all_shapes, function(shape) {
  names(shape) <- make.unique(names(shape))
  return(shape)
})

# Combine all shapes into one data frame
combined_shapes <- do.call(rbind, all_shapes)

# Define the state FIPS codes to keep (three digits)
state_fips <- c("013", "025", "011", "008", "048", "053", "060", "017")

# Filter the combined shapefile to keep only the specified states
filtered_shapes <- combined_shapes %>% filter(STATEFP %in% state_fips)

# Define the path for the new shapefile
output_path <- file.path(folder_path, "Filtered_States_Shapefile.shp")

# Save the filtered shapefile
st_write(filtered_shapes, output_path)

cat("Filtered shapefile saved at:", output_path)












# Load necessary packages
library(sf)
library(dplyr)

# Define the folder path
folder_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/CensusTractShapefiles"

# Get a list of all shapefiles in the folder
shapefiles <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)

# Define the state FIPS codes to keep (both two digits and three digits)
state_fips <- c("13", "25", "11", "08", "48", "53", "06", "17", "013", "025", "011", "008", "048", "053", "060", "017")

# Function to standardize and filter shapefiles
process_shapefile <- function(file, state_fips) {
  shape <- st_read(file)
  
  # Ensure both STATEFP and NHGISST columns exist
  if (!("STATEFP" %in% colnames(shape))) {
    shape <- shape %>%
      mutate(STATEFP = NA)
  }
  if (!("NHGISST" %in% colnames(shape))) {
    shape <- shape %>%
      mutate(NHGISST = NA)
  }
  
  # Create a unified STATEFP column
  shape <- shape %>%
    mutate(STATEFP = coalesce(STATEFP, NHGISST)) %>%
    select(-NHGISST)  # Remove NHGISST if no longer needed
  
  # Filter the shapefile to keep only the specified states
  filtered_shape <- shape %>% filter(STATEFP %in% state_fips)
  
  # Only save the filtered shapefile if it has rows
  if (nrow(filtered_shape) > 0) {
    # Define the path for the new shapefile
    new_file_name <- sub(".shp$", "_Filtered.shp", basename(file))
    output_path <- file.path(folder_path, new_file_name)
    
    # Save the filtered shapefile
    st_write(filtered_shape, output_path)
    
    cat("Filtered shapefile saved at:", output_path, "\n")
  } else {
    cat("No matching states found in:", file, "\n")
  }
}

# Apply the function to each shapefile
lapply(shapefiles, process_shapefile, state_fips = state_fips)
