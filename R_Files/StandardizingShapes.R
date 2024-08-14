

####### filtering for 1970 counties 







install.packages("sf")
install.packages("dplyr")
# Load necessary libraries
library(sf)
library(dplyr)

# File path
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970.shp"

# Read the shapefile
tracts <- st_read(shapefile_path)

# Create the 'statecountycode' column by combining 'NHGISST' and 'NHGISCTY'
tracts <- tracts %>%
  mutate(statecountycode = paste0(NHGISST, NHGISCTY))

# Filter the observations
filtered_tracts <- tracts %>%
  filter(statecountycode %in% c("5300330", "4802010", "1301210", "1300890", "2500250", "2500170", "0800310", "1100010"))

# Save the filtered shapefile
filtered_shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_filtered.shp"
st_write(filtered_tracts, filtered_shapefile_path)

##### making GISJOIN 

# Load necessary libraries
library(sf)
library(dplyr)
library(stringr)

# File path for the existing shapefile
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_filtered.shp"

# Read the shapefile
tracts <- st_read(shapefile_path)

# Convert GISJOIN to character if it is not already
tracts <- tracts %>%
  mutate(GISJOIN = as.character(GISJOIN)) %>%
  mutate(GISJOIN = str_pad(GISJOIN, width = 14, side = "right", pad = "0"))

# Define a new file path to save the updated shapefile
updated_shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_padded.shp"

# Check if the file already exists and remove it if necessary
if (file.exists(updated_shapefile_path)) {
  file.remove(updated_shapefile_path)
}

# Save the updated shapefile
st_write(tracts, updated_shapefile_path)


# Load necessary libraries
library(sf)
library(dplyr)
library(stringr)

# File path for the existing shapefile
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_filtered.shp"

# Read the shapefile
tracts <- st_read(shapefile_path)

# Pad GISJOIN with zeros at the end and rename the column to GISJOIN_1970
tracts <- tracts %>%
  mutate(GISJOIN = as.character(GISJOIN)) %>%
  mutate(GISJOIN = str_pad(GISJOIN, width = 14, side = "right", pad = "0")) %>%
  rename(GISJOIN_1970 = GISJOIN)

# Define a new file path to save the updated shapefile
updated_shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_padded.shp"

# Check if the file already exists and remove it if necessary
if (file.exists(updated_shapefile_path)) {
  file.remove(updated_shapefile_path)
}

# Save the updated shapefile
st_write(tracts, updated_shapefile_path)






# Load the sf package
library(sf)

# Define the path to the shapefile
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_padded.shp"

# Read the shapefile
gdf <- st_read(shapefile_path)

# Rename the column
if ("GISJOIN_" %in% names(gdf)) {
  names(gdf)[names(gdf) == "GISJOIN_"] <- "GISJOIN_1970"
} else {
  print("Column 'GISJOIN_' not found.")
}

# Define the path for the updated shapefile
updated_shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_renamed.shp"

# Write the updated shapefile
st_write(gdf, updated_shapefile_path)

print("Shapefile updated and saved.")











##### renameing GISJOIN 
# Load necessary libraries
library(sf)
library(dplyr)
library(stringr)

# File path for the existing shapefile
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_filtered.shp"

# Read the shapefile
tracts <- st_read(shapefile_path)

# Convert GISJOIN to character, pad with zeros at the end, and rename the column
tracts <- tracts %>%
  mutate(GISJOIN = as.character(GISJOIN)) %>%
  mutate(GISJOIN = str_pad(GISJOIN, width = 14, side = "right", pad = "0")) %>%
  rename(GISJOIN_1970 = GISJOIN)

# Define a new file path to save the updated shapefile
updated_shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_padded.shp"

# Check if the file already exists and remove it if necessary
if (file.exists(updated_shapefile_path)) {
  file.remove(updated_shapefile_path)
}

# Save the updated shapefile
st_write(tracts, updated_shapefile_path)


# Load the sf package
library(sf)

# Define the path to the shapefile
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_padded.shp"

# Read the shapefile
gdf <- st_read(shapefile_path)

# Rename the column
if ("GISJOIN_" %in% names(gdf)) {
  names(gdf)[names(gdf) == "GISJOIN_"] <- "GISJOIN_1970"
} else {
  print("Column 'GISJOIN_' not found.")
}

# Define the path for the updated shapefile
updated_shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/CensusTract_Shapes/FilteringForStates/Filtered_States/filtered_US_tract_1970_renamed.shp"

# Write the updated shapefile
st_write(gdf, updated_shapefile_path)

print("Shapefile updated and saved.")








#### standardising census tracts to 1970s
# Load necessary libraries
library(haven)       # For reading .dta files
library(sf)          # For spatial data manipulation
library(dplyr)       # For data manipulation

# File paths
dta_file <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/Census_Count_combined_data_allyears.dta"
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/points_shapefile.shp"

# Read the DTA file
data <- read_dta(dta_file)

# Ensure the LAT and LONG variables are correctly named
# Replace 'LAT' and 'LONG' with the actual names of your variables if they are different
data <- data %>%
  filter(!is.na(LAT) & !is.na(LONG))  # Remove rows with missing coordinates

# Convert the data frame to an sf object with LAT and LONG as coordinates
sf_data <- st_as_sf(data, coords = c("LONG", "LAT"), crs = 4326)

# Save the sf object to a shapefile
st_write(sf_data, shapefile_path, driver = "ESRI Shapefile")

# Confirm the shapefile was saved
print(paste("Shapefile saved to:", shapefile_path))






















library(haven)
library(sf)
library(ggplot2)
library(dplyr)

# Load the DTA file
dta_file <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/Census_Count_combined_data_allyears.dta"
data <- read_dta(dta_file)

# Check the names of the variables
names(data)

# Ensure LAT and LONG variables exist and are correctly named
# Assuming the variables are named LAT and LONG; modify if needed
if (!("LAT" %in% names(data)) | !("LONG" %in% names(data))) {
  stop("LAT and/or LONG variables are missing from the dataset.")
}

# Create a spatial data frame
spatial_data <- data %>%
  filter(!is.na(LAT) & !is.na(LONG)) %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326)  # Assuming WGS84 coordinate system

# Plot the data
ggplot() +
  geom_sf(data = spatial_data, color = "blue", size = 0.5) +
  labs(title = "Geocoded Points",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Save to shapefile
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/geocoded_points.shp"
st_write(spatial_data, shapefile_path)

print(paste("Shapefile saved to:", shapefile_path))




# Load required library
library(tibble)

# Define the coordinates and city names
cities <- tibble(
  city = c("Boston", "Atlanta", "Denver", "Seattle", "Houston", "Washington, D.C."),
  latitude = c(42.3601, 33.7490, 39.7392, 47.6062, 29.7604, 38.9072),
  longitude = c(-71.0589, -84.3880, -104.9903, -122.3321, -95.3698, -77.0369)
)

# Define the file path
output_file <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/CBDs_WGS84_with_coords.csv"

# Save as a CSV file
write.csv(cities, output_file, row.names = FALSE)

# Print message
cat("CSV file saved at:", output_file)



# Load required libraries
library(sf)
library(tibble)

# Define the coordinates and city names
cities <- tibble(
  city = c("Boston", "Atlanta", "Denver", "Seattle", "Houston", "Washington, D.C."),
  latitude = c(42.3601, 33.7490, 39.7392, 47.6062, 29.7604, 38.9072),
  longitude = c(-71.0589, -84.3880, -104.9903, -122.3321, -95.3698, -77.0369)
)

# Convert to an sf object in WGS 84
cities_sf <- st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4326)

# Transform coordinates to the desired projected coordinate system (e.g., EPSG:3857)
cities_sf_proj <- st_transform(cities_sf, crs = 3857)

# Extract the projected coordinates
cities_proj <- st_coordinates(cities_sf_proj)
cities <- cities %>%
  mutate(x = cities_proj[, 1], y = cities_proj[, 2])

# Define the file path
output_file <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/CBDs_projected_coords.csv"

# Save as a CSV file
write.csv(cities, output_file, row.names = FALSE)

# Print message
cat("CSV file saved at:", output_file)






# Load required libraries
library(sf)
library(tibble)

# Define the coordinates and city names
cities <- tibble(
  city = c("Boston", "Atlanta", "Denver", "Seattle", "Houston", "Washington, D.C."),
  latitude = c(42.3601, 33.7490, 39.7392, 47.6062, 29.7604, 38.9072),
  longitude = c(-71.0589, -84.3880, -104.9903, -122.3321, -95.3698, -77.0369)
)

# Convert to an sf object in WGS 84
cities_sf <- st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4326)

# Transform coordinates to NAD_1983_Albers (EPSG:102003)
cities_sf_proj <- st_transform(cities_sf, crs = 102003)

# Extract the projected coordinates
cities_proj <- st_coordinates(cities_sf_proj)
cities <- cities %>%
  mutate(x = cities_proj[, 1], y = cities_proj[, 2])

# Define the file path
output_file <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/CBDs_NAD1983_Albers_coords.csv"

# Save as a CSV file
write.csv(cities, output_file, row.names = FALSE)

# Print message
cat("CSV file saved at:", output_file)












##### calculating distance 
# Install the necessary libraries
install.packages("sf")
install.packages("dplyr")
install.packages("geosphere")




# Load the necessary libraries
library(sf)
library(geosphere)

# Read the shapefiles
tracts <- st_read("/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/1970_CRS.shp")
cbds <- st_read("/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/CBDs_WGS84_with_coords.shp")

# Ensure both shapefiles use the same CRS
if (st_crs(tracts) != st_crs(cbds)) {
  tracts <- st_transform(tracts, crs = st_crs(cbds))
}

# Create a common key for joining based on 3-digit county FIPS code
tracts$county_fips <- substr(tracts$NHGISCT, 1, 3)
cbds$county_fips <- substr(cbds$city, 1, 3)

# Perform a spatial join to match CBDs to tracts based on county FIPS code
tracts <- st_join(tracts, cbds, join = st_intersects)

# Extract centroids of tracts
tracts_centroids <- st_centroid(tracts)

# Calculate the distance from each census tract centroid to the CBD
tracts$dist <- mapply(function(xlong, ylat, cblong, cblat) {
  distVincentySphere(c(xlong, ylat), c(cblong, cblat))
}, tracts$XLong, tracts$YLat, tracts$longitude, tracts$latitude)

# Convert distance to numeric (meters)
tracts$dist <- as.numeric(tracts$dist)

# Save the updated shapefile with the distance information
st_write(tracts, "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/1970_CRS_with_dist.shp")















#### with merged shapefiles 

# Install and load necessary packages
install.packages(c("sf", "geosphere"))
# Install and load necessary packages
install.packages(c("sf", "geosphere", "dplyr"))









library(sf)
library(geosphere)
library(dplyr)

# Path to the shapefile
shapefile_path <- "/Users/matthewlee/Desktop/RUPS_Dis/Census_Data/Combined_Data/CBDs_WGS84_with_coords.shp"

# Read the shapefile
data <- st_read(shapefile_path)

# Ensure the coordinates are numeric and handle missing values
data <- data %>%
  mutate(
    XLong = as.numeric(XLong),
    YLat = as.numeric(YLat),
    XLong_2 = as.numeric(XLong_2),
    YLat_X = as.numeric(YLat_2)
  ) %>%
  filter(!is.na(XLong) & !is.na(YLat) & !is.na(XLong_2) & !is.na(YLat_2))

# Calculate distance in meters and add to data
data <- data %>%
  rowwise() %>%
  mutate(
    dist = distHaversine(c(XLong, YLat), c(XLong_2, YLat_2))
  ) %>%
  ungroup()

# Write the updated shapefile with the new distance column
st_write(data, shapefile_path, delete_layer = TRUE)
