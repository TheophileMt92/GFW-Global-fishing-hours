library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(sf)
library(tidyverse)
library(future)

#---- Open the AIS data 

# Set the path to the folder
path <- "Data/AIS Fishing Effort 2017-2020"

# List all CSV files in the folder
AIS_csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Read all CSV files and combine them into a single data frame
AIS_fishing <- AIS_csv_files %>%
  map_df(~read_csv(.))

# Aggregate fishing hours by latitude and longitude
aggregated_AIS_fishing <- AIS_fishing %>%
  group_by(cell_ll_lat, cell_ll_lon) %>%
  summarise(total_fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>%
  ungroup() %>%
  # Remove any cells with zero or negative fishing hours
  filter(total_fishing_hours > 0)

#---- Open the SAR data 

# Set the path to the 2016 folder
path <- "Data/SAR Vessel detections 2017-2020"

# List all CSV files in the folder
SAR_csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

# Read all CSV files and combine them into a single data frame
SAR_fishing <- SAR_csv_files %>%
  map_df(~read_csv(.))

# Aggregate fishing hours by latitude and longitude
aggregated_SAR_fishing <- SAR_fishing %>%
  mutate(
    lat_rounded = round(lat, digits = 2),
    lon_rounded = round(lon, digits = 2)
  ) %>%
  #  filter(matched_category == "fishing") %>%
  group_by(lat_rounded, lon_rounded) %>%
  filter(fishing_score >= 0.9) %>%
  summarise(
    total_presence_score = sum(presence_score, na.rm = TRUE),
    avg_fishing_score = mean(fishing_score, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(total_presence_score = round(total_presence_score, digits = 0)) %>%
  ungroup()


# Function to standardize coordinates to 0.1 degree resolution
standardize_coords <- function(lon, lat) {
  list(
    lon_std = floor(lon * 10) / 10,
    lat_std = floor(lat * 10) / 10
  )
}

# Aggregate AIS data to 1 degree resolution
AIS_data_01deg <- aggregated_AIS_fishing %>%
  mutate(coords = purrr::map2(cell_ll_lon, cell_ll_lat, standardize_coords)) %>%
  mutate(
    lon_std = sapply(coords, function(x) x$lon_std),
    lat_std = sapply(coords, function(x) x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_fishing_hours = sum(total_fishing_hours, na.rm = TRUE), .groups = "drop")

# Aggregate SAR data to 1 degree resolution
SAR_data_01deg <- aggregated_SAR_fishing %>%
  mutate(coords = purrr::map2(lon_rounded, lat_rounded, standardize_coords)) %>%
  mutate(
    lon_std = sapply(coords, function(x) x$lon_std),
    lat_std = sapply(coords, function(x) x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_presence_score = sum(total_presence_score, na.rm = TRUE), .groups = "drop")

# Merge the datasets
combined_data_01deg <- full_join(
  AIS_data_01deg,
  SAR_data_01deg,
  by = c("lon_std", "lat_std")
) %>%
  mutate(
    has_AIS = !is.na(total_fishing_hours) & total_fishing_hours > 0,
    has_SAR = !is.na(total_presence_score) & total_presence_score > 0
  )

# Separate the data into training (both AIS and SAR) and prediction (only SAR) sets
training_data <- combined_data_01deg %>%
  filter(has_AIS & has_SAR) %>%
  select(total_fishing_hours, total_presence_score, lon_std, lat_std)

prediction_data <- combined_data_01deg %>%
  filter(!has_AIS & has_SAR) %>%
  select(total_presence_score, lon_std, lat_std)

# Train the random forest model with timing
set.seed(123)  # for reproducibility
rf_timing <- system.time({
  rf_model <- randomForest(
    total_fishing_hours ~ total_presence_score + lon_std + lat_std,
    data = training_data,
    ntree = 500,
    importance = TRUE
  )
})

# Print the time taken
print(paste("Random Forest model training time:", rf_timing["elapsed"], "seconds"))

save(rf_model,file="rf_model.Rdata")