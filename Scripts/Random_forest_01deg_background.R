library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(sf)
library(tidyverse)
library(future)

#---- Open the AIS data 

# Set the path to the folder
#path <- "Data/AIS Fishing Effort 2017-2020"

# List all CSV files in the folder
#AIS_csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Read all CSV files and combine them into a single data frame
#AIS_fishing <- AIS_csv_files %>%
#  map_df(~read_csv(.))

load(here::here("Data", "AIS_fishing.Rdata"))
load(here::here("Data", "SAR_fishing.Rdata"))

# Aggregate fishing hours by latitude and longitude
aggregated_AIS_fishing <- AIS_fishing %>%
  group_by(cell_ll_lat, cell_ll_lon) %>%
  summarise(total_fishing_hours = sum(fishing_hours, na.rm = TRUE), .groups = "drop") %>%
  filter(total_fishing_hours > 0)

# Aggregate fishing hours by latitude and longitude
aggregated_SAR_fishing <- SAR_fishing %>%
  mutate(
    lat_rounded = round(lat, digits = 2),
    lon_rounded = round(lon, digits = 2)
  ) %>%
  group_by(lat_rounded, lon_rounded) %>%
  filter(fishing_score >= 0.9) %>%
  summarise(
    total_presence_score = sum(presence_score, na.rm = TRUE),
    avg_fishing_score = mean(fishing_score, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(total_presence_score = round(total_presence_score, digits = 0))


# Function to standardize coordinates to 0.1 degree resolution
standardize_coords <- function(lon, lat) {
  list(
    lon_std = floor(lon * 10) / 10,
    lat_std = floor(lat * 10) / 10
  )
}

# Aggregate AIS data to 0.1 degree resolution
AIS_data_01deg <- aggregated_AIS_fishing %>%
  mutate(coords = purrr::map2(cell_ll_lon, cell_ll_lat, standardize_coords)) %>%
  mutate(
    lon_std = sapply(coords, function(x) x$lon_std),
    lat_std = sapply(coords, function(x) x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_fishing_hours = sum(total_fishing_hours, na.rm = TRUE), .groups = "drop")

# Aggregate SAR data to 0.1 degree resolution
SAR_data_01deg <- aggregated_SAR_fishing %>%
  mutate(coords = purrr::map2(lon_rounded, lat_rounded, standardize_coords)) %>%
  mutate(
    lon_std = sapply(coords, function(x) x$lon_std),
    lat_std = sapply(coords, function(x) x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_presence_score = sum(total_presence_score, na.rm = TRUE), .groups = "drop")

# Merge the datasets
combined_data_01deg <-full_join(
  AIS_data_01deg,
  SAR_data_01deg,
  by = c("lon_std", "lat_std")
) %>%
  mutate(
    has_AIS = !is.na(total_fishing_hours) & total_fishing_hours > 0,
    has_SAR = !is.na(total_presence_score) & total_presence_score > 0
  )

save(combined_data_01deg, file=here::here("data","combined_data_O1deg.Rdata"))

# Separate the data into training (both AIS and SAR) and prediction (only SAR) sets
training_data <- combined_data_01deg %>%
  filter(has_AIS & has_SAR) %>%
  select(total_fishing_hours, total_presence_score, lon_std, lat_std)

prediction_data <- combined_data_01deg %>%
  filter(!has_AIS & has_SAR) %>%
  select(total_presence_score, lon_std, lat_std)

# Train the random forest model with timing
#set.seed(123)  # for reproducibility
#rf_timing <- system.time({
#  rf_model <- randomForest(
#    total_fishing_hours ~ total_presence_score + lon_std + lat_std,
#    data = training_data,
#    ntree = 500,
#    importance = TRUE
#  )
#})

# Print the time taken
#print(paste("Random Forest model training time:", rf_timing["elapsed"], "seconds"))

#save(rf_model,file="rf_model_O1deg.Rdata")
#load(here::here("rf_model_01deg.Rdata"))

#--- With environmental raster layers 
# Load the raster data
load(here::here("raster_df.Rdata"))  # Assuming you've saved your raster dataframe

# Convert your dataframes to data.tables
library(data.table)
setDT(combined_data_01deg)
setDT(raster_df)

# Standardize coordinates in raster_df to match combined_data_01deg
# Create lon_std and lat_std in raster_df, rounding to 0.1 degree resolution
raster_df[, `:=`(
  lon_std = round(lon_std, digits = 1),
  lat_std = round(lat_std, digits = 1)
)]

# Aggregate raster_df to 0.1 degree resolution
aggregated_raster_df <- raster_df[, .(
  dist_shore = mean(dist_shore, na.rm = TRUE),
  dist_ports = mean(dist_ports, na.rm = TRUE),
  bathy = mean(bathy, na.rm = TRUE)
), by = .(lon_std, lat_std)]


# Perform the join using data.table
combined_data_with_rasters <- raster_df[combined_data_01deg, 
                                        on = .(lon_std, lat_std), 
                                  nomatch = 0]
head(raster_df)
head(combined_data_01deg)
# Convert back to dataframe if needed
combined_data_with_rasters <- as.data.frame(combined_data_with_rasters)

# Update training and prediction datasets
training_data <- combined_data_with_rasters %>%
  filter(has_AIS & has_SAR) %>%
  select(total_fishing_hours, total_presence_score, lon_std, lat_std, dist_shore, dist_ports, bathy)

prediction_data <- combined_data_with_rasters %>%
  filter(!has_AIS & has_SAR) %>%
  select(total_presence_score, lon_std, lat_std, dist_shore, dist_ports, bathy)

# Train the new random forest model
set.seed(123)  # for reproducibility
rf_timing <- system.time({
  rf_model_new <- randomForest(
    total_fishing_hours ~ total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy,
    data = training_data,
    ntree = 500,
    importance = TRUE
  )
})

# Print the time taken
print(paste("Random Forest model training time:", rf_timing["elapsed"], "seconds"))

# Save the new model
save(rf_model_new, file = "rf_model_01deg_with_rasters.Rdata")

# Print variable importance
print(importance(rf_model_new))

# Plot variable importance
varImpPlot(rf_model_new)

# Make predictions on the prediction dataset
predictions <- predict(rf_model_new, newdata = prediction_data)

# Add predictions to the prediction dataset
prediction_data$predicted_fishing_hours <- predictions

# You can now use prediction_data for further analysis or visualization


