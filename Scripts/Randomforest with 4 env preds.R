library(ncdf4)
library(here)
library(raster)
library(data.table)
library(dplyr)
library(randomForest)

# Load the SST raster layer
SST_raster <- raster(here::here("Data", "Environmental data layers", "Average_SST_2021.tif"))

# Open the saved adjusted rasters
Ports_adjusted <- raster(here::here("Data", "Environmental data layers", "distance-from-port-0.1deg-adjusted.tif"))
Shore_adjusted <- raster(here::here("Data", "Environmental data layers", "distance-from-shore-0.1deg-adjusted.tif"))
Bathy_adjusted <- raster(here::here("Data", "Environmental data layers", "bathymetry-0.1deg-adjusted.tif"))

# Resample SST to match one of the other rasters (e.g., Shore_adjusted)
SST_resampled <- resample(SST_raster, Shore_adjusted, method = "bilinear")

# Optionally extend SST if you want to ensure it covers the same extent.
# SST_resampled <- extend(SST_resampled, Shore_adjusted)

# Stack the rasters including the adjusted SST
raster_stack <- stack(Shore_adjusted, Ports_adjusted, Bathy_adjusted, SST_resampled)

# Convert the stack to a dataframe
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Rename the columns
names(raster_df) <- c("x", "y", "dist_shore", "dist_ports", "bathy", "SST")

# Remove NA values if desired
raster_df <- na.omit(raster_df)

# Convert to data.table for efficiency
setDT(raster_df)

# Round x and y to 1 decimal place for consistency
raster_df[, `:=`(
  lon_std = round(x, digits = 1),
  lat_std = round(y, digits = 1)
)]

# Load the combined data
load(here::here("data", "combined_data_O1deg.Rdata"))
setDT(combined_data_01deg)

# Perform the join using data.table to include SST
combined_data_with_rasters <- raster_df[combined_data_01deg, 
                                        on = .(lon_std, lat_std), 
                                        nomatch = 0]

# Convert back to dataframe if needed
combined_data_with_rasters <- as.data.frame(combined_data_with_rasters)

# Prepare the training data
training_data <- combined_data_with_rasters %>%
  filter(has_AIS & has_SAR) %>%
  dplyr::select(total_fishing_hours, total_presence_score, lon_std, lat_std, dist_shore, dist_ports, bathy, SST) %>%
  na.omit()

# Train the random forest model
set.seed(123)  # for reproducibility
rf_timing <- system.time({
  rf_model_env_4envvar <- randomForest(
    total_fishing_hours ~ total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy + SST,
    data = training_data,
    ntree = 500,
    importance = TRUE
  )
})

cat("Random Forest model training time:", rf_timing["elapsed"], "seconds\n")

# Save the new model
save(rf_model_env_4envvar, file = "rf_model_01deg_with_4rasters.Rdata")