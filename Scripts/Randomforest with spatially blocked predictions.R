library(ncdf4)
library(here)
library(raster)
library(data.table)
library(dplyr)
library(randomForest)

# Load the SST raster layer
#SST_raster <- raster(here::here("Data", "Environmental data layers", "Average_SST_2021.tif"))

# Open the saved adjusted rasters
Ports_adjusted <- raster(here::here("Data", "Environmental data layers", "distance-from-port-0.1deg-adjusted.tif"))
Shore_adjusted <- raster(here::here("Data", "Environmental data layers", "distance-from-shore-0.1deg-adjusted.tif"))
Bathy_adjusted <- raster(here::here("Data", "Environmental data layers", "bathymetry-0.1deg-adjusted.tif"))

# Resample SST to match one of the other rasters (e.g., Shore_adjusted)
#SST_resampled <- resample(SST_raster, Shore_adjusted, method = "bilinear")

# Optionally extend SST if you want to ensure it covers the same extent.
# SST_resampled <- extend(SST_resampled, Shore_adjusted)

# Stack the rasters including the adjusted SST
raster_stack <- stack(Shore_adjusted, Ports_adjusted, Bathy_adjusted) #, SST_resampled)

# Convert the stack to a dataframe
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Rename the columns
names(raster_df) <- c("x", "y", "dist_shore", "dist_ports", "bathy") #, "SST")

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
  dplyr::select(total_fishing_hours, total_presence_score, lon_std, lat_std, dist_shore, dist_ports, bathy) %>% #, SST
  na.omit()

#------------------------- Random forest with spatial cross-validation
# Load necessary libraries
library(blockCV)
library(doParallel)
library(randomForest)
library(sp)
library(dplyr)

# Assuming your training_data has lon_std (longitude) and lat_std (latitude) columns
coords <- SpatialPoints(training_data[, c("lon_std", "lat_std")])

# Combine your training data with spatial coordinates
spatial_data <- SpatialPointsDataFrame(coords, data = training_data)

# Create 5 systematic spatial blocks using cv_spatial without specifying a response variable
spatial_blocks <- cv_spatial(
  x = spatial_data,          # Full dataset (SpatialPointsDataFrame with predictors and response variable)
  partitionType = "systematic",  # Systematic block assignment based on spatial proximity
  range = 50000,            # Define the block size (50 km blocks)
  k = 5,                    # Number of spatial blocks (for 5-fold cross-validation)
  selection = "systematic",  # Systematic block assignment
  iteration = 100           # Number of iterations for block optimization
)

# Plot the spatial blocks (without extra arguments)
cv_plot(spatial_blocks)

library(ggplot2)
library(sf)

# Convert spatial_data to sf object if it's not already
if (!inherits(spatial_data, "sf")) {
  spatial_data_sf <- st_as_sf(spatial_data)
} else {
  spatial_data_sf <- spatial_data
}

# Extract the fold assignments from spatial_blocks
fold_assignments <- spatial_blocks$foldID

# Add fold assignments to the sf object
spatial_data_sf$fold <- as.factor(fold_assignments)

# Create the plot
ggplot() +
  geom_sf(data = spatial_data_sf, aes(color = fold), size = 1) +
  scale_color_viridis_d(name = "Fold") +
  theme_minimal() +
  labs(title = "Spatial Blocks for 5-Fold Cross-Validation",
       subtitle = "Points colored by fold assignment") +
  theme(legend.position = "bottom")

#Testing to split the coordinates with k-means clustering
# Ensure spatial_data is an sf object
if (!inherits(spatial_data, "sf")) {
  spatial_data_sf <- st_as_sf(spatial_data)
} else {
  spatial_data_sf <- spatial_data
}

# Extract coordinates
coords <- st_coordinates(spatial_data_sf)

# Perform k-means clustering
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(coords, centers = 6)

# Add cluster assignments to the sf object
spatial_data_sf$block <- as.factor(kmeans_result$cluster)

# Create the plot
ggplot() +
  geom_sf(data = spatial_data_sf, aes(color = block), size = 1) +
  scale_color_viridis_d(name = "Block") +
  theme_minimal() +
  labs(title = "Spatial Blocks for 6-Fold Cross-Validation",
       subtitle = "Points colored by block assignment") +
  theme(legend.position = "bottom")

# Register cores for parallel processing (use 1 less than your total cores)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Initialize a list to store results
results <- foreach(i = 1:6, .packages = c("randomForest", "dplyr"), .export = "spatial_data_sf") %dopar% {
  
  # Get the indices of the training and test sets for the i-th block
  test_indices <- spatial_data_sf$block == i
  train_data <- spatial_data_sf[!test_indices, ]
  test_data <- spatial_data_sf[test_indices, ]
  
  # Train Random Forest on the training data
  rf_model <- randomForest(
    total_fishing_hours ~ total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy,
    data = train_data,
    ntree = 500,
    importance = TRUE
  )
  
  # Save the model
  model_filename <- paste0("rf_model_fold_", i, ".rds")
  saveRDS(rf_model, file = model_filename)
  
  # Predict on the test data
  predictions <- predict(rf_model, test_data)
  
  # Return a list with results and model filename
  list(
    results = data.frame(observed = test_data$total_fishing_hours, predicted = predictions, fold = i),
    model_filename = model_filename
  )
}

# Stop the cluster once done
stopCluster(cl)

# Extract results and model filenames
results_df <- do.call(rbind, lapply(results, function(x) x$results))
model_filenames <- sapply(results, function(x) x$model_filename)

head(results_df)
# Save results
save(results_df, file = "spatial_cv_results.RData")
save(model_filenames, file = "model_filenames.RData")

# Print confirmation
print("Results saved to 'spatial_cv_results.RData'")
print("Model filenames saved to 'model_filenames.RData'")
print("Individual models saved as separate .rds files")

# If you want to load a specific model later, you can use:
# model <- readRDS("rf_model_fold_1.rds")  # for example, to load the model for fold 1
# Combine the results and calculate overall performance metrics
rmse <- sqrt(mean((results$predicted - results$observed)^2))
r2 <- 1 - (sum((results$observed - results$predicted)^2) / sum((results$observed - mean(results$observed))^2))

# Print the performance metrics
print(paste("6-fold spatial block CV RMSE:", rmse))
print(paste("6-fold spatial block CV R-squared:", r2))

library(sp)
library(spdep)

# Ensure spatial_data and results_df are aligned
spatial_data <- spatial_data[order(row.names(spatial_data)), ]
results_df <- results_df[order(row.names(results_df)), ]

# Get all coordinates
all_coordinates <- coordinates(spatial_data)

# Create neighbor list for the entire dataset
k <- 30  # Number of nearest neighbors, adjust as needed
nb_all <- knn2nb(knearneigh(all_coordinates, k = k))

# Create spatial weights for the entire dataset
lw_all <- nb2listw(nb_all, style="W", zero.policy = TRUE)

# Function to calculate Moran's I
calculate_morans_i <- function(residuals, indices, lw_all) {
  # Create a logical vector for subsetting
  subset_vector <- rep(FALSE, length(lw_all$neighbours))
  subset_vector[indices] <- TRUE
  
  # Subset the weights list for the current fold
  lw_subset <- subset.listw(lw_all, subset_vector, zero.policy = TRUE)
  
  # Calculate Moran's I
  moran_result <- moran.test(residuals, lw_subset, zero.policy = TRUE)
  return(moran_result)
}

# Calculate Moran's I for each fold in the spatial cross-validation
morans_i_results <- list()

for (i in 1:6) {
  # Get the indices for this fold
  fold_indices <- which(results_df$fold == i)
  
  # Calculate residuals
  fold_residuals <- results_df$observed[fold_indices] - results_df$predicted[fold_indices]
  
  # Calculate Moran's I for this fold
  morans_i_results[[i]] <- calculate_morans_i(fold_residuals, fold_indices, lw_all)
  
  # Print progress
  print(paste("Completed fold", i))
}

save(morans_i_results, file="morans_i_results.Rdata")

# Print results
print("Moran's I for each fold in spatial cross-validation:")
for (i in 1:6) {
  print(paste("Fold", i))
  print(morans_i_results[[i]])
}

# Calculate average Moran's I statistic across folds
average_morans_i <- mean(sapply(morans_i_results, function(x) x$estimate[1]))
print(paste("Average Moran's I across folds:", average_morans_i))

# Visualize Moran's I results
morans_i_df <- data.frame(
  Fold = paste("Fold", 1:5),
  Morans_I = sapply(morans_i_results, function(x) x$estimate[1])
)

ggplot(morans_i_df, aes(x = Fold, y = Morans_I)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = average_morans_i, linetype = "dashed", color = "green") +
  labs(title = "Moran's I for Spatial Cross-Validation Folds",
       y = "Moran's I") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add p-values to the results
morans_i_df$p_value <- sapply(morans_i_results, function(x) x$p.value)

# Print the data frame with Moran's I and p-values
print(morans_i_df)