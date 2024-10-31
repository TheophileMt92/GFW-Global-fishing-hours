library(raster)
library(ggplot2)
library(data.table)
library(dplyr)
library(randomForest)
library(caret)
library(sf)
library(tidyverse)
library(future)

# Read the TIFF files
#Dist_ports <- raster(here::here("Data", "Environmental data layers", "distance-from-port-v20201104.tiff"))
#Dist_shore <- raster(here::here("Data", "Environmental data layers", "distance-from-shore.tif"))
#Bathy <- raster(here::here("Data", "Environmental data layers", "bathymetry.tif"))

# Create a template raster with 0.1 degree resolution
#template <- raster(extent(-180, 180, -90, 90), resolution = 0.1)

# Resample all rasters to 0.1 degree resolution
#Ports_adjusted <- resample(Dist_ports, template, method = "bilinear")
#Shore_adjusted <- resample(Dist_shore, template, method = "bilinear")
#Bathy_adjusted <- resample(Bathy, template, method = "bilinear")

# Save the adjusted rasters with different names
#writeRaster(Ports_adjusted, filename = here::here("Data", "Environmental data layers", "distance-from-port-0.1deg-adjusted.tif"), format = "GTiff", overwrite = TRUE)
#writeRaster(Shore_adjusted, filename = here::here("Data", "Environmental data layers", "distance-from-shore-0.1deg-adjusted.tif"), format = "GTiff", overwrite = TRUE)
#writeRaster(Bathy_adjusted, filename = here::here("Data", "Environmental data layers", "bathymetry-0.1deg-adjusted.tif"), format = "GTiff", overwrite = TRUE)

# Now, open the saved adjusted rasters
Ports_adjusted <- raster(here::here("Data", "Environmental data layers", "distance-from-port-0.1deg-adjusted.tif"))
Shore_adjusted <- raster(here::here("Data", "Environmental data layers", "distance-from-shore-0.1deg-adjusted.tif"))
Bathy_adjusted <- raster(here::here("Data", "Environmental data layers", "bathymetry-0.1deg-adjusted.tif"))

# Stack the resampled rasters
raster_stack <- stack(Shore_adjusted, Ports_adjusted, Bathy_adjusted)

# Convert the stack to a dataframe
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Rename the columns
names(raster_df) <- c("x", "y", "dist_shore", "dist_ports", "bathy")

# Remove NA values if desired
raster_df <- na.omit(raster_df)

# Convert to data.table for efficiency
setDT(raster_df)

# Round x and y to 1 decimal place for consistency
raster_df[, `:=`(
  lon_std = round(x, digits = 1),
  lat_std = round(y, digits = 1)
)]

# View the first few rows of the dataframe
print(head(raster_df))

# Now proceed with the join and model training
load(here::here("data","combined_data_O1deg.Rdata"))
setDT(combined_data_01deg)

# Perform the join using data.table
combined_data_with_rasters <- raster_df[combined_data_01deg, 
                                        on = .(lon_std, lat_std), 
                                        nomatch = 0]

# Convert back to dataframe if needed
combined_data_with_rasters <- as.data.frame(combined_data_with_rasters)

# Prepare the training data
training_data <- combined_data_with_rasters %>%
  filter(has_AIS & has_SAR) %>%
  select(total_fishing_hours, total_presence_score, lon_std, lat_std, dist_shore, dist_ports, bathy) %>%
  na.omit()

# Train the random forest model with timing
set.seed(123)  # for reproducibility
rf_timing <- system.time({
  rf_model_env <- randomForest(
    total_fishing_hours ~ total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy,
    data = training_data,
    ntree = 500,
    importance = TRUE
  )
})

cat("Random Forest model training time:", rf_timing["elapsed"], "seconds\n")

# Save the new model
save(rf_model_env, file = "rf_model_01deg_with_rasters.Rdata")
load("rf_model_01deg_with_rasters.Rdata")

# Variable importance plot
varImpPlot(rf_model_env)

# Print a summary of the model
print(rf_model_env)

evaluate_model <- function(model, data, log_target = FALSE) {
  predictions <- predict(model, newdata = data)
  if (log_target) {
    predictions <- 10^predictions - 1
  }
  
  actual <- data$total_fishing_hours
  
  # Basic Error Metrics
  mae <- mean(abs(actual - predictions), na.rm = TRUE)
  rmse <- sqrt(mean((actual - predictions)^2, na.rm = TRUE))
  mape <- mean(abs((actual - predictions) / actual) * 100, na.rm = TRUE)
  medae <- median(abs(actual - predictions), na.rm = TRUE)
  
  # R-squared (matching randomForest's % Var explained)
  r_squared <- model$rsq[length(model$rsq)]
  
  # Adjusted R-squared
  n <- length(actual)
  p <- length(model$forest$independent.variable.names) # Number of predictors
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
  
  # Residual Analysis
  residuals <- actual - predictions
  mean_residual <- mean(residuals, na.rm = TRUE)
  sd_residual <- sd(residuals, na.rm = TRUE)
  
  # Feature Importance (for Random Forest)
  feature_importance <- importance(model)
  
  return(list(
    "Mean Absolute Error" = mae,
    "Root Mean Squared Error" = rmse,
    "Mean Absolute Percentage Error" = mape,
    "Median Absolute Error" = medae,
    "R-Squared" = r_squared,
    "Adjusted R-Squared" = adj_r_squared,
    "Mean of Residuals" = mean_residual,
    "Standard Deviation of Residuals" = sd_residual,
    "Feature Importance" = feature_importance
  ))
}

validation_data <- combined_data_with_rasters %>%
  mutate(
    data_category = case_when(
      has_AIS & has_SAR ~ "Both AIS and SAR",
      has_AIS & !has_SAR ~ "Only AIS",
      !has_AIS & has_SAR ~ "Only SAR",
      TRUE ~ "No fishing detected"
    )
  )

# Evaluate all models
validation_data <- validation_data %>% filter(data_category == "Both AIS and SAR")
results_rf_env <- evaluate_model(rf_model_env, validation_data)

#Tables
# Create a dataframe for the table
results_table <- data.frame(
  Metric = c("Mean Absolute Error", "Root Mean Squared Error", "Mean Absolute Percentage Error",
             "Median Absolute Error", "R-Squared", "Adjusted R-Squared",
             "Mean of Residuals", "Standard Deviation of Residuals"),
  Value = round(c(results_rf_env$`Mean Absolute Error`,
                  results_rf_env$`Root Mean Squared Error`,
                  results_rf_env$`Mean Absolute Percentage Error`,
                  results_rf_env$`Median Absolute Error`,
                  results_rf_env$`R-Squared`,
                  results_rf_env$`Adjusted R-Squared`,
                  results_rf_env$`Mean of Residuals`,
                  results_rf_env$`Standard Deviation of Residuals`),
                2)  # Round to 2 decimal places
)

# Create and display the table
library(knitr)
library(kableExtra)

kable(results_table, format = "html", digits = 4, caption = "Model Evaluation Metrics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center")

# For feature importance, create a separate table
feature_importance <- as.data.frame(results_rf_env$`Feature Importance`)
feature_importance$Feature <- rownames(feature_importance)
feature_importance <- feature_importance[, c("Feature", "%IncMSE", "IncNodePurity")]
colnames(feature_importance) <- c("Feature", "%IncMSE", "IncNodePurity")

# Sort the feature importance table by %IncMSE in descending order
feature_importance <- feature_importance[order(-feature_importance$`%IncMSE`), ]

kable(feature_importance, format = "html", digits = 4, caption = "Feature Importance") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:3, width = "150px")

library(ggplot2)
library(gridExtra)
library(pdp)

# Function to create a partial dependence plot
create_pdp <- function(model, variable, data) {
  pd <- partial(model, pred.var = variable, train = data)
  ggplot(pd, aes_string(x = variable, y = "yhat")) +
    geom_line() +
    theme_minimal() +
    labs(x = variable, y = "Partial Dependence", title = paste("Partial Dependence Plot for", variable))
}

# Create partial dependence plots for each variable
pdp_total_presence_score <- create_pdp(rf_model_env, "total_presence_score", training_data)
pdp_lon_std <- create_pdp(rf_model_env, "lon_std", training_data)
pdp_lat_std <- create_pdp(rf_model_env, "lat_std", training_data)
pdp_dist_shore <- create_pdp(rf_model_env, "dist_shore", training_data)
pdp_dist_ports <- create_pdp(rf_model_env, "dist_ports", training_data)
pdp_bathy <- create_pdp(rf_model_env, "bathy", training_data)

# Save PDP objects individually
saveRDS(pdp_total_presence_score, "pdp_total_presence_score.rds")
saveRDS(pdp_lon_std, "pdp_lon_std.rds")
saveRDS(pdp_lat_std, "pdp_lat_std.rds")
saveRDS(pdp_dist_shore, "pdp_dist_shore.rds")
saveRDS(pdp_dist_ports, "pdp_dist_ports.rds")
saveRDS(pdp_bathy, "pdp_bathy.rds")

# Read individual PDP objects
pdp_total_presence_score <- readRDS("pdp_total_presence_score.rds")
pdp_lon_std <- readRDS("pdp_lon_std.rds")
pdp_lat_std <- readRDS("pdp_lat_std.rds")
pdp_dist_shore <- readRDS("pdp_dist_shore.rds")
pdp_dist_ports <- readRDS("pdp_dist_ports.rds")
pdp_bathy <- readRDS("pdp_bathy.rds")

# Arrange the plots in a grid
grid.arrange(
  pdp_total_presence_score, pdp_lon_std, pdp_lat_std,
  pdp_dist_shore, pdp_dist_ports, pdp_bathy,
  ncol = 2
)

#Test for autocorrelation 
library(randomForest)
library(spdep)
library(ncf)
library(ggplot2)

library(spdep)
library(sf)

# Step 1: Calculate residuals (assuming this part worked fine)
training_data$residuals <- training_data$total_fishing_hours - predict(rf_model_env, training_data)

# Step 2: Prepare spatial data
# Convert to sf object
training_data_sf <- st_as_sf(training_data, coords = c("lon_std", "lat_std"), crs = 4326)

# Create a neighbors list
# We'll use k-nearest neighbors instead of a fixed distance
k <- 8  # You can adjust this number
nb <- knearneigh(st_coordinates(training_data_sf), k = k)
nb <- knn2nb(nb)

# Check for empty neighbor sets
empty_nb <- which(sapply(nb, length) == 0)
if(length(empty_nb) > 0) {
  print(paste("Warning:", length(empty_nb), "points have no neighbors"))
  # Remove points with no neighbors
  training_data_sf <- training_data_sf[-empty_nb,]
  nb <- nb[-empty_nb]
}

# Create a spatial weights matrix
lw <- nb2listw(nb, style="W")

# Perform Moran's I test
moran_test <- moran.test(training_data_sf$residuals, lw)
print(moran_test)

##------------ Create a non-random test set 
# Prepare the data
data <- combined_data_with_rasters %>%
  filter(has_AIS & has_SAR) %>%
  select(total_fishing_hours, total_presence_score, lon_std, lat_std, dist_shore, dist_ports, bathy) %>%
  na.omit()

# Split the data into training and test sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Train the random forest model
set.seed(123)  # for reproducibility
rf_timing <- system.time({
  rf_model_env <- randomForest(
    total_fishing_hours ~ total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy,
    data = train_data,
    ntree = 500,
    importance = TRUE
  )
})

cat("Random Forest model training time:", rf_timing["elapsed"], "seconds\n")

# Save the new model
rf_model_env_train=rf_model_env
save(rf_model_env_train, file = "rf_model_01deg_with_rasters_train.Rdata")
load("rf_model_01deg_with_rasters_train.Rdata")

# Print model summary
print(rf_model_env_train)

# Print variable importance
print(importance(rf_model_env_train))

# Make predictions on the test set
test_data$predictions <- predict(rf_model_env_train, test_data)

# Calculate residuals
test_data$residuals <- test_data$total_fishing_hours - test_data$predictions

# Prepare spatial data for autocorrelation analysis
test_data_sf <- st_as_sf(test_data, coords = c("lon_std", "lat_std"), crs = 4326)

# Create a neighbors list
k <- 8  # You can adjust this number
nb <- knearneigh(st_coordinates(test_data_sf), k = k)
nb <- knn2nb(nb)

# Check for empty neighbor sets
empty_nb <- which(sapply(nb, length) == 0)
if(length(empty_nb) > 0) {
  print(paste("Warning:", length(empty_nb), "points have no neighbors"))
  # Remove points with no neighbors
  test_data_sf <- test_data_sf[-empty_nb,]
  nb <- nb[-empty_nb]
}

# Create a spatial weights matrix
lw <- nb2listw(nb, style="W")

# Perform Moran's I test on the test set residuals
moran_test <- moran.test(test_data_sf$residuals, lw)
print(moran_test)

# Create a correlogram
# Sample a subset of points if the dataset is too large
set.seed(123)
sample_size <- min(10000, nrow(test_data_sf))
sample_indices <- sample(1:nrow(test_data_sf), sample_size)

coords <- st_coordinates(test_data_sf)
correlogram <- correlog(coords[sample_indices, 1], coords[sample_indices, 2], 
                        test_data_sf$residuals[sample_indices], 
                        increment = 1, resamp = 100, latlon = TRUE)

# Plot the correlogram
ggplot(data = as.data.frame(correlogram$correlation), 
       aes(x = correlogram$mean.of.class, y = correlogram$correlation)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Distance (km)", y = "Moran's I", 
       title = "Spatial Correlogram of Random Forest Residuals (Test Set)") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 2000))  # Adjust as needed

ggsave("spatial_correlogram_test_set.png", width = 10, height = 6)

# Calculate and print model performance metrics
rmse <- sqrt(mean((test_data$total_fishing_hours - test_data$predictions)^2))
mae <- mean(abs(test_data$total_fishing_hours - test_data$predictions))
r_squared <- 1 - sum((test_data$total_fishing_hours - test_data$predictions)^2) / 
  sum((test_data$total_fishing_hours - mean(test_data$total_fishing_hours))^2)

print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

# Save the model if needed
save(rf_model_env_train, file = "rf_model_01deg_with_rasters.Rdata")