library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(gridExtra)
#---- Open the AIS data 

# Set the path to the folder
#path <- "Data/AIS Fishing Effort 2017-2020"

# List all CSV files in the folder
#AIS_csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Read all CSV files and combine them into a single data frame
#AIS_fishing <- AIS_csv_files %>%
#  map_df(~read_csv(.))

load(here::here("Data", "AIS_fishing.Rdata"))

# Aggregate fishing hours by latitude and longitude
aggregated_AIS_fishing <- AIS_fishing %>%
  group_by(cell_ll_lat, cell_ll_lon) %>%
  summarise(total_fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>%
  ungroup() %>%
  # Remove any cells with zero or negative fishing hours
  filter(total_fishing_hours > 0)

#---- Open the SAR data 

# Set the path to the 2016 folder
#path <- "Data/SAR Vessel detections 2017-2020"

# List all CSV files in the folder
#SAR_csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

# Read all CSV files and combine them into a single data frame
#SAR_fishing <- SAR_csv_files %>%
#  map_df(~read_csv(.))

load(here::here("Data", "SAR_fishing.Rdata"))

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
  rf_model_01deg <- randomForest(
    total_fishing_hours ~ total_presence_score + lon_std + lat_std,
    data = training_data,
    ntree = 500,
    importance = TRUE
  )
})

# Print the time taken
print(paste("Random Forest model training time:", rf_timing["elapsed"], "seconds"))

save(rf_model_01deg,file="rf_model_01deg.Rdata")
load("rf_model_01deg.Rdata")

#Visualise results 
# Make predictions
predictions <- predict(rf_model, newdata = prediction_data)

# Add predictions to the original dataset
combined_data_01deg <- combined_data_01deg %>%
  mutate(
    predicted_fishing_hours = case_when(
      has_AIS ~ total_fishing_hours,
      has_SAR ~ predict(rf_model, newdata = select(., total_presence_score, lon_std, lat_std)),
      TRUE ~ 0
    )
  )

# Create the world map
world_map <- map_data("world")

#Map of predicted fishing hours only 
# Prepare the data for the map
map_data <- combined_data_01deg %>%
  filter(!has_AIS & has_SAR) %>%
  select(lon_std, lat_std, predicted_fishing_hours)

# Create the map
predicted_SAR_only_plot <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "darkgray", fill = "lightgray", size = 0.1) +
  geom_tile(data = map_data, 
            aes(x = lon_std, y = lat_std, fill = predicted_fishing_hours)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "Predicted fishing hours (2017-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = scales::comma,
    guide = guide_colorbar(barwidth = 20, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Predicted Fishing Hours in Areas with Only SAR Detections",
       subtitle = "0.1 degree resolution",
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Print the map
print(predicted_SAR_only_plot)

#Map of both original and predicted AIS fishing hours 
# Visualize the results
predicted_plot <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = combined_data_01deg, 
            aes(x = lon_std, y = lat_std, fill = predicted_fishing_hours)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "Predicted fishing hours (2017-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = scales::comma,
    guide = guide_colorbar(barwidth = 20, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Global Predicted Fishing Hours (0.1 degree resolution)",
       subtitle = "Based on AIS data and Random Forest predictions from SAR data",
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

print(predicted_plot)

#Aggregate data to 1 degree resolution 
# First, round the coordinates to the nearest degree
combined_data_1deg <- combined_data_01deg %>%
  mutate(
    lon_1deg = round(lon_std),
    lat_1deg = round(lat_std)
  )

# Aggregate the data
aggregated_data <- combined_data_1deg %>%
  group_by(lon_1deg, lat_1deg) %>%
  summarise(
    predicted_fishing_hours = sum(predicted_fishing_hours, na.rm = TRUE),
    .groups = 'drop'
  )

# Create the world map
world_map <- map_data("world")

# Create the map
predicted_plot_1deg <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = aggregated_data, 
            aes(x = lon_1deg, y = lat_1deg, fill = predicted_fishing_hours)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "Predicted fishing hours (2017-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = scales::comma,
    guide = guide_colorbar(barwidth = 20, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Global Predicted Fishing Hours (1 degree resolution)",
       subtitle = "Based on AIS data and Random Forest predictions from SAR data made at 0.1 degree resolution",
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Print the map
print(predicted_plot_1deg)

# Evaluate the model
# Function to evaluate models
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
  
  # R-squared and Adjusted R-squared
  ss_total <- sum((actual - mean(actual))^2, na.rm = TRUE)
  ss_residual <- sum((actual - predictions)^2, na.rm = TRUE)
  r_squared <- 1 - (ss_residual / ss_total)
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
    MAE = mae,                            # Mean Absolute Error
    RMSE = rmse,                          # Root Mean Squared Error
    MAPE = mape,                          # Mean Absolute Percentage Error
    MedAE = medae,                        # Median Absolute Error
    R_squared = r_squared,                # R-Squared (Coefficient of Determination)
    Adjusted_R_squared = adj_r_squared,   # Adjusted R-Squared
    Mean_Residual = mean_residual,        # Mean of Residuals
    SD_Residual = sd_residual,            # Standard Deviation of Residuals
    Feature_Importance = feature_importance # Feature Importance (for Random Forest)
  ))
}

# Merge the datasets
combined_data_01deg <- full_join(
  AIS_data_01deg,
  SAR_data_01deg,
  by = c("lon_std", "lat_std")
) %>%
  mutate(
    has_AIS = !is.na(total_fishing_hours) & total_fishing_hours > 0,
    has_SAR = !is.na(total_presence_score) & total_presence_score > 0,
    data_category = case_when(
      has_AIS & has_SAR ~ "Both AIS and SAR",
      has_AIS & !has_SAR ~ "Only AIS",
      !has_AIS & has_SAR ~ "Only SAR",
      TRUE ~ "No fishing detected"
    )
  )

# Evaluate all models
validation_data <- combined_data_01deg %>% filter(data_category == "Both AIS and SAR")
results_no_transform <- evaluate_model(rf_model, validation_data)
print(results_no_transform)


# Function to create partial dependence plot
library(pdp)
create_pdp <- function(model, data, feature) {
  pdp_data <- partial(model, pred.var = feature, train = data)
  ggplot() +
    geom_line(data = pdp_data, aes_string(x = feature, y = "yhat")) +
    geom_rug(data = data, aes_string(x = feature), sides = "b", alpha = 0.1) +
    theme_minimal() +
    labs(title = NULL,
         x = feature,
         y = "Partial Dependence")
}

# Create plots
p1 <- create_pdp(rf_model, validation_data, "total_presence_score")
p2 <- create_pdp(rf_model, validation_data, "lon_std")
p3 <- create_pdp(rf_model, validation_data, "lat_std")

# Combine all plots
combined_plot <- grid.arrange(p1, p2, p3, ncol = 3)

# Display the combined plot
print(combined_plot)

#Other model performance indicators 
print(rf_model)

# Variable importance
varImpPlot(rf_model)

#Mean absolute Error (MAE) and Root Mean Square Error (RMSE)
# Make predictions on the validation set
validation_predictions <- predict(rf_model, newdata = validation_data)

# For model evaluation
validation_data <- combined_data_01deg %>% 
  filter(data_category == "Both AIS and SAR")

# For making predictions
prediction_data <- combined_data_01deg %>% 
  filter(data_category == "SAR only")

# Now you can properly evaluate your model
comparison_plot <- ggplot(validation_data, 
                          aes(x = total_fishing_hours, y = model_prediction)) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Validation: Actual vs Predicted Fishing Hours (1 degree resolution)",
       x = "Actual Fishing Hours (AIS)",
       y = "Predicted Fishing Hours")

print(comparison_plot)

# Calculate R-squared
lm_model <- lm(log10(model_prediction) ~ log10(total_fishing_hours), data = validation_data)
r_squared <- summary(lm_model)$r.squared
print(paste("R-squared (log scale):", round(r_squared, 4)))