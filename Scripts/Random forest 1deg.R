library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(sf)

# Function to standardize coordinates to 1 degree resolution
standardize_coords <- function(lon, lat) {
  list(
    lon_std = floor(lon),
    lat_std = floor(lat)
  )
}

# Aggregate AIS data to 1 degree resolution
AIS_data_1deg <- aggregated_AIS_fishing %>%
  mutate(coords = purrr::map2(cell_ll_lon, cell_ll_lat, standardize_coords)) %>%
  mutate(
    lon_std = sapply(coords, function(x) x$lon_std),
    lat_std = sapply(coords, function(x) x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_fishing_hours = sum(total_fishing_hours, na.rm = TRUE), .groups = "drop")

# Aggregate SAR data to 1 degree resolution
SAR_data_1deg <- aggregated_SAR_fishing %>%
  mutate(coords = purrr::map2(lon_rounded, lat_rounded, standardize_coords)) %>%
  mutate(
    lon_std = sapply(coords, function(x) x$lon_std),
    lat_std = sapply(coords, function(x) x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_presence_score = sum(total_presence_score, na.rm = TRUE), .groups = "drop")

# Merge the datasets
combined_data_1deg <- full_join(
  AIS_data_1deg,
  SAR_data_1deg,
  by = c("lon_std", "lat_std")
) %>%
  mutate(
    has_AIS = !is.na(total_fishing_hours) & total_fishing_hours > 0,
    has_SAR = !is.na(total_presence_score) & total_presence_score > 0
  )

# Separate the data into training (both AIS and SAR) and prediction (only SAR) sets
training_data <- combined_data_1deg %>%
  filter(has_AIS & has_SAR) %>%
  select(total_fishing_hours, total_presence_score, lon_std, lat_std)

prediction_data <- combined_data_1deg %>%
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

#Visualise results 
# Make predictions
predictions <- predict(rf_model, newdata = prediction_data)

# Add predictions to the original dataset
combined_data_1deg <- combined_data_1deg %>%
  mutate(
    predicted_fishing_hours = case_when(
      has_AIS ~ total_fishing_hours,
      has_SAR ~ predict(rf_model, newdata = select(., total_presence_score, lon_std, lat_std)),
      TRUE ~ 0
    )
  )

# Visualize the results
predicted_plot <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = combined_data_1deg, 
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
  labs(title = "Global Predicted Fishing Hours (1 degree resolution)",
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

#Map of predicted fishing hours only 
# Prepare the data for the map
map_data <- combined_data_1deg %>%
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
       subtitle = "1 degree resolution",
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

# Print summary statistics
summary_stats <- map_data %>%
  summarise(
    total_cells = n(),
    min_hours = min(predicted_fishing_hours),
    max_hours = max(predicted_fishing_hours),
    mean_hours = mean(predicted_fishing_hours),
    median_hours = median(predicted_fishing_hours)
  )

print(summary_stats)

# Evaluate the model
# Function to evaluate models
evaluate_model <- function(model, data, log_target = FALSE) {
  predictions <- predict(model, newdata = data)
  if (log_target) {
    predictions <- 10^predictions - 1
  }
  
  actual <- data$total_fishing_hours
  
  mae <- mean(abs(actual - predictions), na.rm = TRUE)
  rmse <- sqrt(mean((actual - predictions)^2, na.rm = TRUE))
  
  # Correct R-squared calculation
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predictions)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(list(MAE = mae, RMSE = rmse, R_squared = r_squared))
}

# Evaluate all models
validation_data <- combined_data_1deg %>% filter(data_category == "Both AIS and SAR")
results_no_transform <- evaluate_model(rf_model, validation_data)
print(results_no_transform)

# Function to create partial dependence plot
create_pdp <- function(model, data, feature, model_name) {
  pdp_data <- partial(model, pred.var = feature, train = data)
  ggplot(pdp_data, aes_string(x = feature, y = "yhat")) +
    geom_line() +
    theme_minimal() +
    labs(title = paste(model_name, "-", feature),
         x = feature,
         y = "Partial Dependence")
}

# Create plots for no transformation model
p1 <- create_pdp(rf_model, validation_data, "total_presence_score", "No Transform")
p2 <- create_pdp(rf_model, validation_data, "lon_std", "No Transform")
p3 <- create_pdp(rf_model, validation_data, "lat_std", "No Transform")

# Combine all plots
grid.arrange(p1, p2, p3, ncol = 3)

#Other model performance indicators 
print(rf_model)

# Variable importance
varImpPlot(rf_model)

#Mean absolute Error (MAE) and Root Mean Square Error (RMSE)
# Make predictions on the validation set
validation_predictions <- predict(rf_model, newdata = validation_data)

# For model evaluation
validation_data <- combined_data_1deg %>% 
  filter(data_category == "Both AIS and SAR")

# For making predictions
prediction_data <- combined_data_1deg %>% 
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



