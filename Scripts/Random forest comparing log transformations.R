
library(randomForest)
library(dplyr)
library(pdp)
library(ggplot2)
library(gridExtra)

# Prepare the data
combined_data_1deg <- combined_data_1deg %>%
  mutate(
    log_total_presence_score = log10(total_presence_score + 1),
    log_total_fishing_hours = log10(total_fishing_hours + 1)
  )

# Model 1: Original model (no transformations)
rf_model_no_transform <- randomForest(
  total_fishing_hours ~ total_presence_score + lon_std + lat_std,
  data = combined_data_1deg %>% filter(has_AIS & has_SAR),
  ntree = 500
)

# Model 2: Log-transformed presence score, original fishing hours
rf_model_original <- randomForest(
  total_fishing_hours ~ log_total_presence_score + lon_std + lat_std,
  data = combined_data_1deg %>% filter(has_AIS & has_SAR),
  ntree = 500
)

# Model 3: Log-transformed presence score and fishing hours
rf_model_log <- randomForest(
  log_total_fishing_hours ~ log_total_presence_score + lon_std + lat_std,
  data = combined_data_1deg %>% filter(has_AIS & has_SAR),
  ntree = 500
)

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
results_no_transform <- evaluate_model(rf_model_no_transform, validation_data)
results_original <- evaluate_model(rf_model_original, validation_data)
results_log <- evaluate_model(rf_model_log, validation_data, log_target = TRUE)

# Print results
print("No Transformation Model:")
print(results_no_transform)
print("Presence Score Log-Transformed Model:")
print(results_original)
print("Both Log-Transformed Model:")
print(results_log)

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
p1 <- create_pdp(rf_model_no_transform, validation_data, "total_presence_score", "No Transform")
p2 <- create_pdp(rf_model_no_transform, validation_data, "lon_std", "No Transform")
p3 <- create_pdp(rf_model_no_transform, validation_data, "lat_std", "No Transform")

# Create plots for presence score log-transformed model
p4 <- create_pdp(rf_model_original, validation_data, "log_total_presence_score", "Presence Log")
p5 <- create_pdp(rf_model_original, validation_data, "lon_std", "Presence Log")
p6 <- create_pdp(rf_model_original, validation_data, "lat_std", "Presence Log")

# Create plots for both log-transformed model
p7 <- create_pdp(rf_model_log, validation_data, "log_total_presence_score", "Both Log")
p8 <- create_pdp(rf_model_log, validation_data, "lon_std", "Both Log")
p9 <- create_pdp(rf_model_log, validation_data, "lat_std", "Both Log")

# Combine all plots
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)
