
library(randomForest)
library(dplyr)
library(pdp)
library(ggplot2)
library(gridExtra)

# Prepare the data
load(here::here("training_data.Rdata"))

training_data_log <- training_data %>%
  mutate(
    log_total_presence_score = log10(total_presence_score + 1),
    log_total_fishing_hours = log10(total_fishing_hours + 1)
  )

library(parallel)
library(randomForest)

# Function to run a single model
run_model <- function(formula, data) {
  randomForest(
    formula,
    data = data,
    ntree = 500,
    importance = TRUE
  )
}

# Set up parallel processing
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)

# Export necessary objects to the cluster
clusterExport(cl, c("training_data_log", "run_model"))

# Load required packages on each cluster
clusterEvalQ(cl, library(randomForest))

# Define the models
models <- list(
  no_transform = as.formula(total_fishing_hours ~ total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy),
  original = as.formula(total_fishing_hours ~ log_total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy),
  log = as.formula(log_total_fishing_hours ~ log_total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy)
)

# Run models in parallel
results <- parLapply(cl, models, function(formula) run_model(formula, training_data_log))

# Stop the cluster
stopCluster(cl)

# Save the models
rf_model_no_transform <- results[[1]]
rf_model_original <- results[[2]]
rf_model_log <- results[[3]]

# Save models to files
saveRDS(rf_model_no_transform, "rf_model_no_transform.rds")
saveRDS(rf_model_original, "rf_model_original.rds")
saveRDS(rf_model_log, "rf_model_log.rds")

# Add the new model with only total_fishing_hours log-transformed
rf_model_fishing_log <- randomForest(
  log_total_fishing_hours ~ total_presence_score + lon_std + lat_std + dist_shore + dist_ports + bathy,
  data = training_data_log,
  ntree = 500,
  importance = TRUE
)

# Save the new model
saveRDS(rf_model_fishing_log, "rf_model_fishing_log.rds")

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

# Evaluate all models
validation_data <- combined_data_with_rasters %>%
  mutate(
    data_category = case_when(
      has_AIS & has_SAR ~ "Both AIS and SAR",
      has_AIS & !has_SAR ~ "Only AIS",
      !has_AIS & has_SAR ~ "Only SAR",
      TRUE ~ "No fishing detected"
    )
  )

validation_data <- validation_data %>% filter(data_category == "Both AIS and SAR")

# Evaluate all models
results_no_transform <- evaluate_model(rf_model_no_transform, validation_data)

validation_data_logpres <- validation_data %>%
  mutate(log_total_presence_score = log10(total_presence_score + 1))

results_original <- evaluate_model(rf_model_original, validation_data_logpres)

# Add evaluation for the new model (fishing hours log-transformed)
results_fishing_log <- evaluate_model(rf_model_fishing_log, validation_data, log_target = TRUE)

results_log <- evaluate_model(rf_model_log, validation_data_logpres, log_target = TRUE)

# Print results
print("No Transformation Model:")
print(results_no_transform)
print("Fishing Hours Log-Transformed Model:")
print(results_fishing_log)
print("Presence Score Log-Transformed Model:")
print(results_original)
print("Both Log-Transformed Model:")
print(results_log)

library(knitr)
library(kableExtra)

library(knitr)
library(kableExtra)

# Create a data frame with the results
results_df <- data.frame(
  Metric = c("Mean Absolute Error", "Root Mean Squared Error", "Mean Absolute Percentage Error", "Median Absolute Error", "R-Squared", "Adjusted R-Squared", "Mean of Residuals", "Standard Deviation of Residuals"),
  No_Transform = c(results_no_transform$`Mean Absolute Error`, results_no_transform$`Root Mean Squared Error`, results_no_transform$`Mean Absolute Percentage Error`, results_no_transform$`Median Absolute Error`, results_no_transform$`R-Squared`, results_no_transform$`Adjusted R-Squared`, results_no_transform$`Mean of Residuals`, results_no_transform$`Standard Deviation of Residuals`),
  Fishing_Log = c(results_fishing_log$`Mean Absolute Error`, results_fishing_log$`Root Mean Squared Error`, results_fishing_log$`Mean Absolute Percentage Error`, results_fishing_log$`Median Absolute Error`, results_fishing_log$`R-Squared`, results_fishing_log$`Adjusted R-Squared`, results_fishing_log$`Mean of Residuals`, results_fishing_log$`Standard Deviation of Residuals`),
  Presence_Log = c(results_original$`Mean Absolute Error`, results_original$`Root Mean Squared Error`, results_original$`Mean Absolute Percentage Error`, results_original$`Median Absolute Error`, results_original$`R-Squared`, results_original$`Adjusted R-Squared`, results_original$`Mean of Residuals`, results_original$`Standard Deviation of Residuals`),
  Both_Log = c(results_log$`Mean Absolute Error`, results_log$`Root Mean Squared Error`, results_log$`Mean Absolute Percentage Error`, results_log$`Median Absolute Error`, results_log$`R-Squared`, results_log$`Adjusted R-Squared`, results_log$`Mean of Residuals`, results_log$`Standard Deviation of Residuals`)
)

# Create the kable
kable(results_df, format = "html", digits = 1,
      col.names = c("Metric", "No Transform", "Fishing Hours Log", "Presence Score Log", "Both Log"),
      caption = "Model Performance Comparison") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Models" = 4)) %>%
  column_spec(1, bold = TRUE)

library(parallel)
library(randomForest)
library(pdp)
library(ggplot2)
library(gridExtra)

# Function to create partial dependence plot
create_pdp <- function(model, data, feature, model_name) {
  pdp_data <- partial(model, pred.var = feature, train = data)
  plot <- ggplot(pdp_data, aes_string(x = feature, y = "yhat")) +
    geom_line() +
    theme_minimal() +
    labs(title = paste(model_name, "-", feature),
         x = feature,
         y = "Partial Dependence")
  return(plot)
}

# Set up parallel processing
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)

# Export necessary objects and functions to the cluster
clusterExport(cl, c("rf_model_no_transform", "rf_model_fishing_log", "rf_model_original", "rf_model_log", 
                    "validation_data", "create_pdp"))

# Load required packages on each cluster
clusterEvalQ(cl, {
  library(randomForest)
  library(pdp)
  library(ggplot2)
})

# Define the plot configurations
plot_configs <- list(
  list(model = rf_model_no_transform, feature = "total_presence_score", name = "No Transform"),
  list(model = rf_model_no_transform, feature = "lon_std", name = "No Transform"),
  list(model = rf_model_no_transform, feature = "lat_std", name = "No Transform"),
  list(model = rf_model_no_transform, feature = "dist_ports", name = "No Transform"),
  list(model = rf_model_no_transform, feature = "dist_shore", name = "No Transform"),
  list(model = rf_model_no_transform, feature = "bathy", name = "No Transform"),
  list(model = rf_model_fishing_log, feature = "total_presence_score", name = "Fishing Log"),
  list(model = rf_model_fishing_log, feature = "lon_std", name = "Fishing Log"),
  list(model = rf_model_fishing_log, feature = "lat_std", name = "Fishing Log"),
  list(model = rf_model_fishing_log, feature = "dist_ports", name = "Fishing Log"),
  list(model = rf_model_fishing_log, feature = "dist_shore", name = "Fishing Log"),
  list(model = rf_model_fishing_log, feature = "bathy", name = "Fishing Log"),
  list(model = rf_model_original, feature = "log_total_presence_score", name = "Presence Log"),
  list(model = rf_model_original, feature = "lon_std", name = "Presence Log"),
  list(model = rf_model_original, feature = "lat_std", name = "Presence Log"),
  list(model = rf_model_original, feature = "dist_ports", name = "Presence Log"),
  list(model = rf_model_original, feature = "dist_shore", name = "Presence Log"),
  list(model = rf_model_original, feature = "bathy", name = "Presence Log"),
  list(model = rf_model_log, feature = "log_total_presence_score", name = "Both Log"),
  list(model = rf_model_log, feature = "lon_std", name = "Both Log"),
  list(model = rf_model_log, feature = "lat_std", name = "Both Log"),
  list(model = rf_model_log, feature = "dist_ports", name = "Both Log"),
  list(model = rf_model_log, feature = "dist_shore", name = "Both Log"),
  list(model = rf_model_log, feature = "bathy", name = "Both Log")
)

# Run plots in parallel
plots <- parLapply(cl, plot_configs, function(config) {
  create_pdp(config$model, validation_data, config$feature, config$name)
})

# Stop the cluster
stopCluster(cl)

# Name the plots
plot_names <- sapply(plot_configs, function(config) {
  paste0("pdp_", gsub(" ", "_", config$name), "_", config$feature)
})
names(plots) <- plot_names

# Save the plot objects
saveRDS(plots, "pdp_plots.rds")

# Combine all plots (optional)
combined_plot <- do.call(grid.arrange, c(plots, list(ncol = 6)))

# Print a message
cat("Individual plot objects have been saved to 'pdp_plots.rds'")