library(dplyr)
library(ggplot2)
library(scales)
library(purrr)

# Function to standardize coordinates to 0.1 degree resolution
standardize_coords <- function(lon, lat) {
  list(
    lon_std = floor(lon * 10) / 10,
    lat_std = floor(lat * 10) / 10
  )
}

# Standardize and aggregate AIS data
AIS_data_std <- aggregated_AIS_fishing %>%
  mutate(coords = map2(cell_ll_lon, cell_ll_lat, standardize_coords)) %>%
  mutate(
    lon_std = map_dbl(coords, ~ .x$lon_std),
    lat_std = map_dbl(coords, ~ .x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_fishing_hours = sum(total_fishing_hours, na.rm = TRUE), .groups = "drop")

head(AIS_data_std)
summary(AIS_data_std$total_fishing_hours)

# Create the plot
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long = long, lat = lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = AIS_data_std, 
            aes(x = lon_std, y = lat_std, fill = total_fishing_hours)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "AIS Fishing Effort (hours; 2017-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000),
    labels = scales::comma,
    guide = guide_colorbar(barwidth = 20, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = NULL,
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Standardize and aggregate SAR data
SAR_data_std <- aggregated_SAR_fishing %>%
  mutate(coords = map2(lon_rounded, lat_rounded, standardize_coords)) %>%
  mutate(
    lon_std = map_dbl(coords, ~ .x$lon_std),
    lat_std = map_dbl(coords, ~ .x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_presence_score = sum(total_presence_score, na.rm = TRUE), .groups = "drop")

summary(SAR_data_std$total_presence_score)

# Create the plot
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long = long, lat = lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = SAR_data_std, 
            aes(x = lon_std, y = lat_std, fill = total_presence_score)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "Fishing vessel detections (2017-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000),
    labels = scales::comma,
    guide = guide_colorbar(barwidth = 20, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = NULL,
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Merge the datasets
combined_data <- full_join(
  AIS_data_std,
  SAR_data_std,
  by = c("lon_std", "lat_std")
)

# Categorize each cell
combined_data <- combined_data %>%
  mutate(category = case_when(
    total_fishing_hours > 0 & total_presence_score > 0 ~ "Both AIS and SAR",
    total_fishing_hours > 0 & (is.na(total_presence_score) | total_presence_score == 0) ~ "Only AIS",
    (is.na(total_fishing_hours) | total_fishing_hours == 0) & total_presence_score > 0 ~ "Only SAR",
    TRUE ~ "No fishing detected"
  ))

# Create the plot
world_plot <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long = long, lat = lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = combined_data, 
            aes(x = lon_std, y = lat_std, fill = category)) +
  scale_fill_manual(
    values = c("Both AIS and SAR" = "purple", 
               "Only AIS" = "blue", 
               "Only SAR" = "red", 
               "No fishing detected" = "white"),
    name = "Fishing Data Source",
    guide = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Global Fishing Detection",
       subtitle = "Comparison of AIS (2017-2020) and SAR (2017-2020) data at 0.1-degree resolution",
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Display the plot
print(world_plot)

# Get summary statistics
summary_stats <- combined_data %>% 
  count(category) %>% 
  mutate(percentage = n / sum(n) * 100)

print(summary_stats)

#---- Now 1-degree resolution only 
library(dplyr)
library(ggplot2)
library(scales)
library(purrr)

# Function to standardize coordinates to 1 degree resolution
standardize_coords <- function(lon, lat) {
  list(
    lon_std = floor(lon),
    lat_std = floor(lat)
  )
}

# Standardize and aggregate AIS data
AIS_data_std <- aggregated_AIS_fishing %>%
  mutate(coords = map2(cell_ll_lon, cell_ll_lat, standardize_coords)) %>%
  mutate(
    lon_std = map_dbl(coords, ~ .x$lon_std),
    lat_std = map_dbl(coords, ~ .x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_fishing_hours = sum(total_fishing_hours, na.rm = TRUE), .groups = "drop")

head(AIS_data_std)
summary(AIS_data_std$total_fishing_hours)

# Create the plot
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long = long, lat = lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = AIS_data_std, 
            aes(x = lon_std, y = lat_std, fill = total_fishing_hours)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "AIS Fishing Effort (hours; 2017-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = scales::comma,
    guide = guide_colorbar(barwidth = 20, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = NULL,
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Standardize and aggregate SAR data
SAR_data_std <- aggregated_SAR_fishing %>%
  mutate(coords = map2(lon_rounded, lat_rounded, standardize_coords)) %>%
  mutate(
    lon_std = map_dbl(coords, ~ .x$lon_std),
    lat_std = map_dbl(coords, ~ .x$lat_std)
  ) %>%
  group_by(lon_std, lat_std) %>%
  summarise(total_presence_score = sum(total_presence_score, na.rm = TRUE), .groups = "drop")

summary(SAR_data_std$total_presence_score)

# Create the plot
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long = long, lat = lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = SAR_data_std, 
            aes(x = lon_std, y = lat_std, fill = total_presence_score)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "Fishing vessel detections (2017-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000),
    labels = scales::comma,
    guide = guide_colorbar(barwidth = 20, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = NULL,
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Merge the datasets
combined_data <- full_join(
  AIS_data_std,
  SAR_data_std,
  by = c("lon_std", "lat_std")
)

# Categorize each cell
combined_data <- combined_data %>%
  mutate(category = case_when(
    total_fishing_hours > 0 & total_presence_score > 0 ~ "Both AIS and SAR",
    total_fishing_hours > 0 & (is.na(total_presence_score) | total_presence_score == 0) ~ "Only AIS",
    (is.na(total_fishing_hours) | total_fishing_hours == 0) & total_presence_score > 0 ~ "Only SAR",
    TRUE ~ "No fishing detected"
  ))

# Create the plot
world_plot <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = combined_data, 
            aes(x = lon_std, y = lat_std, fill = category)) +
  scale_fill_manual(
    values = c("Both AIS and SAR" = "purple", 
               "Only AIS" = "blue", 
               "Only SAR" = "red", 
               "No fishing detected" = "white"),
    name = "Fishing Data Source",
    guide = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Global Fishing Detection",
       subtitle = "Comparison of AIS (2017-2020) and SAR (2017-2020) data at 1-degree resolution",
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = ggplot2::margin(b = 10))
  )

# Display the plot
print(world_plot)

# Get summary statistics
summary_stats <- combined_data %>% 
  count(category) %>% 
  mutate(percentage = n / sum(n) * 100)

print(summary_stats)


