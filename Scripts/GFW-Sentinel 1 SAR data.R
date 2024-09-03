# Load required libraries
library(tidyverse)

# Set the path to the 2016 folder
path <- "Data/SAR Vessel detections 2017-2020"

# List all CSV files in the folder
SAR_csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

# Read all CSV files and combine them into a single data frame
SAR_fishing <- SAR_csv_files %>%
  map_df(~read_csv(.))

save(SAR_fishing, file="Data/SAR_fishing.Rdata")

# Print the first few rows and basic information about the combined data frame
print(head(SAR_fishing))
print(str(SAR_fishing))

print(summary(SAR_fishing$matching_score))
print(summary(as.factor(SAR_fishing$matched_category)))

# Load required libraries if not already loaded
library(maps)
library(viridis)

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
  # Remove any cells with zero or negative fishing hours

# Print the first few rows and basic information about the combined data frame
print(head(aggregated_SAR_fishing))
print(str(aggregated_SAR_fishing))

# Check the range of our data
print(summary(aggregated_SAR_fishing$total_presence_score))

# Create the world map
world_map <- map_data("world")

# Create the plot
library(scales)

ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = aggregated_SAR_fishing, 
            aes(x = lon_rounded, y = lat_rounded, fill = total_presence_score)) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log",
    breaks = c(1, 2, 5, 10, 25, 50, 100, 150),
    labels = scales::comma,
    limits = c(1, 151),
    name = "Fishing vessel detections (2017-2020)", 
    guide = guide_colorbar(barwidth = 15, barheight = 0.5, title.position = "top", title.hjust = 0.5)
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = NULL,
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.margin = margin(t = 20, r = 0, b = 0, l = 0),
    legend.title = element_text(margin = margin(b = 10))
  )

ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_point(data = aggregated_SAR_fishing, 
             aes(x = lon_rounded, y = lat_rounded),
             color = "red", shape = 15, size = 0.1) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = NULL,
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "gray80", size = 0.1),
    panel.grid.minor = element_blank()
  )

