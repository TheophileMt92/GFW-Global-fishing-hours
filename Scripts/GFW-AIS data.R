# Load required libraries
library(tidyverse)

# Set the path to the 2016 folder
path <- "Data/AIS Fishing Effort 2017-2020"

# List all CSV files in the folder
AIS_csv_files <- list.files(path = path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Read all CSV files and combine them into a single data frame
AIS_fishing <- AIS_csv_files %>%
  map_df(~read_csv(.))

# Print the first few rows and basic information about the combined data frame
print(head(AIS_fishing))
print(str(AIS_fishing))

# Load required libraries if not already loaded
library(maps)
library(viridis)

# Aggregate fishing hours by latitude and longitude
aggregated_AIS_fishing <- AIS_fishing %>%
  group_by(cell_ll_lat, cell_ll_lon) %>%
  summarise(total_fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>%
  ungroup() %>%
  # Remove any cells with zero or negative fishing hours
  filter(total_fishing_hours > 0)

# Check the range of our data
print(summary(aggregated_AIS_fishing$total_fishing_hours))

# Create the world map
world_map <- map_data("world")

# Create the plot
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  geom_tile(data = aggregated_AIS_fishing, 
            aes(x = cell_ll_lon, y = cell_ll_lat, fill = total_fishing_hours)) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log1p",
    name = "AIS Fishing Effort (2016-2020)", 
    breaks = c(0, 1, 10, 100, 1000, 10000),
    labels = scales::comma,
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

