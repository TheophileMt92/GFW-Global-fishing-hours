library(ncdf4)
library(here)
library(raster)
library(ggplot2)

# List all .nc files in the specified directory
nc_files <- list.files(here::here("Data", "Environmental data layers", "SST Copernicus"),
                       pattern = "\\.nc$", full.names = TRUE)

# Initialize a list to store SST data
all_sst_data <- list()

# Loop through each .nc file to extract SST data
for (file in nc_files) {
  cat("Processing file:", file, "\n")
  
  # Open the NetCDF file
  nc_data <- nc_open(file)
  
  # Extract the analysed sea surface temperature data
  sst_data <- ncvar_get(nc_data, "analysed_sst")
  
  # Convert from Kelvin to Celsius
  sst_data_celsius <- sst_data - 273.15
  
  # Get latitude and longitude for mapping
  lat <- ncvar_get(nc_data, "lat")
  lon <- ncvar_get(nc_data, "lon")
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Store the data in a list
  all_sst_data[[file]] <- sst_data_celsius
}

# Average SST across all files
avg_sst <- Reduce("+", all_sst_data) / length(all_sst_data)

# Create a raster object from the averaged SST data
avg_sst_raster <- raster(t(avg_sst), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
# Flip the raster to ensure correct orientation
avg_sst_raster <- flip(avg_sst_raster, direction = 'y')

# Save the raster file
output_file <- here::here("Data", "Environmental data layers", "Average_SST_2021.tif")
writeRaster(avg_sst_raster, filename = output_file, format = "GTiff", overwrite = TRUE)

# Prepare data for plotting
lat_lon_grid <- expand.grid(lon = lon, lat = lat)
lat_lon_grid$avg_sst <- as.vector(avg_sst)  # Flatten the averaged SST data into a vector

# Plot the averaged SST data
ggplot(lat_lon_grid, aes(x = lon, y = lat, fill = avg_sst)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma", name = "Average Temperature (°C)") +
  labs(title = "Average Analysed Sea Surface Temperature",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  coord_fixed()  # Keep aspect ratio
