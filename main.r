# 1. Load necessary libraries
library(terra)
library(sf)
library(elevatr)
library(rchelsa)
library(biscale)
library(ggplot2)
library(cowplot)
library(rayshader)

# 2. Set the working directory (adjust as needed)
main_dir <- getwd()

# 3. Define a vector of IDs to download (CHELSA temperature and precipitation data)
ids <- c(1, 12)  # Temperature and Precipitation layers

# Download the CHELSA data for temperature and precipitation
download_chelsa_data <- function(id, path){
  rchelsa::get_chelsea_data(
    categ = "clim", type = "bio",
    id = id, path = path
  )
}
lapply(ids, download_chelsa_data, path = main_dir)

# List files to check for downloaded data
list.files()

# Load the downloaded raster files (temperature and precipitation)
temp <- terra::rast("CHELSA_bio10_01.tif")  # Temperature data
prec <- terra::rast("CHELSA_bio10_12.tif")  # Precipitation data

# Average precipitation (for the monthly data)
prec_average <- prec / 30

# Combine temperature and precipitation into a raster stack
temp_prec <- c(temp, prec_average)

# Assign names to each layer in the stack
names(temp_prec) <- c("temperature", "precipitation")

# 4. Get India shapefile (adjust file path to the shapefile)
country_sf <- st_read("C:/Users/Admin/Documents/IND_adm/IND_adm1.shp")  # Corrected file path

# Convert the 'sf' object to 'SpatVector' for compatibility with terra
country_vect <- terra::vect(country_sf)

# 5. Obtain AWS tiles DEM data from elevatr and crop to India extent
dem <- elevatr::get_elev_raster(
  locations = country_sf, z = 8,
  clip = "locations"
)

# Convert the DEM to SpatRaster format
dem_raster <- terra::rast(dem)

# Ensure both raster and vector have the same CRS
if (terra::crs(dem_raster) != terra::crs(country_vect)) {
  country_vect <- terra::project(country_vect, crs(dem_raster))
}

# Crop the DEM raster to India using the vector object (India boundaries)
dem_cropped <- terra::crop(dem_raster, country_vect, mask = TRUE)

# 6. Resample temperature and precipitation to match DEM resolution
temp_prec_resampled <- terra::resample(
  x = temp_prec,
  y = dem_cropped, method = "bilinear"
)

# Plot the resampled raster (temperature and precipitation)
terra::plot(temp_prec_resampled)

# 7. Convert the resampled raster to a dataframe with coordinates for plotting
temp_prec_df <- as.data.frame(
  temp_prec_resampled, xy = TRUE
)

# 8. Create breaks for the bivariate map (temperature and precipitation)
breaks <- biscale::bi_class(
  temp_prec_df, x = temperature,
  y = precipitation, style = "fisher",
  dim = 3
)

# Define the color palette
pal <- "DkBlue"

# Define a custom theme for the map
theme_for_the_win <- function(){
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      plot.background = element_rect(
        fill = "white", color = NA
      ),
      plot.title = element_text(
        color = "grey10", hjust = .5,
        face = "bold", vjust = -1
      ),
      plot.subtitle = element_text(
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text(
        size = 9, color = "grey20",
        hjust = .5, vjust = 1
      ),
      plot.margin = unit(c(0, 0, 0, 0), "lines"
      )
    )
}

# 9. Create 2D bivariate map using ggplot2
map <- ggplot(breaks) +
  geom_raster(
    aes(
      x = x, y = y, fill = bi_class
    ), show.legend = TRUE
  ) +
  biscale::bi_scale_fill(
    pal = pal, dim = 3,
    flip_axes = TRUE, rotate_pal = FALSE
  ) +
  labs(
    title = "India: Temperature and Precipitation",
    subtitle = "Average temperature and precipitation (1981-2010)",
    caption = "Source: CHELSA | Author: Your Name",
    x = "", y = ""
  ) +
  coord_sf(crs = terra::crs(dem_cropped)) +
  theme_for_the_win()

# Create the legend for the bivariate map
legend <- biscale::bi_legend(
  pal = pal,
  flip_axes = TRUE,
  rotate_pal = FALSE,
  dim = 3,
  xlab = "Temperature (°C)",
  ylab = "Precipitation (mm)",
  size = 8
)

# Combine the map and legend using cowplot
full_map <- cowplot::ggdraw() +
  cowplot::draw_plot(
    plot = map, x = 0, y = 0,
    width = 1, height = 1
  ) +
  cowplot::draw_plot(
    plot = legend, x = .05, y = .13,
    width = .25, height = .25
  )

# Display the final map with legend
print(full_map)

# Save as PNG file
ggsave(
  filename = "india_bivariate_2d.png",
  width = 7, height = 7, dpi = 600,
  device = "png", bg = "white", full_map
)

# 10. Create terrain layer using DEM data
dem_df <- dem_cropped |>
  as.data.frame(xy = TRUE, na.rm = TRUE)
names(dem_df)[3] <- "dem"

# Create the terrain layer map
dem_map_simple <- ggplot(dem_df, aes(x = x, y = y, fill = dem)) +
  geom_raster() +
  scale_fill_gradientn(colors = "white") +
  guides(fill = "none") +
  coord_sf(crs = terra::crs(dem_cropped)) +
  theme_for_the_win() +
  theme(legend.position = "none")

rayshader::plot_gg(
  ggobj = dem_map_simple,
  width = 5,
  height = 5,
  windowsize = c(600, 600),
  scale = 100,
  shadow = TRUE,
  shadow_intensity = 1,
  phi = 87, theta = 0, zoom = .56,
  multicore = TRUE
)

# 12. Zoom Out for Better Perspective
rayshader::render_camera(zoom = .6)

# 13. Download HDRI Lighting File
url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/brown_photostudio_02_4k.hdr"
hdri_file <- basename(url)

download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb"
)

# 14. Render the 3D Object with HDRI Lighting
rayshader::render_highquality(
  filename = "india_bivariate_3d.png",
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity = 1,
  rotate_env = 90,
  parallel = TRUE,
  width = 2000, height = 2000,
  interactive = FALSE
)
