# 1. INSTALL & LOAD PACKAGES
libs <- c("sf", "R.utils", "deckgl", "htmlwidgets")
installed_libraries <- libs %in% rownames(installed.packages())
if (any(!installed_libraries)) install.packages(libs[!installed_libraries])
invisible(lapply(libs, library, character.only = TRUE))

# 2. SET DOWNLOAD FOLDER
download_dir <- "C:/Users/Admin/Documents/sem-8/GIS/3d_population"
dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)

# 3. DOWNLOAD & EXTRACT POPULATION DATA FOR INDIA
options(timeout = 300)
url <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_IN_20231101.gpkg.gz"
filename <- basename(url)
destfile <- file.path(download_dir, filename)
download.file(url, destfile = destfile, mode = "wb")
R.utils::gunzip(destfile, remove = FALSE)
gpkg_file <- gsub(".gz", "", destfile)

# 4. LOAD AND FILTER MUMBAI REGION
pop_sf <- sf::st_read(gpkg_file) |> sf::st_make_valid() |> sf::st_transform(4326)
mumbai_bbox <- sf::st_bbox(c(xmin = 72.75, xmax = 73.05, ymin = 18.85, ymax = 19.35), crs = sf::st_crs(4326))
mumbai_poly <- sf::st_as_sfc(mumbai_bbox)
mumbai_sf <- pop_sf[sf::st_intersects(pop_sf, mumbai_poly, sparse = FALSE), ]

# 5. EXTRACT CENTROIDS FOR HEXBIN LAYER
centroids <- sf::st_centroid(mumbai_sf)
coords <- sf::st_coordinates(centroids)
population <- mumbai_sf$population

# 6. COMBINE INTO DATAFRAME FOR HEXBIN LAYER
hex_data <- data.frame(
  lon = coords[, 1],
  lat = coords[, 2],
  population = population
)

# 7. CREATE HEXAGON LAYER MAP 
color_range <- c(
  "#FF69B4",  # Neon Pink
  "#00FFFF",  # Neon Blue / Aqua
  "#8A2BE2",  # Neon Purple (BlueViolet)
  "#C8A2C8",  # Neon Lilac (Bright lilac variant)
  "#DA70D6"   # Orchid (Pastel-neon mix)
)
map <- deckgl::deckgl(
  latitude = 19.0760,
  longitude = 72.8777,
  zoom = 10,
  pitch = 45
) |>
  deckgl::add_hexagon_layer(
    data = hex_data,
    get_position = ~lon + lat,
    autoHighlight = TRUE,
    elevationScale = 5,  # Lower height
    extruded = TRUE,
    radius = 500,
    getElevationWeight = ~population,
    elevationAggregation = "SUM",
    getColorWeight = ~population,
    colorAggregation = "SUM",
    colorRange = color_range,
    coverage = 0.95,         # 🔲 Shrinks hex slightly to reveal space between
    opacity = 0.9,           # ✨ Makes color a bit transparent so gaps look like borders
    pickable = TRUE,
    tooltip = "Population: {{population}}"
  ) |>
  deckgl::add_basemap(deckgl::use_carto_style("dark-matter"))



# 8. SAVE TO HTML
output_html <- file.path(download_dir, "mumbai_population_hex3D.html")
htmlwidgets::saveWidget(map, file = output_html, selfcontained = FALSE)
browseURL(output_html)
