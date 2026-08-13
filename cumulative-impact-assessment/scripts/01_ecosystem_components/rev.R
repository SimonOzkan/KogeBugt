#---------------------- Rev -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs
target_crs <- 25832

# Indlæs grid og undersøgelsesområde fra de forskellige project paths

grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(., crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = st_area(.)) %>%
  st_drop_geometry(.)

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)

## Hent Rev-lag fra MST's WFS-service (se data folder / metadata_log)

wfs_url <- "https://gisportal.mst.dk/server/services/ekstern/KDI_anlaeg_paa_soeterritoriet/MapServer/WFSServer"

request <- paste0(
  wfs_url,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=KDI_anlaeg_paa_soeterritoriet:Anlaeg_paa_soeterritoriet__polygon_",
  "&outputFormat=GEOJSON"
)

anlaeg_polygon <- st_read(request) %>%
  st_transform(crs = target_crs)

rev_poly <- anlaeg_polygon %>%
  filter(TYPE == "Rev")


## Intersect med grid
rev_intersect <- st_intersection(rev_poly, grid) %>%
  st_make_valid()

# Find fraktion af hver gridcelle dækket af rev og sæt til value for raster (0-1)
rev_area <- rev_intersect %>%
  mutate(area_rev = st_area(.)) %>%
  left_join(grid_area, by = "id") %>%
  mutate(value = as.numeric(area_rev) / as.numeric(area_grid),
         value = pmin(value, 1))

# Konverter til raster

r_template <- terra::rast(
  extent     = terra::ext(terra::vect(assessment_area_dissolved)),
  resolution = 250,
  crs        = "EPSG:25832"
)

rev_rast <- terra::rasterize(
  terra::vect(rev_area),
  r_template,
  field    = "value",
  fun      = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = NA        # celler udenfor assessment area → NA
)

plot(rev_rast)

terra::writeRaster(
  rev_rast,
  filename = file.path(PATHS$output_ecosystem_tif, "\\rev.tif"),
  overwrite = TRUE
)


############### Plotting for bilag ################


map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 

map_rev <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = rev_area,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_rev

ggsave(plot = map_rev,
       filename = file.path(PATHS$output_ecosystem_png, "\\rev.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
