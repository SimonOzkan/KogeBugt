#---------------------- Militær områder (polygonie) -----------------
# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

## ------------------------------------------------------------------
## 1 Indlæs militærområder (se data folder / metadata_log)
## ------------------------------------------------------------------

mili <- st_read(file.path(PATHS$input_pressure,
                          "military",
                          "EMODnet_HA_MilitaryAreas_20250314",
                          "EMODnet_HA_MilitaryAreas_pg_20250314.shp")) %>%
  st_transform(., crs = target_crs) %>%
  st_make_valid() %>%
  dplyr::distinct(.)


## ------------------------------------------------------------------
## 2 Intersection med grid
## ------------------------------------------------------------------

mili_oresund <- st_intersection(mili, grid) %>%
  mutate(area_mili = st_area(.)) 


mili_area <- mili_oresund %>%
  mutate(value = as.numeric(area_mili) / as.numeric(area_grid),
         value = pmin(value, 1)) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

mili_rast <- terra::rasterize(
  terra::vect(mili_area),
  r_template,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

plot(mili_rast)

terra::writeRaster(
  mili_rast,
  filename  = file.path(PATHS$output_pressure_tif, "\\militaeromraader.tif"),
  overwrite = TRUE
)


## ------------------------------------------------------------------
## 4  Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)


map_mili <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = mili_area, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_mili

ggsave(plot = map_mili,
       filename = file.path(PATHS$output_pressure_png, "anlaeg","militaeromraader.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
