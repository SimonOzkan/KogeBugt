#---------------------- Militær områder (polygonie) -----------------
# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()

## ------------------------------------------------------------------
## 1 Indlæs militærområder (se data folder / metadata_log)
## ------------------------------------------------------------------

mili <- st_read(file.path(PATHS$input_pressure,
                          "military",
                          "EMODnet_HA_MilitaryAreas_20250314",
                          "EMODnet_HA_MilitaryAreas_pg_20250314.shp")) %>%
  st_transform(., crs = target_crs) %>%
  dplyr::distinct(.) %>%
  st_make_valid() 


## ------------------------------------------------------------------
## 2 Intersection med grid
## ------------------------------------------------------------------

mili_oresund <- st_intersection(grid_sf,mili) %>%
  mutate(area_mili = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_mili = sum(area_mili, na.rm = TRUE), .groups = "drop")


mili_area <- grid_sf %>%
  left_join(mili_oresund) %>%
  mutate(value = tidyr::replace_na(area_mili, 0),
         value = as.numeric(value) / as.numeric(area_grid)) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------


mili_rast <- terra::rasterize(
  terra::vect(mili_area),
  grid_raster,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

plot(mili_rast)

terra::writeRaster(
  mili_rast,
  filename  = file.path(PATHS$output_pressure_tif, "restriktive_omraader","\\militaeromraader.tif"),
  overwrite = TRUE
)


## ------------------------------------------------------------------
## 4  Plotting for bilag
## ------------------------------------------------------------------

mili_rast_sf <- mili_rast %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()



map_mili <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = mili_rast_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_mili

gem_trim(map_mili,
         file.path(PATHS$output_pressure_png, "restriktive_omraader\\militaer.png"),
         dpi = 360, width_cm = 18)
