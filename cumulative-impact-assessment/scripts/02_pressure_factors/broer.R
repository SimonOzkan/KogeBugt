#---------------------- Broer (buffer / fraktion) -----------------
# Indlæs pakker, assessment grid og set path fra source setup fil
source("scripts/00_setup.R")
# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Indlæs broer hentet via OSM service i QGIS
broer <- st_read(file.path(PATHS$input_pressure, "broer","osm_broer","broer_osm.gpkg")) 

#--------- intersection med undersøgelsesområdet
broer_koge <- st_intersection(broer, assessment_area_dissolved)
message("Antal broer i undersøgelsesområdet: ", paste(nrow(broer_koge)))

# Læg 50 m buffer omkring hver bro
broer_koge_buffer <- broer_koge %>%
  st_buffer(dist = 50) %>%
  st_union() %>%
  st_as_sf()

# For at få fraktion pr. celle skal bufferne klippes til grid-cellerne igen,
# så arealet beregnes celle-for-celle
broer_koge_buffer_grid <- st_intersection(broer_koge_buffer, grid) %>%
  mutate(area_bro = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_bro_id = sum(area_bro), .groups = "drop")

broer_koge_gridded <- grid %>%                          # start fra HELE grid
  left_join(broer_koge_buffer_grid, by = "id") %>%      # join bro-arealer på
  mutate(
    area_bro_id = tidyr::replace_na(as.numeric(area_bro_id), 0),  # ingen bro = 0
    value = as.numeric(area_bro_id) / as.numeric(area_grid),
    value = pmin(value, 1)
  ) %>%
  dplyr::select(id, value, geometry) %>%
  st_as_sf()


message("Antal grid-celler i alt: ", nrow(broer_koge_gridded),
        " – heraf med Broer: ", sum(broer_koge_gridded$value > 0))

#-------- Lav raster ------------------

bro_buffer_rast <- terra::rasterize(
  terra::vect(broer_koge_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",    # præcis en værdi på id 
  background = NA         # celler udenfor assessment area → NA
)

plot(bro_buffer_rast)
terra::writeRaster(
  bro_buffer_rast,
  filename = file.path(PATHS$output_pressure_tif,"anlaeg", "broer_buffer.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

map_broer_buffer <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = broer_koge_buffer,fill = "yellow", color=NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_broer_buffer

ggsave(plot = map_broer_buffer,
       filename = file.path(PATHS$output_pressure_png, "anlaeg","broer_buffer.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)


# - Broer uden buffer ---------------------------

# Sæt present-værdi (1) på alle broer
broer_koge_pa <- broer_koge %>%
  mutate(value = 1)

# Konverter til raster: enhver celle berørt af en bro → 1, ellers 0
bro_pa_rast <- terra::rasterize(
  terra::vect(broer_koge_pa),
  grid_raster,
  field      = "value",
  fun        = "max",    # present hvis mindst én bro berører cellen
  background = 0          # celler uden bro (men i assessment area) → 0
)
# Maskér til assessment area, så celler udenfor bliver NA
bro_pa_rast <- terra::mask(bro_pa_rast, assessment_area_vect)

plot(bro_pa_rast)
terra::writeRaster(
  bro_pa_rast,
  filename = file.path(PATHS$output_pressure_tif, "\\broer_pa.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

map_broer_pa <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = broer_koge, color = "yellow", size = 1.5) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_broer_pa

ggsave(plot = map_broer_pa,
       filename = file.path(PATHS$output_pressure_png, "/anlaeg/", "broer_pa.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
