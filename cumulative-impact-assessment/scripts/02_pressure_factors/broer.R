#---------------------- Broer (buffer / fraktion) -----------------
# Indlæs pakker, assessment grid og set path fra source setup fil
source("scripts/00_setup.R")
# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Indlæs broer hentet via OSM service i QGIS
broer <- st_read(file.path(PATHS$input_pressure, "broer","osm_broer","broer_osm.gpkg")) %>%
  st_transform(crs = target_crs)

# Til at plotte
broer_koge <- st_intersection(broer, assessment_area_dissolved)

#--------- intersection med undersøgelsesområdet
broer_highway_koge <- st_intersection(broer, grid_sf) %>%
  mutate(value=1) %>%
  filter(highway %in% c("motorway","motorway_link","road","busway")) %>%
  st_drop_geometry()

broer_paths_koge <- st_intersection(broer, grid_sf) %>%
  mutate(value=1) %>%
  filter(!highway %in% c("motorway","motorway_link","road","busway"),
         !id %in% broer_highway_koge$id) %>%
  st_drop_geometry()


highway_gridded <- grid_sf %>%
  left_join(broer_highway_koge) %>%
  mutate(value = tidyr::replace_na(value, 0)) %>%
  st_make_valid()


paths_gridded <- grid_sf %>%
  left_join(broer_paths_koge) %>%
  mutate(value = tidyr::replace_na(value, 0)) %>%
  st_make_valid()

#-------- Lav raster ------------------

highway_rast <- terra::rasterize(
  terra::vect(highway_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",    # præcis en værdi på id 
  background = NA         # celler udenfor assessment area → NA
)

path_rast <- terra::rasterize(
  terra::vect(paths_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",    # præcis en værdi på id 
  background = NA         # celler udenfor assessment area → NA
)


plot(highway_rast)

plot(path_rast)



max_dist_highway <- 3200 # ØKOMAR værdi
max_dist_path <- 500


highway_decay <- decay_focal(highway_rast, max_dist = max_dist_highway)

path_decay <- decay_focal(path_rast, max_dist = max_dist_path)

par(mfrow = c(1,2))
plot(highway_decay)
plot(path_decay)



terra::writeRaster(
  highway_decay,
  filename = file.path(PATHS$output_pressure_tif,"iei", "highway_decay.tif"),
  overwrite = TRUE
)

terra::writeRaster(
  path_decay,
  filename = file.path(PATHS$output_pressure_tif,"iei", "path_decay.tif"), 
  overwrite = TRUE
)

############### Plotting for bilag ################

viridis_start_color <- viridis_pal()(1)

map_broer_buffer <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = broer_koge_buffer,
          aes(fill = "Tilstede"), shape = 21, color = NA, size = 1.5) +
  scale_fill_manual(name = NULL,
                    values = c("Tilstede" = "red", "Ikke tilstede" = "#9FBDD8"))+
  boundary +
  theme_minimal() +
  my_theme +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 30))+
  north_arrow +
  scale_bar
map_broer_buffer



gem_trim(map_broer_buffer,
         file.path(PATHS$output_pressure_png, "anlaeg", "broer_buffer_trim.png"),
         dpi = 360, width_cm = 18)

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
