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
