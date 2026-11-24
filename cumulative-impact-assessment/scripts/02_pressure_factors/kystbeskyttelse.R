#----------------------------------- Kystbeskyttelse / Anlæg på havet  ----------------------- ##

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
# Hent tilgængelige project paths
PATHS <- set_project_paths()


## Bemærk der er et lag kaldet sandfodring og Terrænændring (dækker det samme). Hør om det skal med. 

# EMODnet for Sverige
kystbeskyt <- st_read(file.path(PATHS$input_pressure, "/kyst_modifikationer/_ags_CoastalDefence_lines_HOLAS3/CoastalDefence_lines_HOLAS3.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_intersection(assessment_area_dissolved)

plot(st_geometry(kystbeskyt))
#--------- intersection med undersøgelsesområdet
kystbeskyt_koge <- st_intersection(kystbeskyt, grid_sf) %>%
  mutate(value=1) %>%
  st_drop_geometry()

kystbeskyt_gridded <- grid_sf %>%
  left_join(kystbeskyt_koge) %>%
  mutate(value = tidyr::replace_na(value, 0)) %>%
  st_make_valid()

#-------- Lav raster ------------------

kystbeskyt_rast <- terra::rasterize(
  terra::vect(kystbeskyt_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",    # præcis en værdi på id 
  background = NA         # celler udenfor assessment area → NA
)


plot(kystbeskyt_rast)


max_dist <- 3000

kystbeskyt_decay <- decay_focal(kystbeskyt_rast, max_dist)

plot(kystbeskyt_decay)

terra::writeRaster(
  kystbeskyt_decay,
  filename = file.path(PATHS$output_pressure_tif,"iei", "kystbeskyt_decay.tif"), 
  overwrite = TRUE
)

############### Plotting for bilag ################

map_kystbeskyt <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = kystbeskyt,
          aes(fill = "Tilstede"), shape = 21, color = "red", size = 1.5) +
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
map_kystbeskyt

gem_trim(map_kystbeskyt,
         file.path(PATHS$output_pressure_png, "iei", "kystbeskyt.png"),
         dpi = 360, width_cm = 18)
