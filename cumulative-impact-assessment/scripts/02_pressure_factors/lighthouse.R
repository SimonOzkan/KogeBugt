#---------------------- Fyrtårne (buffer / fraktion) -----------------
# Indlæs pakker, assessment grid og set path fra source setup fil
source("scripts/00_setup.R")
# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Indlæs fyrtårne (EMODnet)
lighthouse <- st_read(file.path(PATHS$input_pressure,
                                "fyrtaarn",
                                "EMODnet_HA_Heritage_Lighthouses_202231016",
                                "EMODnet_HA_Heritage_Lighthouses_20231016.shp")) %>%
  st_transform(crs = target_crs)

#--------- intersection med undersøgelsesområdet
fyr_koge <- st_intersection(lighthouse, grid_sf) %>%
  mutate(value = 1)

fyr_koge_gridded <- grid_sf %>%
  left_join(fyr_koge %>% st_drop_geometry()) %>%
  mutate(value = tidyr::replace_na(value, 0))

max_dist <- 5400 # m - taget fra økomar


fyr_raster <- terra::rasterize(
  terra::vect(fyr_koge_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",    # præcis en værdi på id
  background = NA         # celler udenfor assessment area → NA
)
plot(fyr_raster)

fyrtaarn_decay    <- decay_focal(fyr_raster, max_dist)

plot(fyrtaarn_decay)
plot(st_geometry(fyr_koge), add = TRUE, pch = 16, cex = 0.5)


terra::writeRaster(
  fyrtaarn_decay,
  filename = file.path(PATHS$output_pressure_tif, "iei", "fyrtaarne_decay.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################

map_fyr_buffer <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = fyr_koge,
          aes(fill = "Tilstede"), shape = 21, color = "black", size = 1.5) +
  scale_fill_manual(name = NULL,
                    values = c("Tilstede" = "red", "Ikke tilstede" = "#9FBDD8"))+
  boundary +
  theme_minimal() +
  my_theme +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 16))+
  north_arrow +
  scale_bar
map_fyr_buffer



gem_trim(map_fyr_buffer,
         file.path(PATHS$output_pressure_png, "iei", "fyrtaarn.png"),
         dpi = 360, width_cm = 18)


