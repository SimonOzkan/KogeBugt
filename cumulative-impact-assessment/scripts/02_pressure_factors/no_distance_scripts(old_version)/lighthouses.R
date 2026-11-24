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
fyr_koge <- st_intersection(lighthouse, assessment_area_dissolved)

######## Dette er ikke aktuelt efter effekt distance #####
# # Læg 25 m buffer omkring hvert fyrtårn (punkter -> cirkler)
# fyr_koge_buffer <- fyr_koge %>%
#   st_buffer(dist = 25) %>%
#   st_union() %>%
#   st_as_sf()
# 
# # For at få fraktion pr. celle skal bufferne klippes til grid-cellerne igen,
# # så arealet beregnes celle-for-celle
# fyr_koge_buffer_grid <- st_intersection(fyr_koge_buffer, grid_sf) %>%
#   mutate(area_fyr = st_area(.)) %>%
#   st_drop_geometry() %>%
#   group_by(id) %>%
#   summarise(area_fyr_id = sum(as.numeric(area_fyr), na.rm = TRUE), .groups = "drop")
# 
# fyr_koge_gridded <- grid_sf %>%                            # start fra HELE grid
#   left_join(fyr_koge_buffer_grid, by = "id") %>%        # join fyr-arealer på
#   mutate(
#     area_fyr_id = tidyr::replace_na(as.numeric(area_fyr_id), 0),  # ingen fyr = 0
#     value = as.numeric(area_fyr_id) / as.numeric(area_grid)  ) %>%
#   st_as_sf()
# 
# 
# message("Antal grid-celler i alt: ", nrow(fyr_koge_gridded),
#         " – heraf med fyrtårne: ", sum(fyr_koge_gridded$value > 0))
# 
# lav 
# 
# fyr_buffer_rast <- terra::rasterize(
#   terra::vect(fyr_koge_gridded),
#   grid_raster,
#   field      = "value",
#   fun        = "max",    # præcis en værdi på id
#   background = NA         # celler udenfor assessment area → NA
# )
# plot(fyr_buffer_rast) 
#-----------

max_dist <- 5400 # m - taget fra økomar

#### Anvender effekt distance funktion ##### 

fyr_decay <- distance_decay(fyr_koge, grid_raster, max_dist)
names(fyr_decay) <- "lighthouses"

plot(fyr_decay)
plot(st_geometry(fyr_koge), add = TRUE, pch = 16, cex = 0.5)


terra::writeRaster(
  fyr_decay,
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
         file.path(PATHS$output_pressure_png, "anlaeg", "fyrtaarn.png"),
         dpi = 360, width_cm = 18)
