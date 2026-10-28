#---------------------- Renseanlæg (buffer / fraktion) -----------------
# Indlæs pakker, assessment grid og set path fra source setup fil
source("scripts/00_setup.R")
# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Indlæs renseanlæg / udledningspunkter (EMODnet - UWWTD Discharge Points)
renseanlaag <- st_read(file.path(PATHS$input_pressure, "kemisk_forurening",
                                 "spildevand",
                                 "EMODnet_HA_WasteDisposal_UWWTD_DischargePoints_20250718",
                                 "EMODnet_HA_WasteDisposal_UWWTP_DischargePoints_Locations_20250718.shp")) %>%
  st_transform(crs = target_crs)

#--------- intersection med undersøgelsesområdet
rens_koge <- st_intersection(renseanlaag, assessment_area_dissolved)
message("Antal renseanlæg i undersøgelsesområdet: ", paste(nrow(rens_koge)))

# Læg 100 m buffer omkring hvert udledningspunkt (punkter -> cirkler)
rens_koge_buffer <- rens_koge %>%
  st_buffer(dist = 100) %>%
  st_union() %>%
  st_as_sf()

# For at få fraktion pr. celle skal bufferne klippes til grid-cellerne igen,
# så arealet beregnes celle-for-celle
rens_koge_buffer_grid <- st_intersection(rens_koge_buffer, grid_sf) %>%
  mutate(area_rens = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_rens_id = sum(area_rens), .groups = "drop")

rens_koge_gridded <- grid_sf %>%                           # start fra HELE grid
  left_join(rens_koge_buffer_grid, by = "id") %>%       # join renseanlæg-arealer på
  mutate(
    area_rens_id = tidyr::replace_na(as.numeric(area_rens_id), 0),  # ingen renseanlæg = 0
    value = as.numeric(area_rens_id) / as.numeric(area_grid)
  ) %>%
  st_as_sf()


message("Antal grid-celler i alt: ", nrow(rens_koge_gridded),
        " – heraf med renseanlæg: ", sum(rens_koge_gridded$value > 0))

#-------- Lav raster ------------------

rens_buffer_rast <- terra::rasterize(
  terra::vect(rens_koge_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",    # præcis en værdi på id
  background = NA         # celler udenfor assessment area → NA
)


plot(rens_buffer_rast)
terra::writeRaster(
  rens_buffer_rast,
  filename = file.path(PATHS$output_pressure_tif, "anlaeg", "renseanlaeg_buffer.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################


map_rens_buffer <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = rens_koge,
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
map_rens_buffer


gem_trim(map_rens_buffer,
         file.path(PATHS$output_pressure_png, "anlaeg","renseanlaeg_buffer.png"),
         dpi = 360, width_cm = 18)
