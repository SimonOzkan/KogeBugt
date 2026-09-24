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
rens_koge_buffer_grid <- st_intersection(rens_koge_buffer, grid) %>%
  mutate(area_rens = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_rens_id = sum(area_rens), .groups = "drop")

rens_koge_gridded <- grid %>%                           # start fra HELE grid
  left_join(rens_koge_buffer_grid, by = "id") %>%       # join renseanlæg-arealer på
  mutate(
    area_rens_id = tidyr::replace_na(as.numeric(area_rens_id), 0),  # ingen renseanlæg = 0
    value = as.numeric(area_rens_id) / as.numeric(area_grid),
    value = pmin(value, 1)
  ) %>%
  dplyr::select(id, value, geometry) %>%
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
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

map_rens_buffer <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = rens_koge_buffer, fill = "yellow", color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_rens_buffer

ggsave(plot = map_rens_buffer,
       filename = file.path(PATHS$output_pressure_png, "anlaeg", "renseanlaeg_buffer.png"),
       bg = "white",
       height = 18,
       width = 18,
       dpi = 300)


# - Renseanlæg uden buffer ---------------------------

# Sæt present-værdi (1) på alle renseanlæg
rens_koge_pa <- rens_koge %>%
  mutate(value = 1)

# Konverter til raster: enhver celle berørt af et renseanlæg → 1, ellers 0
rens_pa_rast <- terra::rasterize(
  terra::vect(rens_koge_pa),
  grid_raster,
  field      = "value",
  fun        = "max",    # present hvis mindst ét renseanlæg berører cellen
  background = 0          # celler uden renseanlæg (men i assessment area) → 0
)
# Maskér til assessment area, så celler udenfor bliver NA
rens_pa_rast <- terra::mask(rens_pa_rast, assessment_area_vect)

plot(rens_pa_rast)
terra::writeRaster(
  rens_pa_rast,
  filename = file.path(PATHS$output_pressure_tif, "anlaeg", "renseanlaeg_pa.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################
map_rens_pa <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = rens_koge, color = "yellow", size = 3) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_rens_pa

ggsave(plot = map_rens_pa,
       filename = file.path(PATHS$output_pressure_png, "anlaeg", "renseanlaeg_pa.png"),
       bg = "white",
       height = 18,
       width = 18,
       dpi = 300)
