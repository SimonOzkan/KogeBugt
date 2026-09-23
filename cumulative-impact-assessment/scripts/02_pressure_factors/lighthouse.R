#---------------------- Fyrtårne (buffer / fraktion) -----------------
# Indlæs pakker, assessment grid og set path fra source setup fil
source("scripts/00_setup.R")
# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Indlæs fyrtårne (EMODnet Heritage - Lighthouses)
# NB: EMODnet-lag ligger typisk i EPSG:3035, så vi transformerer til target_crs
#     med det samme for at undgå CRS-mismatch ved st_intersection nedenfor.
lighthouse <- st_read(file.path(PATHS$input_pressure,
                                "fyrtaarn",
                                "EMODnet_HA_Heritage_Lighthouses_202231016",
                                "EMODnet_HA_Heritage_Lighthouses_20231016.shp")) %>%
  st_transform(crs = target_crs)

#--------- intersection med undersøgelsesområdet
fyr_koge <- st_intersection(lighthouse, assessment_area_dissolved)
message("Antal fyrtårne i undersøgelsesområdet: ", paste(nrow(fyr_koge)))

# Læg 25 m buffer omkring hvert fyrtårn (punkter -> cirkler)
fyr_koge_buffer <- fyr_koge %>%
  st_buffer(dist = 25) %>%
  st_union() %>%
  st_as_sf()

# For at få fraktion pr. celle skal bufferne klippes til grid-cellerne igen,
# så arealet beregnes celle-for-celle
fyr_koge_buffer_grid <- st_intersection(fyr_koge_buffer, grid) %>%
  mutate(area_fyr = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_fyr_id = sum(area_fyr), .groups = "drop")

fyr_koge_gridded <- grid %>%                            # start fra HELE grid
  left_join(fyr_koge_buffer_grid, by = "id") %>%        # join fyr-arealer på
  mutate(
    area_fyr_id = tidyr::replace_na(as.numeric(area_fyr_id), 0),  # ingen fyr = 0
    value = as.numeric(area_fyr_id) / as.numeric(area_grid),
    value = pmin(value, 1)
  ) %>%
  dplyr::select(id, value, geometry) %>%
  st_as_sf()


message("Antal grid-celler i alt: ", nrow(fyr_koge_gridded),
        " – heraf med fyrtårne: ", sum(fyr_koge_gridded$value > 0))

#-------- Lav raster ------------------

fyr_buffer_rast <- terra::rasterize(
  terra::vect(fyr_koge_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",    # præcis en værdi på id
  background = NA         # celler udenfor assessment area → NA
)

plot(fyr_buffer_rast)
terra::writeRaster(
  fyr_buffer_rast,
  filename = file.path(PATHS$output_pressure_tif, "anlaeg", "fyrtaarne_buffer.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

map_fyr_buffer <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = fyr_koge_buffer, fill = "yellow", color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_fyr_buffer

ggsave(plot = map_fyr_buffer,
       filename = file.path(PATHS$output_pressure_png, "anlaeg", "fyrtaarne_buffer.png"),
       bg = "white",
       height = 18,
       width = 18,
       dpi = 300)


# - Fyrtårne uden buffer ---------------------------

# Sæt present-værdi (1) på alle fyrtårne
fyr_koge_pa <- fyr_koge %>%
  mutate(value = 1)

# Konverter til raster: enhver celle berørt af et fyrtårn → 1, ellers 0
fyr_pa_rast <- terra::rasterize(
  terra::vect(fyr_koge_pa),
  grid_raster,
  field      = "value",
  fun        = "max",    # present hvis mindst ét fyrtårn berører cellen
  background = 0          # celler uden fyr (men i assessment area) → 0
)
# Maskér til assessment area, så celler udenfor bliver NA
fyr_pa_rast <- terra::mask(fyr_pa_rast, assessment_area_vect)

plot(fyr_pa_rast)
terra::writeRaster(
  fyr_pa_rast,
  filename = file.path(PATHS$output_pressure_tif, "anlaeg", "fyrtaarne_pa.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################
map_fyr_pa <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = fyr_koge, color = "yellow", size = 1.5) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_fyr_pa

ggsave(plot = map_fyr_pa,
       filename = file.path(PATHS$output_pressure_png, "anlaeg", "fyrtaarne_pa.png"),
       bg = "white",
       height = 18,
       width = 18,
       dpi = 300)