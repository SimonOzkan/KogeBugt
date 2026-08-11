#---------------------- Klapning (dumpning af opmudret materiale) -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs
target_crs <- 25832

# Indlæs grid og undersøgelsesområde fra de forskellige project paths

grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(., crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = st_area(.)) %>%
  st_drop_geometry(.)

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)


## ------------------------------------------------------------------
## 1 Indlæs klapningsdata (se data folder / metadata_log)
## ------------------------------------------------------------------

klapning <- st_read(file.path(PATHS$input_pressure, "\\klapning\\EMODnet_HA_DredgeSpoilDumping_20251105\\EMODnet_HA_DredgeSpoilDumping_pg_Locations_20251105.shp")) %>%
  st_transform(., crs = target_crs) %>%
  st_make_valid()

# TODO: Forhør med Ciaran om der kun tages højde for område og ikke hvad/hvor meget der dumpes.
# Hvis ikke, anvend EMODnet-laget i stedet (samme geografiske data, men simplificeret
# attributinformation), så dubletter mellem de to kilder undgås. Indlæses her til senere brug,
# men indgår ikke i den aktuelle beregning.
klapning_HELCOM <- st_read(file.path(PATHS$input_pressure, "\\klapning\\_ags_depositing_poly_36_2_2024\\depositing_poly_36_2_2024.shp")) %>%
  st_transform(., crs = target_crs)


## ------------------------------------------------------------------
## 2 Intersection med grid
## ------------------------------------------------------------------


klapning_oresund <- st_intersection(klapning, grid) %>%
  filter(SITE_NAME != "Avlandshage Klapplads") %>%  # står ikke på Miljøministeriets oversigt over klappladser (er udgået)
  mutate(area_klapning = st_area(.))

klapning_area <- klapning_oresund %>%
  left_join(grid_area, by = "id") %>%
  mutate(value = as.numeric(area_klapning) / as.numeric(area_grid),
         value = pmin(value, 1)) %>%
  st_make_valid()

## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------


r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

klapning_rast <- terra::rasterize(
  terra::vect(klapning_area),
  r_template,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

plot(klapning_rast)

terra::writeRaster(
  klapning_rast,
  filename  = file.path(PATHS$output_pressure_tif, "\\klapning.tif"),
  overwrite = TRUE
)

## ------------------------------------------------------------------
## 4  Plotting for bilag 
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)


map_klapning <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = klapning_area, aes(fill = value), color = NA) +
  # Indlæs temear fra 00_setup dokument
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_klapning

ggsave(plot = map_klapning,
       filename = file.path(PATHS$output_pressure_png, "\\klapning.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
