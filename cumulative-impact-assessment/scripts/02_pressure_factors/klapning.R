#---------------------- Klapning (dumpning af opmudret materiale) -----------------
# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

PATHS <- set_project_paths()

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


klapning_oresund <- st_intersection(klapning, grid_sf) %>%
  filter(SITE_NAME != "Avlandshage Klapplads") %>%  # står ikke på Miljøministeriets oversigt over klappladser (er udgået)
  mutate(area_klapning = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_klapning = sum(as.numeric(area_klapning, na.rm = TRUE)), .groups = "drop")

klapning_area <- grid_sf %>%
  left_join(klapning_oresund, by = "id") %>%
  mutate(value = tidyr::replace_na(as.numeric(area_klapning), 0),  # ingen klapning
         value = as.numeric(value) / as.numeric(area_grid),
         value = pmin(value, 1)) %>%
  st_make_valid()

## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------

klapning_rast <- terra::rasterize(
  terra::vect(klapning_area),
  grid_raster,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

plot(klapning_rast_fill)

klapning_rast_fill <- fill_na(klapning_rast, grid_raster, w = 3)

terra::writeRaster(
  klapning_rast_fill,
  filename  = file.path(PATHS$output_pressure_tif, "anlaeg","\\klapning.tif"),
  overwrite = TRUE
)

## ------------------------------------------------------------------
## 4  Plotting for bilag 
## ------------------------------------------------------------------

klapning_sf <- klapning_rast_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_klapning <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = klapning_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_klapning

gem_trim(map_klapning,
         file.path(PATHS$output_pressure_png, "anlaeg", "klapning.png"),
         dpi = 360, width_cm = 18)
