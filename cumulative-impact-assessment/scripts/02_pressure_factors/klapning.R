#---------------------- Klapning (dumpning af opmudret materiale) -----------------
# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

PATHS <- set_project_paths()

## ------------------------------------------------------------------
## 1 Indlæs klapningsdata (se data folder / metadata_log)
## ------------------------------------------------------------------

####
klapning_HELCOM <- st_read(file.path(PATHS$input_pressure, "\\klapning\\_ags_depositing_poly_36_2_2024\\depositing_poly_36_2_2024.shp")) %>%
  st_transform(., crs = target_crs)


Klapning <- read_excel(file.path(PATHS$input_pressure,"klapning/Klapning.xlsx"))

Klapning_data <- read_excel(file.path(PATHS$input_pressure,"klapning/Klapning_data_GIS.xlsx")) %>%
  rename("klapplads" = "Klapplads_",
         "id_klap" = "id") 

# Arealinformation

# ── Hent Holmene fra WFS ───────────────────────────────────────────────────


request <- paste0(
  "https://wfs2-miljoegis.mim.dk/vp3_2endelig2025/ows",
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=vp3_2endelig2025:vp3_2e2025_klappladser",
  "&outputFormat=application/json"
)

klapning_arealinfo <- st_read(request) %>%
  st_transform(crs = target_crs) %>%
  dplyr::select(-c(id,vandomra_1,vandomrade,area,navn,perimeter))

klapning_all <- klapning_arealinfo %>%
  left_join(Klapning_data, by = "klapplads") %>%          # <- tjek nøglen
  filter(!is.na(Gennemsn_1)) %>%
  mutate(area_klap = as.numeric(st_area(.)),
         m3_per_km2 = (as.numeric(Gennemsn_1) / area_klap )*10^6,
         m3_per_km2_lg = log10(m3_per_km2+1))  # intensitet pr. plads

  
max_klap <- max(klapning_all$m3_per_km2, na.rm = TRUE)
max_klap_log <- max(klapning_all$m3_per_km2_lg, na.rm = TRUE)

min_klap <- min(klapning_all$m3_per_km2, na.rm = TRUE)
min_klap_log <- min(klapning_all$m3_per_km2_lg, na.rm = TRUE)


area_grid <- as.numeric(st_area(grid_sf))[1]              # celleareal (fx 62500)

klapning_cells <- st_intersection(klapning_all, grid_sf) %>%
  mutate(piece_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(value = sum(m3_per_km2 * piece_area) / area_grid, .groups = "drop") %>%  # lineær celle-intensitet
  mutate(value_lg      = log10(value + 1),                # log AF celle-intensiteten
         value_norm    = value    / max_klap,             # min=0 -> = (value-0)/(max-0)
         value_norm_lg = value_lg / max_klap_log)          # max_klap_log = log10(max_klap + 1)       # 0-1


## 3. Fordel til grid, 0 hvor ingen klapning ----------------------------------
klapning_gridded <- grid_sf %>%
  left_join(klapning_cells, by = "id") %>%
  mutate(value_norm = tidyr::replace_na(value_norm, 0),
         value_norm_lg = tidyr::replace_na(value_norm_lg,0)) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------

klapning_rast <- terra::rasterize(
  terra::vect(klapning_gridded),
  grid_raster,
  field      = "value_norm",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)



klapning_rast_lg_norm <- terra::rasterize(
  terra::vect(klapning_gridded),
  grid_raster,
  field      = "value_norm_lg",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

par(mfrow = c(1,2))
plot(klapning_rast)
plot(klapning_rast_lg_norm)

terra::writeRaster(
  klapning_rast,
  filename  = file.path(PATHS$output_pressure_tif, "anlaeg","\\klapning_norm.tif"),
  overwrite = TRUE
)



terra::writeRaster(
  klapning_rast_lg_norm,
  filename  = file.path(PATHS$output_pressure_tif, "anlaeg","\\klapning_log_norm.tif"),
  overwrite = TRUE
)


## ------------------------------------------------------------------
## 4.1  Plotting for bilag - ikke log transformeret
## ------------------------------------------------------------------

klapning_sf <- klapning_rast %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_klapning_norm <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = klapning_sf, aes(fill = value_norm), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_klapning_norm

gem_trim(map_klapning_norm,
         file.path(PATHS$output_pressure_png, "anlaeg", "klapning_norm.png"),
         dpi = 360, width_cm = 18)



## ------------------------------------------------------------------
## 4.2  Plotting for bilag - log transformeret
## ------------------------------------------------------------------

klapning_sf_lg <- klapning_rast_lg_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_klapning_lg <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = klapning_sf_lg, aes(fill = value_norm_lg), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_klapning_lg

gem_trim(map_klapning_lg,
         file.path(PATHS$output_pressure_png, "anlaeg", "klapning_lg_norm.png"),
         dpi = 360, width_cm = 18)

