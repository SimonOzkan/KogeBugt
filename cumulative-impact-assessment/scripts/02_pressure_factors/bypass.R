#----------------------------------- Bypass ----------------------- ##
source(here::here("scripts/00_setup.R"))
PATHS <- set_project_paths()

## ------------------------------------------------------------------
## Hent Bypass data fra WFS
## ------------------------------------------------------------------
wfs_url_bypass <- "https://gisportal.mst.dk/server/services/ekstern/KDI_Bypass/MapServer/WFSServer"

request_bypass <- paste0(
  wfs_url_bypass,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=esri:Bypassomraader",
  "&outputFormat=GEOJSON"
)

bypass <- st_read(request_bypass) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

## ------------------------------------------------------------------
## Intersect med undersøgelsesområde
## ------------------------------------------------------------------
bypass_koge <- st_intersection(bypass, grid) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_make_valid()

## ------------------------------------------------------------------
## Intersection med 250m grid og beregn fraktion
## ------------------------------------------------------------------
bypass_grid <- bypass_koge %>%
  mutate(area_intersect = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  mutate(
    area_frac = area_intersect / as.numeric(area_grid),
    area_frac = pmin(area_frac, 1)
  ) %>%
  group_by(id) %>%
  summarise(area_frac = sum(area_frac, na.rm = TRUE), .groups = "drop")

# Start fra HELE grid, så celler uden bypass får eksplicit 0
bypass_frac <- grid %>%
  left_join(bypass_grid, by = "id") %>%
  mutate(
    area_frac = tidyr::replace_na(area_frac, 0),   # ingen bypass = 0
    value     = pmin(area_frac, 1)
  ) %>%
  dplyr::select(id, value, geometry) %>%
  st_as_sf()

message("Grid-celler i alt: ", nrow(bypass_frac),
        " – heraf med bypass: ", sum(bypass_frac$value > 0))


## ------------------------------------------------------------------
## 4. Lav til raster
## ------------------------------------------------------------------

r_bypass <- terra::rasterize(
  terra::vect(bypass_frac),
  grid_raster,
  field      = "value",
  fun        = "max",
  background = NA
)

plot(r_bypass)
# Gem tif
terra::writeRaster(
  r_bypass,
  file.path(PATHS$output_pressure_tif, "anlaeg","bypass_frac.tif"),
  overwrite = TRUE
)

## ------------------------------------------------------------------
## Plot (kun visuelt)
## ------------------------------------------------------------------
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)

viridis_start_color <- viridis_pal()(1)  

map_bypass <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = bypass_koge, fill = "yellow", color = NA, size = 1.5) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_bypass


ggsave(
  plot     = map_bypass,
  filename = file.path(PATHS$output_pressure_png, "anlaeg", "bypass.png"),
  bg       = "white",
  height   = 18,
  width    = 18,
  dpi      = 300
)
