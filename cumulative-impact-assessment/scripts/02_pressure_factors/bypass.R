#----------------------------------- Bypass ----------------------- ##
source(here::here("scripts/00_setup.R"))
PATHS <- set_project_paths()

## ------------------------------------------------------------------
## 1. Hent Bypass data fra WFS
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
## 2. Intersect med undersøgelsesområde
## ------------------------------------------------------------------
bypass_koge <- st_intersection(bypass, grid) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_make_valid()

message("Antal bypass-polygoner i Køge Bugt: ", nrow(bypass_koge))

## ------------------------------------------------------------------
## 3. Intersection med 250m grid og beregn fraktion
## ------------------------------------------------------------------
bypass_grid <- bypass_koge %>%
  mutate(area_intersect = as.numeric(st_area(.)))

# Beregn fraktion af grid celle geometrien og tilføj celle geometri
bypass_frac <- bypass_grid %>%
  st_drop_geometry() %>%
  mutate(
    area_frac = area_intersect / area_grid,
    area_frac = pmin(area_frac, 1)
  ) %>%
  group_by(id) %>%
  summarise(
    area_frac = sum(area_frac, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(area_frac = pmin(area_frac, 1)) %>%
  left_join(grid, by = "id") %>% 
  dplyr::select(-area_grid)
  


## ------------------------------------------------------------------
## 4. Lav til raster
## ------------------------------------------------------------------

r_bypass <- terra::rasterize(
  terra::vect(bypass_frac),
  r_template,
  field      = "area_frac",
  fun        = "max",
  background = NA
)
r_bypass[r_bypass == 0] <- NA

# Gem tif
terra::writeRaster(
  r_bypass,
  file.path(PATHS$output_pressure_tif, "bypass_frac.tif"),
  overwrite = TRUE
)
message("Gemt: bypass_frac.tif")

## ------------------------------------------------------------------
## 5. Lav sf til plot
## ------------------------------------------------------------------
bypass_sf_plot <- r_bypass %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = 1) %>%
  st_transform(crs = target_crs)

## ------------------------------------------------------------------
## 6. Indlæs baggrundskort og plot
## ------------------------------------------------------------------
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)


viridis_start_color <- viridis_pal()(1)  

map_bypass <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = bypass_sf_plot, aes(fill = value), color = NA, size = 2) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_bypass

## ------------------------------------------------------------------
## 7. Gem plot
## ------------------------------------------------------------------
ggsave(
  plot     = map_bypass,
  filename = file.path(PATHS$output_pressure_png, "\\anlaeg_hav\\bypass.png"),
  bg       = NULL,
  height   = 18,
  width    = 18,
  dpi      = 300
)
