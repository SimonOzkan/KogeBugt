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
bypass_koge <- st_intersection(bypass, grid_sf) %>%
  st_make_valid()

## ------------------------------------------------------------------
## Intersection med 250m grid og beregn fraktion
## ------------------------------------------------------------------
bypass_grid <- bypass_koge %>%
  mutate(area_intersect = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_frac = sum(area_intersect, na.rm = TRUE), .groups = "drop")

# Start fra HELE grid, så celler uden bypass får eksplicit 0
bypass_frac <- grid_sf %>%
  left_join(bypass_grid, by = "id") %>%
  mutate(
    area_frac = tidyr::replace_na(area_frac, 0),   # ingen bypass = 0
    value = as.numeric(area_frac)/ as.numeric(area_grid)
  ) %>%
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

global(is.na(r_bypass) & !is.na(grid_raster), "sum")[1, 1]

r_bypass_fill <- fill_na(r_bypass, grid_raster, w = 3)
plot(r_bypass_fill)
# Gem tif
terra::writeRaster(
  r_bypass,
  file.path(PATHS$output_pressure_tif, "anlaeg","bypass.tif"),
  overwrite = TRUE
)

## ------------------------------------------------------------------
## Plot 
## ------------------------------------------------------------------

bypass_sf <- r_bypass_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_bypass <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = bypass_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_bypass

gem_trim(map_bypass,
         file.path(PATHS$output_pressure_png, "anlaeg", "bypass.png"),
         dpi = 360, width_cm = 18)

