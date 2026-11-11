############## Fritidsdykning

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Hent fritidsdykkersteder

fritidsdyk <- st_read(file.path(PATHS$input_pressure, "fritidsdykning","Fritidsdykning.shp")) %>%
  st_transform(, crs = target_crs) 
  

# Tilføj buffer og intersect

fritidsdyk_buf <- fritidsdyk %>%
  st_buffer(dist = 200) %>%
  st_union() %>%
  st_make_valid()
## ------------------------------------------------------------------
## 2 Intersection med grid
## ------------------------------------------------------------------
fritidsdyk_area <- st_intersection(grid_sf, fritidsdyk_buf) %>%   # <- grid_sf først
  mutate(area_dyk = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_dyk = sum(area_dyk, na.rm = TRUE), .groups = "drop")


fritidsdyk_gridded <- grid_sf %>%
  left_join(fritidsdyk_area, by = "id") %>%
  mutate(value = tidyr::replace_na(as.numeric(area_dyk), 0),  # ingen fritidsdyk
         value = as.numeric(value) / as.numeric(area_grid)) %>%
  st_make_valid()

## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------

fritidsdyk_rast <- terra::rasterize(
  terra::vect(fritidsdyk_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)


fritidsdyk_rast_fill <- fill_na(fritidsdyk_rast, grid_raster, w = 3)

plot(fritidsdyk_rast_fill)


terra::writeRaster(
  fritidsdyk_rast_fill,
  filename  = file.path(PATHS$output_pressure_tif, "rekreative_aktiv","fritidsdyk.tif"),
  overwrite = TRUE
)

## ------------------------------------------------------------------
## 4  Plotting for bilag 
## ------------------------------------------------------------------

fritidsdyk_sf <- fritidsdyk_rast_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_fritidsdyk <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = fritidsdyk_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_fritidsdyk

gem_trim(map_fritidsdyk,
         file.path(PATHS$output_pressure_png, "rekreative_aktiv", "fritidsdyk.png"),
         dpi = 360, width_cm = 18)
