############## Områder vigtige for friluftsliv ##########

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Hent fritidsdykkersteder
## ------------------------------------------------------------------
## 1 Hent data
## ------------------------------------------------------------------

kystrekreative <- st_read(file.path(PATHS$input_pressure, "kystrekreative","friluftsliv_area")) %>%
  st_transform(, crs = target_crs) 


## ------------------------------------------------------------------
## 2 Intersection med grid
## ------------------------------------------------------------------


kystrekreative_area <- st_intersection(grid_sf,kystrekreative) %>%
  mutate(A_kystrea = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(A_kystrea = sum(A_kystrea, na.rm = TRUE), .groups = "drop")


kystrekreative_gridded <- grid_sf %>%
  left_join(kystrekreative_area) %>%
  mutate(value = tidyr::replace_na(A_kystrea, 0),
         value = as.numeric(value) / as.numeric(area_grid)) %>%
  st_make_valid()




## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------


kystrekreative_rast <- terra::rasterize(
  terra::vect(kystrekreative_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

plot(kystrekreative_rast)


terra::writeRaster(
  kystrekreative_rast,
  filename  = file.path(PATHS$output_pressure_tif, "rekreative_aktiv","kystrekreative.tif"),
  overwrite = TRUE
)


## ------------------------------------------------------------------
## 4  Plotting for bilag
## ------------------------------------------------------------------

kystrekreative_rast_sf <- kystrekreative_rast %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()



map_kystrekreative<- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = kystrekreative_rast_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_kystrekreative

gem_trim(map_kystrekreative,
         file.path(PATHS$output_pressure_png, "rekreative_aktiv","kystrekreative.png"),
         dpi = 360, width_cm = 18)
