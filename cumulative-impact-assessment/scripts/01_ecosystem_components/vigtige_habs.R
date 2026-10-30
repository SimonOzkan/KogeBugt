# Områder vigtige for friluftsliv ###

source("scripts/00_setup.R")
PATHS <- set_project_paths()


vigtig_habs <- st_read(file.path(PATHS$input_ecosystem, "Områder vigtige for friluftsliv","Områder vigtige for friluftsliv.shp"))

vigtig_habs <- st_transform(vigtig_habs, crs = target_crs)

vigtig_habs_grid <- st_intersection(grid_sf, vigtig_habs) %>%
  mutate(area_habs = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_habs = as.numeric(area_habs, na.rm = TRUE), .groups="drop")

vigtig_habs_grid_sf <- left_join(grid_sf, vigtig_habs_grid) %>%
  mutate(area_habs = tidyr::replace_na(area_habs, 0),
         value = area_habs / area_grid) %>%
  st_make_valid()



vigtig_habs_rast <- terra::rasterize(
  terra::vect(vigtig_habs_grid_sf),
  grid_raster,
  field      = "value",
  fun        = "max",
  background = NA
)

plot(vigtig_habs_rast)

terra::writeRaster(vigtig_habs_rast,
                   file.path(PATHS$output_ecosystem_tif, "vigtige_habs", "vigtig_habs_rast.tif"),
                   overwrite = TRUE)



# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
vigtig_habs_sf <- vigtig_habs_rast %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()



# plot


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
map_vigtige_habs <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = vigtig_habs_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_vigtige_habs


gem_trim(map_vigtige_habs,
         file.path(PATHS$output_ecosystem_png, "vigtige_habs\\vigtige_habs.png"),
         dpi = 360, width_cm = 18)
