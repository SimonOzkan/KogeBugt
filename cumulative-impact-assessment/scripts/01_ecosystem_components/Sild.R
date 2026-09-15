#----------------------------------- Skrubbe ----------------------- ##
source(here::here("scripts/00_setup.R"))
PATHS <- set_project_paths()

# Indlæs HELCOM udbredelseskort
sild <- terra::rast(file.path(PATHS$input_ecosystem, "fisk/sild/sild_helcom","EC_36.tif")) %>%
  terra::project(.,crs(grid))

# Fra 500x500 res til 250x250 med bilinear metode
sild_250 <- terra::resample(sild, grid_raster, method = "bilinear") %>%
  terra::mask(terra::vect(assessment_area_dissolved))

names(sild_250) <- "value"

# gem som tif
terra::writeRaster(
  sild_250,
  filename = file.path(PATHS$output_ecosystem_tif,"fisk", "/sild_250.tif"),
  overwrite = TRUE
)

# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
sild_sf <- sild_250 %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_sild <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = sild_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_sild


ggsave(plot = map_sild,
       filename = file.path(PATHS$output_ecosystem_png,"/fisk/" ,"sild.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)




