#----------------------------------- torsk ----------------------- ##
source(here::here("scripts/00_setup.R"))
PATHS <- set_project_paths()

# Indlæs HELCOM udbredelseskort
torsk <- terra::rast(file.path(PATHS$input_ecosystem, "fisk/torsk/_ags_EC_35","EC_35.tif")) %>%
  terra::project(.,crs(grid))

# Fra 500x500 res til 250x250 med bilinear metode
torsk_250 <- terra::resample(torsk, grid_raster, method = "bilinear") %>%
  terra::mask(terra::vect(assessment_area_dissolved))

names(torsk_250) <- "value"

# gem som tif
terra::writeRaster(
  torsk_250,
  filename = file.path(PATHS$output_ecosystem_tif,"fisk", "/torsk_250.tif"),
  overwrite = TRUE
)

# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
torsk_sf <- torsk_250 %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_torsk <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = torsk_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_torsk


ggsave(plot = map_torsk,
       filename = file.path(PATHS$output_ecosystem_png,"/fisk/" ,"torsk.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)





#----------------------------------- torsk fra AquaMAPS ----------------------- ##

# Indlæs aquamaps data
torsk_AQ <- read_csv(
  "C:/Users/SIO/NIVA/Køge Bugt - General/data/Ecosystem/Fisk/torsk/1790945789.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  mutate(
    lon  = as.numeric(`Center Long`),
    lat  = as.numeric(`Center Lat`),
    value = as.numeric(`Overall Probability`)
  ) %>%
  filter(!is.na(value))


# Visualiser data
ggplot(torsk_AQ, aes(x = lon, y = lat, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Overall\nprobability", limits = c(0, 1)) +
  coord_quickmap() +
  labs(title = "Platichthys flesus – overall probability",
       x = "Longitude", y = "Latitude") +
  theme_minimal()


# Lav til raster
torsk_r_AQ <- torsk_AQ %>%
  select(lon, lat, value) %>%
  rast(type = "xyz", crs = "EPSG:4326")

# Sæt crs til samme som undersøgelsesområdet
torsk_r_AQ <- terra::project(torsk_r_AQ,crs(assessment_area_vect))


# Aquamaps har høj opløslighed og er for hele undersøgelsesområdet er der kun 3 værdier. 
# Værdierne resamples til 250 grid
torsk_250_AQ <- terra::resample(torsk_r_AQ,grid_raster, method = "near") %>%
  terra::mask(terra::vect(assessment_area_dissolved))


terra::writeRaster(
  torsk_250_AQ,
  filename = file.path(PATHS$output_ecosystem_tif,"fisk", "/torsk_250_AQ.tif"),
  overwrite = TRUE
)


# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
torsk_sf_AQ <- torsk_250_AQ %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()



# plot
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_torsk_AQ <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = torsk_sf_AQ, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_torsk_AQ


ggsave(plot = map_torsk_AQ,
       filename = file.path(PATHS$output_ecosystem_png,"/fisk/" ,"torsk_aquamaps.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)




