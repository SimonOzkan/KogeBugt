#----------------------------------- Skrubbe ----------------------- ##
source(here::here("scripts/00_setup.R"))
PATHS <- set_project_paths()


# Indlæs aquamaps data
skrubbe <- read_csv(
  "C:/Users/SIO/NIVA/Køge Bugt - General/data/Ecosystem/Fisk/skrubbe/1789369271.csv",
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
ggplot(skrubbe, aes(x = lon, y = lat, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Overall\nprobability", limits = c(0, 1)) +
  coord_quickmap() +
  labs(title = "Platichthys flesus – overall probability",
       x = "Longitude", y = "Latitude") +
  theme_minimal()


# Lav til raster
skrubbe_r <- skrubbe %>%
  select(lon, lat, value) %>%
  rast(type = "xyz", crs = "EPSG:4326")

# Sæt crs til samme som undersøgelsesområdet
skrubbe_r <- terra::project(skrubbe_r,crs(assessment_area_vect))


# Aquamaps har høj opløslighed og er for hele undersøgelsesområdet er der kun 3 værdier. 
# Værdierne resamples til 250 grid
skrubbe_250 <- terra::resample(skrubbe_r,grid_raster, method = "near") %>%
  terra::mask(terra::vect(assessment_area_dissolved))


terra::writeRaster(
  skrubbe_250,
  filename = file.path(PATHS$output_ecosystem_tif,"fisk", "/skrubbe_250.tif"),
  overwrite = TRUE
)

# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
skrubbe_sf <- skrubbe_250 %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_skrubbe <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = skrubbe_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_skrubbe


ggsave(plot = map_skrubbe,
       filename = file.path(PATHS$output_ecosystem_png,"/fisk/" ,"skrubbe.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)




