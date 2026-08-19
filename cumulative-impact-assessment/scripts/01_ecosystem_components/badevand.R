#---------------------- klfa -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs
target_crs <- 25832

# Indlæs grid og undersøgelsesområde fra de forskellige project paths

grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(., crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = st_area(.)) %>%
  st_drop_geometry(.)

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)

grid_raster <- terra::rast(file.path(PATHS$input_assessment_area, "/geotif/assessment_area.tif"))

badevand <- st_read(file.path(PATHS$input_ecosystem, "badevand\\EMODnet_HA_Environment_StatusBathingWater_20260116\\EMODnet_HA_Environment_BathingWaterSites_20260116.shp")) %>%
  st_transform(., crs = target_crs)


# Badevands kvalitet - diskuter om det skal bruges, for nu vises positionerne blot
bade_kval <- read_delim(file.path(PATHS$input_ecosystem, "badevand\\EMODnet_HA_Environment_StatusBathingWater_20260116\\EMODnet_HA_Environment_BathingWaterStatus_20260116.csv"),delim = ";", escape_double = FALSE, trim_ws = TRUE)

badevand_koge_kval <- st_intersection(badevand, grid) %>%
  st_drop_geometry(.) %>%
  left_join(bade_kval, by = c("bathingWat" = "BATHINGWATERIDENTIFIER"))

badevand_koge_norm <- badevand_koge_kval %>%
  mutate(
    pres_score = case_when(
      STATUS == "1 - Excellent"          ~ 0.0,
      STATUS == "2 - Good"               ~ 0.33,
      STATUS %in% c("3 - Good or Sufficient", 
                    "3 - Sufficient")     ~ 0.66,
      STATUS == "4 - Poor"               ~ 1.0,
      STATUS == "0 - Not classified"     ~ NA_real_
    )
  )

badevand_koge_mean <- badevand_koge_norm %>%
  filter(YEAR >= 2019, YEAR <= 2025) %>%
  group_by(bathingWat) %>%  
  summarise(
    pres_score_mean = mean(pres_score, na.rm = TRUE),
    n_years         = sum(!is.na(pres_score)),
    .groups         = "drop"
  ) 


## Konverter til raster - da det er punkt data tildeles en badevandsstation til den gridcelle den overlapper med
badevand_koge_kval <- st_intersection(badevand, grid) %>%
  st_drop_geometry(.) %>%
  left_join(grid) %>%
  mutate(value = 1) %>%
  st_as_sf()



bade_raster <- terra::rasterize(
  terra::vect(badevand_koge_kval),
  grid_raster,
  field      = "value",
  fun        = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = NA
)


terra::writeRaster(
  bade_raster,
  filename  = file.path(PATHS$output_ecosystem_tif, "\\badevand.tif"),
  overwrite = TRUE
)

# plotting
#Ved plotting anvendes punkt data til visualisering
badevand_koge_punkt <- st_intersection(badevand, grid) 


map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)



viridis_start_color <- viridis_pal()(1)  

map_badevand <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = badevand_koge_punkt,
          color = "yellow",
          size = 3) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_badevand

ggsave(plot = map_badevand,
       filename = file.path(PATHS$output_ecosystem_png, "\\badevand_punkt.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)


