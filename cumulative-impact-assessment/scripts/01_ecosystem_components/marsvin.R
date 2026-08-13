#---------------------- Områder egnet til Marsvin -----------------
# EMODnet datalag
# https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/6d617269-6e65-696e-666f-000000008820 
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

grid_raster <- terra::rast(file.path(PATHS$input_assessment_area, "/geotif/assessment_area.tif"))

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)
#####
marsvin_nc_path <- file.path(PATHS$input_ecosystem,"/Pattedyr/porpoiseHabitatSuitability_with_SeasonYears.nc")

marsvin_r <- terra::rast(marsvin_nc_path, subds = "habitat_suitability")

# Behold kun lag 5 (Summer 2013-2022) og 6 (Winter 2013-2022)
r_2013 <- marsvin_r[[c(5, 6)]]
names(r_2013) <- c("summer_2013_2022", "winter_2013_2022")

plot(r_2013[["summer_2013_2022"]])

r_mean_2013_2022 <- (r_2013[["summer_2013_2022"]]+r_2013[["winter_2013_2022"]])/2
names(r_mean_2013_2022) <- "value"
r_mean_2013_2022 <- terra::project(r_mean_2013_2022,crs(assessment_area_dissolved))

marsvin_cropped <- terra::crop(r_mean_2013_2022, assessment_area_dissolved) %>%
  terra::mask(assessment_area_dissolved)


marsvin_250 <- terra::resample(marsvin_cropped,grid_raster, method = "bilinear")

terra::writeRaster(
  marsvin_250,
  filename = file.path(PATHS$output_ecosystem_tif,"pattedyr", "/marsvin_250.tif"),
  overwrite = TRUE
)

# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
marsvin_sf <- marsvin_cropped %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_marsvin <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = marsvin_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_marsvin


ggsave(plot = map_marsvin,
       filename = file.path(PATHS$output_ecosystem_png,"/pattedyr/" ,"marsvin.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)




