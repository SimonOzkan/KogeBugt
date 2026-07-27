#---------------------- Kontinuerlig støj fra skibe -----------------

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


###
ves_dens <-terra::rast(file.path(PATHS$input_pressure, "/AIS skibe/emodnet/EMODnet_HA_Vessel_Density_allAvg/vesseldensity_all_2024.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

ves_dens_cropped <- terra::crop(ves_dens, assessment_area_vect) %>%
  terra::mask(assessment_area_vect)

plot(ves_dens)
