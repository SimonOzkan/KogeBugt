#---------------------- bathing sites -----------------

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


# Raster med 250x250 opløsning for undersøgelsesområdet 
assessment_area_rast <- terra::rast(file.path(PATHS$input_assessment_area, "\\geotif\\assessment_area.tif"))

## EMODnet 
bath_quality <- st_read(file.path(PATHS$input_ecosystem, "badevand/EMODnet_HA_Environment_StatusBathingWater_20260116/EMODnet_HA_Environment_BathingWaterSites_20260116.shp")) %>%
  st_transform(., crs = target_crs)
