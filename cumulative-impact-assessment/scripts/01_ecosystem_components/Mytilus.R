#---------------------- Blåmuslinger (Mytilus) -----------------

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

# Raster med 250x250 opløsning for undersøgelsesområdet (bruges som resample-template)
assessment_area_rast <- terra::rast(file.path(PATHS$input_assessment_area, "\\geotif\\assessment_area.tif"))


## ------------------------------------------------------------------
## 1. Mytilus HELCOM (present/absent)
## ------------------------------------------------------------------

mytilus <- terra::rast(file.path(PATHS$input_ecosystem, "\\Mytilus\\_ags_EC_24\\EC_24.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

# Klip til undersøgelsesområdet
mytilus_cropped <- terra::crop(mytilus, assessment_area_vect) %>%
  terra::mask(assessment_area_vect)

# Resample fra 500x500 til 250x250 opløsning, så det matcher de øvrige lag
mytilus_resampled <- terra::resample(mytilus_cropped, assessment_area_rast, method = "near")

plot(mytilus_resampled, main = "Mytilus (præ-dybdefiltrering)")


## ------------------------------------------------------------------
## 2. Dybdefiltrering (max 10 m)
## ------------------------------------------------------------------

# Metadata angiver ikke en dybdegrænse for Mytilus, men kilder viser de maks. lever på 10 m dybde
z <- terra::rast(file.path(PATHS$input_phys_chem_geo, "/_ags_depth_250m/depth_250m.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

z_10m <- terra::clamp(z, upper = 10, value = FALSE)

z_10m_cropped <- terra::crop(z_10m, assessment_area_vect) %>%
  terra::mask(assessment_area_vect) %>%
  terra::resample(assessment_area_rast, method = "bilinear")

plot(z_10m_cropped, main = "Dybde (klampet til <= 10 m)")


## ------------------------------------------------------------------
## 3. Anvend dybdemaske på Mytilus-lag
## ------------------------------------------------------------------

mytilus_z10 <- mytilus_resampled %>%
  terra::mask(z_10m_cropped) %>%
  terra::classify(cbind(0, NA))

plot(mytilus_z10, main = "Mytilus (dybdefiltreret, <= 10 m)")


## ------------------------------------------------------------------
## 4. Konverter til polygon og beregn arealfraktion per gridcelle
##    (samme princip som Natura2000/Rev/Zostera)
## ------------------------------------------------------------------

mytilus_poly <- mytilus_z10 %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# Overlap mellem tilstødende raster-celler håndteres ved union, så areal ikke tælles dobbelt
mytilus_union <- mytilus_poly %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf()

mytilus_intersect <- st_intersection(mytilus_union, grid) %>%
  st_make_valid()

mytilus_area <- mytilus_intersect %>%
  mutate(area_mytilus = st_area(.)) %>%
  left_join(grid_area, by = "id") %>%
  mutate(value = as.numeric(area_mytilus) / as.numeric(area_grid),
         value = pmin(value, 1))


## ------------------------------------------------------------------
## 5. Rasterize og gem .tif
## ------------------------------------------------------------------

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

mytilus_rast <- terra::rasterize(
  terra::vect(mytilus_area),
  r_template,
  field      = "value",
  fun        = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = NA
)

plot(mytilus_rast, main = "Mytilus arealfraktion")

terra::writeRaster(
  mytilus_rast,
  filename  = file.path(PATHS$output_ecosystem_tif, "\\Mytilus.tif"),
  overwrite = TRUE
)


############### Plotting for bilag ################


map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)


viridis_start_color <- viridis_pal()(1)  

map_mytilus <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = mytilus_area,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_mytilus

ggsave(plot = map_mytilus,
       filename = file.path(PATHS$output_ecosystem_png, "\\Mytilus.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
