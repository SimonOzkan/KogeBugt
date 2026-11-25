#---------------------- Militær områder (polygonie) -----------------
# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()

## ------------------------------------------------------------------
## 1 Indlæs militærområder (se data folder / metadata_log)
## ------------------------------------------------------------------

max_dist <- 13.4*10^3

mili <- st_read(file.path(PATHS$input_pressure,
                          "military",
                          "EMODnet_HA_MilitaryAreas_20250314",
                          "EMODnet_HA_MilitaryAreas_pg_20250314.shp")) %>%
  st_transform(., crs = target_crs) %>%
  dplyr::distinct(.) %>%
  st_make_valid() 


## ------------------------------------------------------------------
## 2 Intersection med grid
## ------------------------------------------------------------------

mili_koge <- st_intersection(mili, assessment_area_dissolved) %>%
  st_make_valid()

## sf til raster
# Celler som en bro rører = 1, øvrige celler i området = 0, udenfor = NA
sf_til_rast <- function(x) {
  x %>%
    terra::vect() %>%
    terra::rasterize(grid_raster, field = 1, touches = TRUE, background = 0) %>%
    terra::mask(grid_raster)
}


mili_rast <- sf_til_rast(mili_koge)
plot(mili_rast)

mili_decay <- decay_focal(mili_rast, max_dist)
plot(mili_decay)

terra::writeRaster(
  mili_decay,
  filename  = file.path(PATHS$output_pressure_tif, "restriktive_omraader","militaeromraader_decay.tif"),
  overwrite = TRUE
)


## ------------------------------------------------------------------
## 4  Plotting for bilag
## ------------------------------------------------------------------

mili_rast_sf <- mili_rast %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = "layer")



map_mili <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = mili_rast_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_mili

gem_trim(map_mili,
         file.path(PATHS$output_pressure_png, "restriktive_omraader","militaer.png"),
         dpi = 360, width_cm = 18)
