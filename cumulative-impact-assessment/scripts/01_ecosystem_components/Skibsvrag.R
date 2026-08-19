#---------------------- Fortidsminder -----------------
source("scripts/00_setup.R")
PATHS <- set_project_paths()
target_crs <- 25832

# Indlæs grid og undersøgelsesområde
grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = as.numeric(st_area(.))) %>%
  st_drop_geometry()

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)
grid_raster <- terra::rast(file.path(PATHS$input_assessment_area, "/geotif/assessment_area.tif"))

## ------------------------------------------------------------------
## 1. Indlæs skibsvrag
## ------------------------------------------------------------------

skibsvrag <- terra::vect(file.path(PATHS$input_ecosystem, "\\fortidsminder\\EMODnet_HA_Heritage_WW_Wrecks_20241226\\EMODnet_HA_Heritage_WW_Wrecks_20241226.gdb"))

# Konverter til sf

skibsvrag_sf <- skibsvrag %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_transform(crs = target_crs)

## ------------------------------------------------------------------
## 2. Klip til undersøgelsesområdet
## ------------------------------------------------------------------

skibsvrag_koge <- st_intersection(skibsvrag_sf, grid) %>%
  mutate(value = 1)

message("Antal skibsvrag i undersøgelsesområdet: ", paste(nrow(skibsvrag_koge)))

## ------------------------------------------------------------------
## 3. Gem som raster
## ------------------------------------------------------------------


skibsvrag_koge_rast <- terra::rasterize(
  terra::vect(skibsvrag_koge),
  grid_raster,
  field      = "value",
  fun        = "max",
  background = NA
)

plot(skibsvrag_koge_rast)

terra::writeRaster(
  skibsvrag_koge_rast,
  filename  = file.path(PATHS$output_ecosystem_tif, "\\skibsvrag.tif"),
  overwrite = TRUE
)

## ------------------------------------------------------------------
## 6. Plot
## ------------------------------------------------------------------
#Ved plotting anvendes punkt data til visualisering

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)



viridis_start_color <- viridis_pal()(1)  

map_skibsvrag <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = skibsvrag_koge,
          color = "yellow",
          size = 3) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_skibsvrag

ggsave(plot = map_skibsvrag,
       filename = file.path(PATHS$output_ecosystem_png, "\\skibsvrag_punkt.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
