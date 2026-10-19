#---------------------- Fortidsminder -----------------
source("scripts/00_setup.R")
PATHS <- set_project_paths()


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


skibsvrag_koge <- st_intersection(skibsvrag_sf, assessment_area_dissolved)


skibsvrag_koge_buff <- skibsvrag_koge %>%
  st_buffer(., dist = 50) %>% 
  st_union(.) %>%
  st_as_sf() %>%
  st_make_valid()

skibsvrag_koge_grid <- st_intersection(skibsvrag_koge_buff, grid) %>%
  mutate(area_vrag = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_vrag = sum(as.numeric(area_vrag), na.rm = TRUE), .groups = "drop")

vrag_frac <-  left_join(grid, skibsvrag_koge_grid) %>%
  filter(!is.na(area_vrag)) %>%
  mutate(value = area_vrag / area_grid )


## ------------------------------------------------------------------
## 3. Gem som raster
## ------------------------------------------------------------------


skibsvrag_koge_rast <- terra::rasterize(
  terra::vect(vrag_frac),
  grid_raster,
  field      = "value",
  fun        = "max",
  background = 0
)

skibsvrag_masked <- terra::mask(skibsvrag_koge_rast, grid_raster)
plot(skibsvrag_masked)

terra::writeRaster(
  skibsvrag_koge_rast,
  filename  = file.path(PATHS$output_ecosystem_tif,"fortid" ,"skibsvrag.tif"),
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
          size =1.5) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_skibsvrag

ggsave(plot = map_skibsvrag,
       filename = file.path(PATHS$output_ecosystem_png, "fortid","skibsvrag_punkt.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
