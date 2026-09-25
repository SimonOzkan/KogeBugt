#-------------------------------- Dumpet kemisk ammunition --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()

## ------------------------------------------------------------------
##  Indlæs ammunitiontærområder (se data folder / metadata_log)
## ------------------------------------------------------------------

ammunition <- st_read(file.path(PATHS$input_pressure,
                          "affald",
                          "EMODnet_HA_WasteDisposal_DumpedMunitions_20260609",
                          "EMODnet_HA_WasteDisposal_DumpedMunitions_pg_20260609.shp")) %>%
  st_transform(., crs = target_crs) %>%
  st_make_valid() %>%
  dplyr::distinct(.)


## ------------------------------------------------------------------
## Intersection med grid
## ------------------------------------------------------------------

ammunition_oresund <- st_intersection(ammunition, grid) %>%
  mutate(area_ammunition = st_area(.)) 


ammunition_area <- ammunition_oresund %>%
  mutate(value = as.numeric(area_ammunition) / as.numeric(area_grid),
         value = pmin(value, 1)) %>%
  st_make_valid()


## ------------------------------------------------------------------
## Konverter til raster og gem
## ------------------------------------------------------------------


ammunition_rast <- terra::rasterize(
  terra::vect(ammunition_area),
  grid_raster,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

plot(ammunition_rast)

terra::writeRaster(
  ammunition_rast,
  filename  = file.path(PATHS$output_pressure_tif, "affald","ammunitiontaeromraader.tif"),
  overwrite = TRUE
)


## ------------------------------------------------------------------
## 4  Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)


map_ammunition <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = ammunition_area, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_ammunition

ggsave(plot = map_ammunition,
       filename = file.path(PATHS$output_pressure_png, "affald","ammunitiontaeromraader.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
