#---------------------- Badevand og badepladser -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

badevand <- st_read(file.path(PATHS$input_ecosystem, "badevand\\EMODnet_HA_Environment_StatusBathingWater_20260116\\EMODnet_HA_Environment_BathingWaterSites_20260116.shp")) %>%
  st_transform(., crs = target_crs)


# Badevands kvalitet - diskuter om det skal bruges, for nu vises positionerne blot
bade_kval <- read_delim(file.path(PATHS$input_ecosystem, "badevand\\EMODnet_HA_Environment_StatusBathingWater_20260116\\EMODnet_HA_Environment_BathingWaterStatus_20260116.csv"),delim = ";", escape_double = FALSE, trim_ws = TRUE)

bbox_square <- st_read(file.path(PATHS$input_assessment_area,"bbox_omraade","koge_bugt_bbox.shp")) %>%
  st_transform(crs = target_crs)

badevand_koge_kval <- st_intersection(badevand,bbox_square) %>%
  left_join(bade_kval, by = c("bathingWat" = "BATHINGWATERIDENTIFIER"))

badevand_koge_norm <- badevand_koge_kval %>%
  mutate(
    pres_score = case_when(
      STATUS == "1 - Excellent"          ~ 1,
      STATUS == "2 - Good"               ~ 2,
      STATUS %in% c("3 - Good or Sufficient", 
                    "3 - Sufficient")     ~ 3,
      STATUS == "4 - Poor"               ~ 4,
      STATUS == "0 - Not classified"     ~ NA_real_
    )
  )

badevand_koge_mean <- badevand_koge_norm %>%
  filter(YEAR >= 2018) %>%
  group_by(bathingWat) %>%  
  summarise(
    pres_score_mean = mean(pres_score, na.rm = TRUE),
    n_years         = sum(!is.na(pres_score)),
    .groups         = "drop"
  ) %>%
  st_buffer(dist = 150)


## Konverter til raster - da det er punkt data tildeles en badevandsstation til den gridcelle den overlapper med


badevand_koge_kval <- st_intersection(grid_sf,badevand_koge_mean) %>%
  st_drop_geometry() %>%
  filter(!is.na(pres_score_mean)) %>%
  mutate(value = (pres_score_mean-4) / (1-4)) # normalised value, where 1 is best and 4 is worst

badevand_koge_kval_gridded <- left_join(grid_sf,badevand_koge_kval) %>%  
  mutate(value = tidyr::replace_na(value,0))



bade_raster <- terra::rasterize(
  terra::vect(badevand_koge_kval_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = NA
)

plot(bade_raster)

terra::writeRaster(
  bade_raster,
  filename  = file.path(PATHS$output_ecosystem_tif, "kyst_badevand","badevand.tif"),
  overwrite = TRUE
)


# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
bade_sf <- bade_raster %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot


map_badevand <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = bade_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_badevand

gem_trim(map_badevand,
         file.path(PATHS$output_ecosystem_png, "kyst_badevand","badevand.png"),
         dpi = 360, width_cm = 18)

