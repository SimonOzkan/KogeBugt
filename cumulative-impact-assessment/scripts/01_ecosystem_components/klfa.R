#---------------------- klfa -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

klfa_cv <- terra::rast(file.path(PATHS$input_ecosystem, "/klorofyl/_ags_chloro_a_cv_resample_250m/chloro_a_cv_resample_250m.tif"))
klfa_mean <-  terra::rast(file.path(PATHS$input_ecosystem, "/klorofyl/_ags_chloro_a_mean_resample_250m/chloro_a_mean_resample_250m.tif"))

klfa_mean <- terra::project(klfa_mean, crs(assessment_area_dissolved))

klfa_mean_cropped <- terra::crop(klfa_mean,assessment_area_dissolved) %>%
  terra::mask(assessment_area_dissolved)

plot(klfa_mean_cropped)

# intersect med undersøgelsesområdet 

klfa_koge <- terra::resample(klfa_mean_cropped,grid_raster)
names(klfa_koge) <- "value"


# log normalisering 
klfa_log <- log10(1+klfa_koge)  # 
klfa_log_min <- terra::global(klfa_log, "min", na.rm = TRUE)[1, 1]
klfa_log_max <- terra::global(klfa_log, "max", na.rm = TRUE)[1, 1]
klfa_norm <- (klfa_log - klfa_log_min) / (klfa_log_max - klfa_log_min)


terra::writeRaster(
  klfa_norm,
  filename = file.path(PATHS$output_ecosystem_tif, "/klfa_log_norm.tif"),
  overwrite = TRUE
)


# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
klfa_sf_log <- klfa_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_klfa <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = klfa_sf_log, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_klfa


ggsave(plot = map_klfa,
       filename = file.path(PATHS$output_ecosystem_png,"klfa.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)












