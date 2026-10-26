#---------------------- klfa -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

klfa_cv <- terra::rast(file.path(PATHS$input_ecosystem, "/klorofyl/_ags_chloro_a_cv_resample_250m/chloro_a_cv_resample_250m.tif"))
klfa_mean <-  terra::rast(file.path(PATHS$input_ecosystem, "/klorofyl/_ags_chloro_a_mean_resample_250m/chloro_a_mean_resample_250m.tif")) 

klfa_mean_koge <- terra::project(klfa_mean, grid_raster, method = "bilinear")

# intersect med undersøgelsesområdet 

names(klfa_mean_koge) <- "value"


# log normalisering 
klfa_log_koge <- log10(1+klfa_mean_koge)  # Log transformering
klfa_log_baltic_sea <- log10(1+klfa_mean)  # Log transformering


klfa_log_min <- terra::global(klfa_log_baltic_sea, "min", na.rm = TRUE)[1, 1]
klfa_log_max <- terra::global(klfa_log_baltic_sea, "max", na.rm = TRUE)[1, 1]
klfa_norm <- (klfa_log_koge - klfa_log_min) / (klfa_log_max - klfa_log_min)

klfa_norm_fill_na <- fill_na(klfa_norm, grid_raster, w = 3)


terra::writeRaster(
  klfa_norm_fill_na,
  filename = file.path(PATHS$output_ecosystem_tif, "/klfa_log_norm.tif"),
  overwrite = TRUE
)


# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
klfa_sf_log <- klfa_norm_fill_na %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot

# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_klfa <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
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











