#-------------------------------- ikke hjemmehørende arter --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()

# ── Hent NIS fra HELCOM ───────────────────────────────────────────────────
# Fra 500x500 res til 250x250 med bilinear metode
NIS_250 <- terra::rast(file.path(PATHS$input_pressure, "NIS","_ags_PL_17","PL_17.tif")) %>%
  terra::project(grid_raster, method = "bilinear") 


NIS_250_fill_na <- fill_na(NIS_250, grid_raster, w = 3)

names(NIS_250_fill_na) <- "value"





# gem som tif
terra::writeRaster(
  NIS_250_fill_na,
  filename = file.path(PATHS$output_pressure_tif,"NIS", "/NIS_250.tif"),
  overwrite = TRUE
)

# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
NIS_sf <- NIS_250_fill_na %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
map_NIS <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = NIS_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_NIS


gem_trim(map_NIS,
         file.path(PATHS$output_pressure_png, "NIS\\NIS.png"),
         dpi = 360, width_cm = 18)



