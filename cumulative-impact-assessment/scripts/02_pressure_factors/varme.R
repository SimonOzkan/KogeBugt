#---------------------- Tilførsel af varme (HELCOM HOLAS 3) -----------------
# HELCOM-lag for tilførsel af varme / termisk påvirkning (PL_06).
# Laget dækker hele Østersøen (EPSG:3857, 1 km) og er allerede normaliseret.
# Vi re-normaliserer 0-1 INDEN FOR Køge Bugt, så det flugter med de øvrige lag.
# Normalisering sker EFTER maskering, så min/max kun kommer fra området.

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()


## ------------------------------------------------------------------
## 1. Indlæs HELCOM-lag (hele Østersøen, EPSG:3857, 1 km)
## ------------------------------------------------------------------

varme <- terra::rast(file.path(PATHS$input_pressure,
                               "varme",
                               "_ags_PL_06",
                               "PL_06.tif"))


## ------------------------------------------------------------------
## 2. Projicér til 250 m grid (1 km -> 250 m, bilinear)
## ------------------------------------------------------------------
# project(x, grid_raster) reprojicerer (3857 -> target_crs) OG aligner til
# 250 m-grid'et i ét trin. bilinear, da laget er et kontinuert indeks.

varme_250 <- terra::project(varme, grid_raster, method = "bilinear")


## ------------------------------------------------------------------
## 3. Maskér til assessment-området
## ------------------------------------------------------------------

varme_koge <- terra::mask(varme_250, assessment_area_vect)


## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------
# scale_linear bruger nu kun celler i Køge Bugt (fordi vi har maskeret først).

varme_norm <- terra::scale_linear(varme_koge)
names(varme_norm) <- "value"

plot(varme_norm)
terra::writeRaster(varme_norm,
                   file.path(PATHS$output_pressure_tif, "varme_norm.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

varme_sf <- varme_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_varme <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = varme_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_varme

ggsave(plot = map_varme,
       filename = file.path(PATHS$output_pressure_png, "varme.png"),
       bg = "white", height = 18, width = 18, dpi = 300)