#---------------------- Tilførsel af varme (HELCOM HOLAS 3) -----------------
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

varme_koge <- fill_na(varme_250, grid_raster, w = 3)

## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------
# scale_linear bruger nu kun celler i Køge Bugt

names(varme_koge) <- "value"

plot(varme_koge)
terra::writeRaster(varme_koge,
                   file.path(PATHS$output_pressure_tif, "varme_tilførelse.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------


varme_sf <- varme_koge %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_varme <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = varme_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_varme


gem_trim(map_varme,
         file.path(PATHS$output_pressure_png, "varme_tilførelse.png"),
         dpi = 360, width_cm = 18)
