#---------------------- Forstyrrelse fra menneskelig tilstedeværelse (HELCOM HOLAS 3) -----------------
# HELCOM "Disturbance of species due to human presence" (PL_11).
# Aggregeret HOLAS 3-lag: rekreativ sejlads (SHEBA, dybdevægtet) + badestrande
# + bynær arealanvendelse (udvidet 1 km mod hav). Lige vægt, summeret,
# dybdevægtet, log-transformeret og normaliseret af HELCOM.
#
# Laget er allerede normaliseret 0-1 for HELE Østersøen. Vi re-normaliserer 0-1
# INDEN FOR Køge Bugt, så det flugter med de øvrige lag (der bruger scale_linear
# lokalt). NB: normalisering sker EFTER maskering, så min/max kun kommer fra
# assessment-området

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()


## ------------------------------------------------------------------
## 1. Indlæs HELCOM-lag (hele Østersøen, EPSG:3857, 1 km)
## ------------------------------------------------------------------

FDHP <- terra::rast(file.path(PATHS$input_pressure,
                              "kystrekreative",
                              "_ags_PL_11",
                              "PL_11.tif"))


## ------------------------------------------------------------------
## 2. Projicér til 250 m grid (1 km -> 250 m, bilinear)
## ------------------------------------------------------------------
# project(x, grid_raster) reprojicerer (3857 -> target_crs) OG aligner til
# 250 m-grid'et i ét trin. bilinear, da laget er et kontinuert indeks.

FDHP_250 <- terra::project(FDHP, grid_raster, method = "bilinear")


## ------------------------------------------------------------------
## 3. Fill evt na og masker og gem
## ------------------------------------------------------------------

FDHP_koge <- fill_na(FDHP_250, grid_raster, w = 3)

names(FDHP_koge) <- "value"

plot(FDHP_koge)
terra::writeRaster(FDHP_koge,
                   file.path(PATHS$output_pressure_tif, "mennesker_fysisk_forstyrrelse.png"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------

FDHP_sf <- FDHP_koge %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_FDHP <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = FDHP_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_FDHP

gem_trim(map_FDHP,
         file.path(PATHS$output_pressure_png, "mennesker_fysisk_forstyrrelse.png"),
         dpi = 360, width_cm = 18)

