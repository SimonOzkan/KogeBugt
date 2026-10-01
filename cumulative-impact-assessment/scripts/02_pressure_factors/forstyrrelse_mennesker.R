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
## 3. Maskér til assessment-området
## ------------------------------------------------------------------

FDHP_koge <- terra::mask(FDHP_250, assessment_area_vect)


## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------
# scale_linear bruger nu kun celler i Køge Bugt (fordi vi har maskeret først).

FDHP_norm <- terra::scale_linear(FDHP_koge)
names(FDHP_norm) <- "value"

plot(FDHP_norm)
terra::writeRaster(FDHP_norm,
                   file.path(PATHS$output_pressure_tif, "forstyrrelse_tilstedevaerelse_norm.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

FDHP_sf <- FDHP_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_FDHP <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = FDHP_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_FDHP

ggsave(plot = map_FDHP,
       filename = file.path(PATHS$output_pressure_png, "forstyrrelse_tilstedevaerelse.png"),
       bg = "white", height = 18, width = 18, dpi = 300)
