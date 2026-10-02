#---------------------- Fuglejagt (HELCOM HOLAS 3) -----------------
# HELCOM-lag for jagt på fugle (PL_15). Bemærk: jagt og fritidsfiskeri blev
# BEVIDST udeladt af HELCOM's "human presence"-forstyrrelseslag, fordi de
# rapporteres på landeniveau og ville overestimere presset - derfor er dette
# et SELVSTÆNDIGT lag, og der er ingen dobbelttælling med forstyrrelseslaget.
#
# Laget dækker hele Østersøen (EPSG:3857, 1 km) og er allerede normaliseret.
# Vi re-normaliserer 0-1 INDEN FOR Køge Bugt, så det flugter med de øvrige lag.
# Normalisering sker EFTER maskering, så min/max kun kommer fra området.

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()


## ------------------------------------------------------------------
## 1. Indlæs HELCOM-lag (hele Østersøen, EPSG:3857, 1 km)
## ------------------------------------------------------------------

fuglejagt <- terra::rast(file.path(PATHS$input_pressure,
                                   "kystrekreative",
                                   "_ags_PL_15",
                                   "PL_15.tif"))


## ------------------------------------------------------------------
## 2. Projicér til 250 m grid (1 km -> 250 m, bilinear)
## ------------------------------------------------------------------
# project(x, grid_raster) reprojicerer (3857 -> target_crs) OG aligner til
# 250 m-grid'et i ét trin. bilinear, da laget er et kontinuert indeks.

fuglejagt_250 <- terra::project(fuglejagt, grid_raster, method = "bilinear")


## ------------------------------------------------------------------
## 3. Maskér til assessment-området
## ------------------------------------------------------------------

fuglejagt_koge <- terra::mask(fuglejagt_250, assessment_area_vect)


## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------
# scale_linear bruger nu kun celler i Køge Bugt (fordi vi har maskeret først).

fuglejagt_norm <- terra::scale_linear(fuglejagt_koge)
names(fuglejagt_norm) <- "value"

plot(fuglejagt_norm)
terra::writeRaster(fuglejagt_norm,
                   file.path(PATHS$output_pressure_tif, "fuglejagt_norm.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

fuglejagt_sf <- fuglejagt_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_fuglejagt <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = fuglejagt_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_fuglejagt

ggsave(plot = map_fuglejagt,
       filename = file.path(PATHS$output_pressure_png, "fuglejagt.png"),
       bg = "white", height = 18, width = 18, dpi = 300)