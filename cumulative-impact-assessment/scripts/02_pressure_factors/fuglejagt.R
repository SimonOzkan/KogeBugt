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
## 3. Fill na og masker
## ------------------------------------------------------------------

fuglejagt_koge <- fill_na(fuglejagt_250, grid_raster, w = 3)


## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------
# scale_linear bruger nu kun celler i Køge Bugt (fordi vi har maskeret først).


names(fuglejagt_koge) <- "value"

plot(fuglejagt_koge)
terra::writeRaster(fuglejagt_koge,
                   file.path(PATHS$output_pressure_tif, "fuglejagt","fuglejagt_koge.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------


fuglejagt_sf <- fuglejagt_koge %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_fuglejagt <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = fuglejagt_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_fuglejagt


gem_trim(map_fuglejagt,
         file.path(PATHS$output_pressure_png, "fuglejagt\\fuglejagt.png"),
         dpi = 360, width_cm = 18)
