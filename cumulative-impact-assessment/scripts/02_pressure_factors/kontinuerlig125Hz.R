#---------------------- Kontinuerlig menneskeskabt støj (HELCOM HOLAS 3) -----------------
# HELCOM "Input of continuous anthropogenic sound" - baseline excess level.
# Baseline excess SPL i 1/3-oktavbåndet ved 125 Hz, overskredet mindst 50 % af
# tiden i hele vandsøjlen, årsværdier 2018. Modelleret i 0,4 km x 0,4 km grid,
# allerede normaliseret af HELCOM.
#
# Vi re-normaliserer 0-1 INDEN FOR Køge Bugt (scale_linear EFTER maskering),
# så laget flugter med de øvrige presfaktor-lag.

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()


## ------------------------------------------------------------------
## 1. Indlæs HELCOM-lag (0,4 km grid)
## ------------------------------------------------------------------

stoj_kont <- terra::rast(file.path(PATHS$input_pressure,
                                   "lyd_energi",     
                                   "_ags_PL_04",           
                                   "PL_04.tif")) 


## ------------------------------------------------------------------
## 2. Projicér til 250 m grid (0,4 km -> 250 m, bilinear)
## ------------------------------------------------------------------
# Kontinuert SPL-indeks -> bilinear er passende.

stoj_kont_250 <- terra::project(stoj_kont, grid_raster, method = "bilinear")


## ------------------------------------------------------------------
## 3. Maskér til assessment-området
## ------------------------------------------------------------------

stoj_kont_koge <- terra::mask(stoj_kont_250, assessment_area_vect)


## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------

stoj_kont_norm <- terra::scale_linear(stoj_kont_koge)
names(stoj_kont_norm) <- "value"

plot(stoj_kont_norm)
terra::writeRaster(stoj_kont_norm,
                   file.path(PATHS$output_pressure_tif, "stoj_kontinuerlig_norm.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

stoj_kont_sf <- stoj_kont_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_stoj_kont <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = stoj_kont_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_stoj_kont

ggsave(plot = map_stoj_kont,
       filename = file.path(PATHS$output_pressure_png, "lyd_energi","stoj_kontinuerlig.png"),
       bg = "white", height = 18, width = 18, dpi = 300)
