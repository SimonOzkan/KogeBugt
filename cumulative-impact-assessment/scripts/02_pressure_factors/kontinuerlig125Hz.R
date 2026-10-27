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

terra::global(stoj_kont_250, max, na.rm = TRUE)

## ------------------------------------------------------------------
## 3. Maskér til assessment-området
## ------------------------------------------------------------------

stoj_kont_koge <- terra::mask(stoj_kont_250, grid_raster)


## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------

stoj_kont_koge_fill <- fill_na(stoj_kont_koge, grid_raster, w = 3)

plot(stoj_kont_koge_fill)

terra::writeRaster(stoj_kont_koge_fill,
                   file.path(PATHS$output_pressure_tif, "stoj_kontinuerlig.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------


stoj_kont_sf <- stoj_kont_koge_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  rename("value" = "PL_04")

map_stoj_kont <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = stoj_kont_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_stoj_kont



gem_trim(map_stoj_kont,
         file.path(PATHS$output_pressure_png, "lyd_energi\\stoj_kontinuerlig.png"),
         dpi = 360, width_cm = 18)


