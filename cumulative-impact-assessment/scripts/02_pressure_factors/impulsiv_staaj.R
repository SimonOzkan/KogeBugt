#---------------------- Impulsiv menneskeskabt støj (HELCOM HOLAS 3) -----------------
# HELCOM "Input of impulsive anthropogenic sound" - impulsive hændelser 2016-2021:
# seismiske undersøgelser, sprængninger, pæleramning og airguns (HELCOM-OSPAR
# Impulsive noise registry). Hver hændelsestype er tildelt en numerisk
# intensitetsværdi (Very low 0.25, Low 0.5, Medium 0.75, High 1), og laget er
# normaliseret af HELCOM. Påvirkningsafstand er IKKE indregnet i kildelaget.
#
# Vi re-normaliserer 0-1 INDEN FOR Køge Bugt (scale_linear EFTER maskering).

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()


## ------------------------------------------------------------------
## 1. Indlæs HELCOM-lag
## ------------------------------------------------------------------

stoj_imp <- terra::rast(file.path(PATHS$input_pressure,
                                  "lyd_energi",       
                                  "_ags_PL_05",           
                                  "PL_05.tif"))         


## ------------------------------------------------------------------
## 2. Projicér til 250 m grid
## ------------------------------------------------------------------
# NB: impulsiv-laget stammer fra kategoriske intensitetsklasser (0.25/0.5/0.75/1).
# Som leveret HELCOM-raster er det normaliseret; bilinear bruges for konsistens
# med de øvrige lag. Vil du bevare de oprindelige klassegrænser skarpt, så skift
# til method = "near" (ingen blanding mellem klasser).

stoj_imp_250 <- terra::project(stoj_imp, grid_raster, method = "bilinear")


## ------------------------------------------------------------------


stoj_imp_250_fill <- fill_na(stoj_imp_250, grid_raster, w = 3)
plot(stoj_imp_250_fill)


terra::writeRaster(stoj_imp_250_fill,
                   file.path(PATHS$output_pressure_tif,"lyd_energi", "stoj_impulsiv.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------


stoj_imp_sf <- stoj_imp_250_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  rename("value" = "PL_05")

map_stoj_imp <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = stoj_imp_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_stoj_imp


gem_trim(map_stoj_imp,
         file.path(PATHS$output_pressure_png, "lyd_energi", "impulsiv.png"),
         dpi = 360, width_cm = 18)
