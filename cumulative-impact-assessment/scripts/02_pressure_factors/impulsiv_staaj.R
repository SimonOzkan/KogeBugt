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
## 3. Maskér til assessment-området
## ------------------------------------------------------------------

stoj_imp_koge <- terra::mask(stoj_imp_250, assessment_area_vect)


## ------------------------------------------------------------------
## 4. Re-normalisér 0-1 inden for området
## ------------------------------------------------------------------

stoj_imp_norm <- terra::scale_linear(stoj_imp_koge)
names(stoj_imp_norm) <- "value"

plot(stoj_imp_norm)
terra::writeRaster(stoj_imp_norm,
                   file.path(PATHS$output_pressure_tif, "stoj_impulsiv_norm.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 5. Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

stoj_imp_sf <- stoj_imp_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_stoj_imp <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = stoj_imp_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_stoj_imp

ggsave(plot = map_stoj_imp,
       filename = file.path(PATHS$output_pressure_png, "lyd_energi","stoj_impulsiv.png"),
       bg = "white", height = 18, width = 18, dpi = 300)