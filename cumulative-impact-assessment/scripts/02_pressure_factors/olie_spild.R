#---------------------- Oliespild (HELCOM HOLAS 3-metode) -----------------
# Indlæs pakker, assessment grid og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()

## ------------------------------------------------------------------
##  Indlæs ulovlige olieudledninger (EMODnet / IllegalOilDischarges)
## ------------------------------------------------------------------

olie_spild <- st_read(file.path(PATHS$input_pressure,
                                "olie",
                                "_ags_IllegalOilDischarges_project",
                                "IllegalOilDischarges_project.shp")) %>%
  st_transform(crs = target_crs)

#--------- intersection med undersøgelsesområdet
olie_koge <- st_intersection(olie_spild, assessment_area_dissolved) %>%
  filter(Year %in% c(2019,2020,2023,2024,2025,2026))


## ------------------------------------------------------------------
## Middelværdier til imputering (HELCOM-metode)
## ------------------------------------------------------------------
# HELCOM: manglende/0 volumen -> gennemsnit af de kendte; manglende/0 areal -> ligeså.
# "hele datalaget" = hele olie_spild (før intersection/år-filter). Vil du hellere
# bruge det lokale gennemsnit for Køge Bugt, så skift olie_spild ud med olie_koge.
mean_vol  <- mean(olie_spild$EstimVol_m[olie_spild$EstimVol_m > 0], na.rm = TRUE)
mean_area <- mean(olie_spild$Area__km2_[olie_spild$Area__km2_ > 0], na.rm = TRUE)

mean_vol
mean_area
## ------------------------------------------------------------------
##  HELCOM-logik pr. spild: olie pr. km2 + cirkelradius
## ------------------------------------------------------------------
# - Manglende/0 volumen og areal erstattes af datalagets gennemsnit.
# - eff_area = max(areal, 1): spild <=1 km2 lægges i 1 km2 (som HELCOM's 1 km2-celle);
#   spild >1 km2 bruger sit eget areal.
# - oil_km2 = volumen / eff_area  ->  estimeret mængde olie pr. km2 (fordeles jævnt).
# - radius bagud-beregnet fra eff_area (cirkulær buffer).

olie <- olie_koge %>%
  mutate(
    vol_m3       = ifelse(is.na(EstimVol_m) | EstimVol_m == 0, mean_vol,  EstimVol_m),
    area_km2     = ifelse(is.na(Area__km2_) | Area__km2_ == 0, mean_area, Area__km2_),
    eff_area_km2 = pmax(area_km2, 1),
    oil_km2      = vol_m3 / eff_area_km2,
    radius_m     = sqrt(eff_area_km2 * 1e6 / pi)
  )


## ------------------------------------------------------------------
## Cirkulær buffer pr. spild (spild-specifik radius; oil_km2 bevares)
## ------------------------------------------------------------------
# NB: IKKE st_union - hvert spild bærer sin egen oil_km2-værdi, som skal med til
# rasteriseringen.

olie_buffer <- st_buffer(olie, dist = olie$radius_m)


## ------------------------------------------------------------------
## Rasterisér olie/km2 til 250 m grid (sum ved overlap) og maskér
## ------------------------------------------------------------------
# fun = "sum": hvor spild overlapper, akkumuleres olie/km2 (mere olie = større pres).

olie_rast <- terra::rasterize(
  terra::vect(olie_buffer),
  grid_raster,
  field      = "oil_km2",
  fun        = "sum",
  background = 0
) %>%
  terra::mask(assessment_area_vect)


## ------------------------------------------------------------------
## 6. Log-transformér og normalisér til 0-1
## ------------------------------------------------------------------
# Olie/km2 er kraftigt højreskæv -> log FØR normalisering (som HELCOM).

olie_norm <- terra::scale_linear(log1p(olie_rast))
names(olie_norm) <- "value"


## ------------------------------------------------------------------
## 7. Gem rastere
## ------------------------------------------------------------------

terra::writeRaster(olie_rast, file.path(PATHS$output_pressure_tif, "miljøfremmede","oliespild_oil_per_km2.tif"),
                   overwrite = TRUE)
terra::writeRaster(olie_norm, file.path(PATHS$output_pressure_tif, "miljøfremmede","oliespild_norm.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## 8. Plotting for bilag
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

olie_sf <- olie_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_olie <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = olie_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_olie

ggsave(plot = map_olie,
       filename = file.path(PATHS$output_pressure_png, "miljøfremmede","oliespild.png"),
       bg = "white",
       height = 18,
       width = 18,
       dpi = 300)
