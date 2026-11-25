#---------------------------- Kabler og gasrør (decay) ------------------------
# Kabler (MST + Energinet) og olie/gas-rørledninger (EMODnet).
# Celler som en linje rører får værdien 1, hvorfra presset aftager lineært til 0
# ved effektdistancen. Overlappende effektzoner håndteres med max (ingen addition).
#------------------------------------------------------------------------------

source("scripts/00_setup.R")   # pakker, grid, decay_focal()
PATHS <- set_project_paths()


## Parametre ------------------------------------------------------------------

max_dist_kabler <- 100   # m, ØKOMAR "Sea cables" (mean 0.10 km)
max_dist_gasroer <- 200  # m, ØKOMAR "Oil and gas pipelines" (mean 0.2 km)


## WFS-kilder -----------------------------------------------------------------

wfs_url_anlaeg_linje  <- "https://gisportal.mst.dk/server/services/ekstern/KDI_anlaeg_paa_soeterritoriet/MapServer/WFSServer"
wfs_url_el_kabel      <- "https://agis.energinet.dk/server/services/INSPIRE/XP_el_Inspir/MapServer/WFSServer"
wfs_url_olie_gas_pipe <- "https://ows.emodnet-humanactivities.eu/wfs"


## 1. Hent data ---------------------------------------------------------------

anlaeg_linje <- st_read(paste0(wfs_url_anlaeg_linje,
                               "?service=WFS&version=2.0.0&request=GetFeature",
                               "&typeNames=KDI_anlaeg_paa_soeterritoriet:Anlaeg_paa_soeterritoriet__linje_",
                               "&outputFormat=GEOJSON"), quiet = TRUE) %>%
  st_transform(target_crs)

kabler_energinet <- st_read(paste0(wfs_url_el_kabel,
                                   "?service=WFS&version=2.0.0&request=GetFeature",
                                   "&typeNames=XP_el_Inspir:KabelTrace&outputFormat=GEOJSON"), quiet = TRUE) %>%
  st_transform(target_crs)

pipelines_emodnet <- st_read(paste0(wfs_url_olie_gas_pipe,
                                    "?service=WFS&version=2.0.0&request=GetFeature",
                                    "&typeNames=emodnet:pipelines&outputFormat=application/json"), quiet = TRUE) %>%
  st_transform(target_crs)


## 2. Udvælg og klip til undersøgelsesområdet --------------------------------

kabler_koge_mst <- anlaeg_linje %>%
  filter(TYPE == "Kabler, rørledninger og klimanalæg") %>%
  st_intersection(assessment_area_dissolved) %>%
  transmute(kilde = "MST")

# Kontek-elektrodeanlæg udelades her (anvendes i preslag for kemisk forurening)
kabler_koge_energi <- kabler_energinet %>%
  filter(Spaending != "Elektrode") %>%
  st_intersection(assessment_area_dissolved) %>%
  transmute(kilde = "Energinet")

kabler_koge <- bind_rows(kabler_koge_mst, kabler_koge_energi)

gasroer_koge <- pipelines_emodnet %>%
  st_intersection(assessment_area_dissolved)


## 3. Rasterisér til 250 m grid -----------------------------------------------

# Celler som en linje rører = 1, øvrige celler i området = 0, udenfor = NA
linje_til_rast <- function(x) {
  x %>%
    terra::vect() %>%
    terra::rasterize(grid_raster, field = 1, touches = TRUE, background = 0) %>%
    terra::mask(grid_raster)
}

kabler_rast  <- linje_til_rast(kabler_koge)
gasroer_rast <- linje_til_rast(gasroer_koge)


## 4. Effektdistance (lineært decay) ------------------------------------------

kabler_decay  <- decay_focal(kabler_rast,  max_dist = max_dist_kabler)
gasroer_decay <- decay_focal(gasroer_rast, max_dist = max_dist_gasroer)

plot(kabler_decay)

names(kabler_decay)  <- "kabler_decay"
names(gasroer_decay) <- "gasroer_decay"

par(mfrow = c(1, 2))
plot(kabler_decay,  main = "Kabler")
plot(gasroer_decay, main = "Gasrør")


## 5. Gem rastere -------------------------------------------------------------
terra::writeRaster(kabler_decay,
                   file.path(PATHS$output_pressure_tif, "iei", "kabler_decay.tif"),  overwrite = TRUE)
terra::writeRaster(gasroer_decay,
                   file.path(PATHS$output_pressure_tif, "iei", "gasroer_decay.tif"), overwrite = TRUE)


## 6. Kort til bilag ----------------------------------------------------------

lav_linjekort <- function(linjer, farve_label = "Tilstede") {
  ggplot() +
    geom_spatraster_rgb(data = tiles, maxcell = 5e6) +            # baggrund: relief
    geom_sf(data = assessment_area_dissolved, fill = "#9FBDD8", color = "black") +
    geom_sf(data = linjer, aes(color = farve_label), linewidth = 0.8) +
    scale_color_manual(name = NULL, values = setNames("red", farve_label)) +
    boundary +
    theme_minimal() +
    my_theme +
    theme(legend.position      = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.background    = element_rect(fill = "transparent", colour = NA),
          legend.text          = element_text(size = 16)) +
    north_arrow +
    scale_bar
}

map_kabler <- lav_linjekort(kabler_koge,  "Kabler")
map_pipes  <- lav_linjekort(gasroer_koge, "Gasrør")


gem_trim(map_kabler,
         file.path(PATHS$output_pressure_png, "iei", "map_kabler_pa.png"),
         dpi = 360, width_cm = 18)

gem_trim(map_pipes,
         file.path(PATHS$output_pressure_png, "iei", "map_roer_pa.png"),
         dpi = 360, width_cm = 18)
