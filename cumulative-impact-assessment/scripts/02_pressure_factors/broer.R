#---------------------------- Broer (decay) -----------------------------------
# Broer fra OpenStreetMap (QuickOSM i QGIS), opdelt i vejbroer og øvrige broer.
# Celler med bro får værdien 1, hvorfra presset aftager lineært til 0 ved
# effektdistancen. Overlappende effektzoner håndteres med max (ingen addition).
#------------------------------------------------------------------------------

# Indlæs pakker, assessment grid og funktioner (bl.a. decay_focal) fra setup
source("scripts/00_setup.R")
PATHS <- set_project_paths()


## Parametre ------------------------------------------------------------------

max_dist_vej  <- 3200   # m, effektdistance for vejbroer (ØKOMAR)
max_dist_oevr <- 500    # m, effektdistance for øvrige broer (gang/cykel, jernbane m.m.)

# OSM highway-typer der regnes som vejbroer
vej_typer <- c("motorway", "motorway_link", "trunk", "trunk_link",
               "primary", "primary_link", "secondary", "secondary_link",
               "tertiary", "road", "busway")


## Indlæs og klassificér broer ------------------------------------------------

broer <- st_read(file.path(PATHS$input_pressure, "broer", "osm_broer", "broer_osm.gpkg")) %>%
  st_transform(crs = target_crs) %>%
  mutate(type = if_else(highway %in% vej_typer, "Vejbro", "Øvrige broer"))

# Broer inden for undersøgelsesområdet (til kort og tjek)
broer_koge <- broer %>%
  st_intersection(assessment_area_dissolved)

## Rasterisér til 250 m grid --------------------------------------------------

# Celler som en bro rører = 1, øvrige celler i området = 0, udenfor = NA
bro_til_rast <- function(x) {
  x %>%
    terra::vect() %>%
    terra::rasterize(grid_raster, field = 1, touches = TRUE, background = 0) %>%
    terra::mask(grid_raster)
}

vej_rast  <- broer %>% filter(type == "Vejbro")       %>% bro_til_rast()
oevr_rast <- broer %>% filter(type == "Øvrige broer") %>% bro_til_rast()

par(mfrow = c(1, 2))
plot(vej_rast,  main = "Vejbroer")
plot(oevr_rast, main = "Øvrige broer")


## Effektdistance (lineært decay) ---------------------------------------------

vej_decay  <- decay_focal(vej_rast,  max_dist = max_dist_vej)
oevr_decay <- decay_focal(oevr_rast, max_dist = max_dist_oevr)

# Samlet brolag: højeste pres pr. celle (ingen dobbelttælling)
broer_decay <- max(vej_decay, oevr_decay)
names(broer_decay) <- "broer_decay"

par(mfrow = c(1, 3))
plot(vej_decay,   main = "Vejbroer")
plot(oevr_decay,  main = "Øvrige broer")
plot(broer_decay, main = "Samlet")


## Gem rastere ----------------------------------------------------------------



terra::writeRaster(broer_decay,  file.path(PATHS$output_pressure_tif,"iei", "broer_decay.tif"), overwrite = TRUE)


## Kort til bilag -------------------------------------------------------------

map_broer <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = broer_koge,
          aes(fill = "Tilstede"), shape = 21, color = "red", size = 1.5) +
  scale_fill_manual(name = NULL,
                    values = c("Tilstede" = "red", "Ikke tilstede" = "#9FBDD8"))+
  boundary +
  theme_minimal() +
  my_theme +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 30))+
  north_arrow +
  scale_bar

map_broer

gem_trim(map_broer,
         file.path(PATHS$output_pressure_png, "iei", "broer.png"),
         dpi = 360, width_cm = 18)
