#----------------------------------- Kontek -----------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()

max_dist <- 1000 #m
## --- WFS-kilder ---
wfs_url_el_kabel      <- "https://agis.energinet.dk/server/services/INSPIRE/XP_el_Inspir/MapServer/WFSServer"

## --- 1. Hent data ---

kabler_energinet <- st_read(paste0(wfs_url_el_kabel,
                                   "?service=WFS&version=2.0.0&request=GetFeature",
                                   "&typeNames=XP_el_Inspir:KabelTrace&outputFormat=GEOJSON"), quiet = TRUE) %>%
  st_transform(target_crs)


### Kontek elektrode anglæg anvendes om preslag i kemisk forurening
kontek_elektrode <- kabler_energinet %>%
  filter(Spaending == "Elektrode") %>%
  st_intersection(assessment_area_dissolved)

# Celler som rør = 1, øvrige celler i området = 0, udenfor = NA
sf_til_rast <- function(x) {
  x %>%
    terra::vect() %>%
    terra::rasterize(grid_raster, field = 1, touches = TRUE, background = 0) %>%
    terra::mask(grid_raster)
}

kontek_rast <- sf_til_rast(kontek_elektrode)

plot(kontek_rast)

kontek_rast_decay <- decay_focal(kontek_rast, max_dist)

plot(kontek_rast_decay)

terra::writeRaster(
  kontek_rast_decay,
  filename  = file.path(PATHS$output_pressure_tif, "kemisk_forurening","Kontek_decay.tif"),
  overwrite = TRUE
)

## ------------------------------------------------------------------
## 4  Plotting for bilag 
## ------------------------------------------------------------------

map_Kontek <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = kontek_elektrode,
          aes(fill = "Tilstede"), color = "red", size = 1.5) +
  scale_fill_manual(name = NULL,
                    values = c("Tilstede" = "red", "Ikke tilstede" = "#9FBDD8"))+
  boundary +
  theme_minimal() +
  my_theme +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 16))+
  north_arrow +
  scale_bar
map_Kontek

gem_trim(map_Kontek,
         file.path(PATHS$output_pressure_png, "kemisk_forurening", "Kontek.png"),
         dpi = 360, width_cm = 18)
