#-------------------------------- Holmene --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()


max_dist <-3.2*10^3
# ── Hent Holmene fra WFS ───────────────────────────────────────────────────
bbox_wgs84 <- st_bbox(st_transform(assessment_area_dissolved, crs = 4326))

request <- paste0(
  "https://havplan.dk/geoserver/havplan/wfs",
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=havplan:Danmarks_havplan_af_28_juni_2024",
  "&outputFormat=application/json",
  "&BBOX=", bbox_wgs84["xmin"], ",", bbox_wgs84["ymin"], ",",
  bbox_wgs84["xmax"], ",", bbox_wgs84["ymax"],
  ",EPSG:4326"
)

havplan <- st_read(request) %>%
  st_transform(crs = target_crs)

# ── Filtrer og dissolv L1 + L2 til ét samlet Holmene polygon ──────────────
holmene <- havplan %>%
  filter(zone_title_prefix == "Zone til Holmene (L)") %>%
  select(zone, geometry) %>%
  st_union() %>%
  st_sf() %>%
  mutate(navn = "Holmene")

# ── Intersection med grid ──────────────────────────────────────────────────
## sf til raster
# Celler som rør = 1, øvrige celler i området = 0, udenfor = NA
sf_til_rast <- function(x) {
  x %>%
    terra::vect() %>%
    terra::rasterize(grid_raster, field = 1, touches = TRUE, background = 0) %>%
    terra::mask(grid_raster)
}

holmene_rast <- sf_til_rast(holmene)
plot(holmene_rast)

holmene_decay <- decay_focal(holmene_rast, max_dist)
plot(holmene_decay)

terra::writeRaster(
  holmene_decay,
  filename  = file.path(PATHS$output_pressure_tif, "iei","holmene_decay.tif"),
  overwrite = TRUE
)


# ── Plot til bilag ────────────────

holmene_sf <- holmene_rast %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = "layer")


map_holmene <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = holmene_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_holmene

# ── Gem plot ───────────────────────────────────────────────────────────────
gem_trim(map_holmene,
         file.path(PATHS$output_pressure_png, "iei","holmene.png"),
         dpi = 360, width_cm = 18)
