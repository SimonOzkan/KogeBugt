#-------------------------------- Holmene --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()
target_crs <- 25832

# Indlæs grid og undersøgelsesområde
grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = as.numeric(st_area(.))) %>%
  st_drop_geometry()

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)

# ── 1. Hent Holmene fra WFS ───────────────────────────────────────────────────
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

# ── 2. Filtrer og dissolv L1 + L2 til ét samlet Holmene polygon ──────────────
holmene <- havplan %>%
  filter(zone_title_prefix == "Zone til Holmene (L)") %>%
  select(zone, geometry) %>%
  st_union() %>%
  st_sf() %>%
  mutate(navn = "Holmene")

# ── 3. Intersection med grid ──────────────────────────────────────────────────
holmene_grid <- st_intersection(grid, holmene) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(area_intersect = as.numeric(st_area(.)))

# ── 4. Beregn fraktion per grid-celle ─────────────────────────────────────────
holmene_frac <- holmene_grid %>%
  st_drop_geometry() %>%
  left_join(grid_area, by = "id") %>%
  mutate(
    area_frac = area_intersect / area_grid,
    area_frac = pmin(area_frac, 1)
  ) %>%
  select(id, navn, area_frac)

message("Antal grid-celler med Holmene: ", nrow(holmene_frac))

# ── 5. Lav raster ─────────────────────────────────────────────────────────────
r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = paste0("EPSG:", target_crs)
)

grid_holmene <- grid %>%
  left_join(holmene_frac %>% select(id, area_frac), by = "id") %>%
  mutate(area_frac = ifelse(is.na(area_frac), 0, area_frac))

r_holmene <- terra::rasterize(
  terra::vect(grid_holmene),
  r_template,
  field      = "area_frac",
  fun        = "max",
  background = NA
)
r_holmene[r_holmene == 0] <- NA

# Gem som tif
dir.create(file.path(PATHS$output_pressure_tif), recursive = TRUE, showWarnings = FALSE)
terra::writeRaster(
  r_holmene,
  filename  = file.path(PATHS$output_pressure_tif, "Holmene_frac.tif"),
  overwrite = TRUE
)
message("Gemt: Holmene_frac.tif")

# ── 6. Lav sf objekt til plot ─────────────────────────────────────────────────
holmene_sf <- r_holmene %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = "area_frac") %>%
  st_transform(crs = target_crs)

# ── 7. Indlæs baggrundskort ───────────────────────────────────────────────────
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)

viridis_start_color <- viridis_pal()(1)  
# ── 8. Plot ───────────────────────────────────────────────────────────────────
map_holmene <- ggplot() +
  geom_sf(data = map_eu,  fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = holmene_sf,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_holmene

# ── 9. Gem plot ───────────────────────────────────────────────────────────────
ggsave(
  plot     = map_holmene,
  filename = file.path(PATHS$output_pressure_png, "\\Holmene.png"),
  bg       = NULL,
  height   = 18,
  width    = 18,
  dpi      = 300
)
