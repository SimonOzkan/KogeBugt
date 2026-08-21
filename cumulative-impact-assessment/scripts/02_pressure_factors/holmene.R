#-------------------------------- Holmene --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()
target_crs <- 25832

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
holmene_grid <- st_intersection(grid, holmene) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(holmene_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(holmene_area = sum(holmene_area),.groups = "drop")

# ── Beregn fraktion per grid-celle ────────────────────────────────
holmene_frac <- grid %>%                       # start fra HELE grid
  left_join(holmene_grid, by = "id") %>%       # join arealerne på
  mutate(
    holmene_area = tidyr::replace_na(holmene_area, 0),   # ingen overlap = 0
    value = as.numeric(holmene_area) / as.numeric(area_grid),
    value = pmin(value, 1)
  ) %>%
  dplyr::select(id, value, geometry) %>%
  st_as_sf()

message("Antal grid-celler i alt: ", nrow(holmene_frac),
        " – heraf med Holmene: ", sum(holmene_frac$value > 0))


# ── Lav raster ────────────────────────────────────────────────────
r_holmene <- terra::rasterize(
  terra::vect(holmene_frac),
  grid_raster,
  field      = "value",
  fun        = "max",
  background = NA        # NA = uden for grid, 0 = i grid uden Holmene
)

plot(r_holmene)

# Gem som tif
terra::writeRaster(
  r_holmene,
  filename  = file.path(PATHS$output_pressure_tif,"anlaeg" ,"Holmene_frac.tif"),
  overwrite = TRUE
)

# ── Plot til bilag ────────────────


map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)

viridis_start_color <- viridis_pal()(1)  

map_holmene <- ggplot() +
  geom_sf(data = map_eu,  fill = "#c3fbb1", color = NA, alpha = 0.5) +
  #geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = holmene_frac,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_holmene

# ── Gem plot ───────────────────────────────────────────────────────────────
ggsave(
  plot     = map_holmene,
  filename = file.path(PATHS$output_pressure_png, "anlaeg","Holmene.png"),
  bg       = NULL,
  height   = 18,
  width    = 18,
  dpi      = 300
)
