#-------------------------------- Holmene --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()

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
holmene_grid <- st_intersection(grid_sf, holmene) %>%
  mutate(holmene_area = as.numeric(st_area(.))) 
# Check for intersection
holmene_check <- holmene_grid %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(holmene_area_sum = sum(holmene_area))

# ── Beregn fraktion per grid-celle ────────────────────────────────
holmene_frac <- grid_sf %>%                       # start fra HELE grid
  left_join(holmene_check, by = "id") %>%       # join arealerne på
  mutate(value = tidyr::replace_na(holmene_area_sum,0),
    value = as.numeric(value) / as.numeric(area_grid)
  ) %>%
  st_make_valid()


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


map_holmene <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = holmene_frac, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_holmene

# ── Gem plot ───────────────────────────────────────────────────────────────
gem_trim(map_holmene,
         file.path(PATHS$output_pressure_png, "anlaeg\\holmene.png"),
         dpi = 360, width_cm = 18)
