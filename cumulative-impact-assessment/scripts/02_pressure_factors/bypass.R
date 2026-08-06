#----------------------------------- Bypass ----------------------- ##
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

## ------------------------------------------------------------------
## 1. Hent Bypass data fra WFS
## ------------------------------------------------------------------
wfs_url_bypass <- "https://gisportal.mst.dk/server/services/ekstern/KDI_Bypass/MapServer/WFSServer"

request_bypass <- paste0(
  wfs_url_bypass,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=esri:Bypassomraader",
  "&outputFormat=GEOJSON"
)

bypass <- st_read(request_bypass) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

## ------------------------------------------------------------------
## 2. Klip til undersøgelsesområdet
## ------------------------------------------------------------------
bypass_koge <- st_intersection(bypass, assessment_area_dissolved) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_make_valid()

message("Antal bypass-polygoner i Køge Bugt: ", nrow(bypass_koge))

## ------------------------------------------------------------------
## 3. Intersection med 250m grid og beregn fraktion
## ------------------------------------------------------------------
bypass_grid <- st_intersection(grid, bypass_koge) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(area_intersect = as.numeric(st_area(.)))

bypass_frac <- bypass_grid %>%
  st_drop_geometry() %>%
  left_join(grid_area, by = "id") %>%
  mutate(
    area_frac = area_intersect / area_grid,
    area_frac = pmin(area_frac, 1)
  ) %>%
  group_by(id) %>%
  summarise(
    area_frac = sum(area_frac, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(area_frac = pmin(area_frac, 1))

message("Antal grid-celler med bypass: ", nrow(bypass_frac))

## ------------------------------------------------------------------
## 4. Lav raster
## ------------------------------------------------------------------
r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = paste0("EPSG:", target_crs)
)

grid_bypass <- grid %>%
  left_join(bypass_frac %>% select(id, area_frac), by = "id") %>%
  mutate(area_frac = ifelse(is.na(area_frac), 0, area_frac))

r_bypass <- terra::rasterize(
  terra::vect(grid_bypass),
  r_template,
  field      = "area_frac",
  fun        = "max",
  background = NA
)
r_bypass[r_bypass == 0] <- NA

# Gem tif
terra::writeRaster(
  r_bypass,
  file.path(PATHS$output_pressure_tif, "bypass_frac.tif"),
  overwrite = TRUE
)
message("Gemt: bypass_frac.tif")

## ------------------------------------------------------------------
## 5. Lav sf til plot
## ------------------------------------------------------------------
bypass_sf_plot <- r_bypass %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = 1) %>%
  st_transform(crs = target_crs)

## ------------------------------------------------------------------
## 6. Indlæs baggrundskort og plot
## ------------------------------------------------------------------
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)

map_bypass <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = bypass_sf_plot, aes(fill = value), color = NA) +
  scale_fill_viridis_c(
    name     = "Bypass",
    limits   = c(0, 1),
    na.value = NA
  ) +
  coord_sf(
    crs  = target_crs,
    xlim = c(696427, 775958),
    ylim = c(6096053, 6179593)
  ) +
  theme_minimal() +
  theme(
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank(),
    legend.position   = c(0.81, 0.90),
    legend.justification = "center",
    legend.title      = element_text(size = 20),
    legend.text       = element_text(size = 18),
    axis.ticks        = element_blank(),
    plot.margin       = grid::unit(c(0, 0, 0, 0), units = "mm"),
    axis.ticks.length = unit(0, "pt")
  ) +
  annotation_north_arrow(
    location    = "br",
    which_north = "true",
    style       = north_arrow_fancy_orienteering,
    pad_x       = unit(3.5, "cm"),
    pad_y       = unit(1.0, "cm"),
    height      = unit(1.8, "cm"),
    width       = unit(1.8, "cm")
  ) +
  annotation_scale(
    location   = "br",
    width_hint = 0.05,
    height     = unit(0.4, "cm"),
    bar_cols   = c("black", "white"),
    pad_x      = unit(0.2, "cm"),
    pad_y      = unit(1.5, "cm"),
    text_cex   = 1.2
  )

map_bypass

## ------------------------------------------------------------------
## 7. Gem plot
## ------------------------------------------------------------------
ggsave(
  plot     = map_bypass,
  filename = file.path(PATHS$output_pressure_png, "\\bypass.png"),
  bg       = NULL,
  height   = 18,
  width    = 18,
  dpi      = 300
)
