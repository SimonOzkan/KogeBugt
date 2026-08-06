#----------------------------------- Impulsiv lyd (ICES data) ----------------------- ##
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
## 1. Hent data og ICES grid
## ------------------------------------------------------------------
ICES_grid <- st_read(file.path(PATHS$input_pressure, "/lyd/impulsiv/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")) %>%
  st_transform(crs = target_crs)

impulsiv <- read_csv(file.path(PATHS$input_pressure, "/lyd/impulsiv/UnderwaternoiseData_0805550177/ImpulsiveNoiseRegisterData_0805550177.csv")) %>%
  filter(Country %in% c("DK", "SE"))

## ------------------------------------------------------------------
## 2. Normaliser ICES navne og join geometri
## ------------------------------------------------------------------
normalize_ices <- function(x) {
  str_replace(x, "([A-Z]+)(0+)([1-9])", "\\1\\3")
}

impulsiv_clean <- impulsiv %>%
  mutate(polygonID_norm = normalize_ices(polygonID))

ICES_grid_clean <- ICES_grid %>%
  mutate(ICESNAME_norm = normalize_ices(ICESNAME))

# Join geometri på normaliserede navne
impulsiv_joined <- impulsiv_clean %>%
  left_join(
    ICES_grid_clean %>% st_drop_geometry() %>% select(ICESNAME_norm),
    by = c("polygonID_norm" = "ICESNAME_norm")
  ) %>%
  left_join(
    ICES_grid_clean %>% select(ICESNAME_norm, geometry),
    by = c("polygonID_norm" = "ICESNAME_norm")
  ) %>%
  st_as_sf(crs = target_crs) %>%
  st_intersection(grid)


ggplot()+
  geom_sf(data = ICES_grid_clean %>% filter(Ecoregion %in% c("Greater North Sea", "Baltic Sea")), aes(fill = "grey"))+
  theme_minimal()


## ------------------------------------------------------------------
## 3. Log-transformer og normaliser duration på HELE datasættet
## ------------------------------------------------------------------

# Log-transformer først (log10(1 + x) for at håndtere 0-værdier)
impulsiv_log <- impulsiv %>%
  mutate(duration_log = log10(1 + duration))

# Beregn min/max på log-transformerede værdier
max_min_impulsiv <- impulsiv_log %>%
  summarise(
    min_dur_log = min(duration_log, na.rm = TRUE),
    max_dur_log = max(duration_log, na.rm = TRUE)
  )

message("Log duration range: ", max_min_impulsiv$min_dur_log, " → ", max_min_impulsiv$max_dur_log)

# Normaliser log-transformerede værdier til 0-1
impulsiv_norm <- impulsiv_joined %>%
  mutate(
    duration_log  = log10(1 + duration),
    duration_norm = (duration_log - max_min_impulsiv$min_dur_log) /
      (max_min_impulsiv$max_dur_log - max_min_impulsiv$min_dur_log)
  )

## ------------------------------------------------------------------
## 4. Aggreger til ICES polygon niveau (sum af normaliseret duration)
## ------------------------------------------------------------------
impulsiv_agg <- impulsiv_norm %>%
  st_drop_geometry() %>%
  group_by(polygonID_norm) %>%
  summarise(
    duration_norm_sum = sum(duration_norm, na.rm = TRUE),
    n_events          = n(),
    .groups = "drop"
  ) %>%
  # Klamp til 0-1 efter summering
  mutate(duration_norm_sum = pmin(duration_norm_sum, 1))

# Join geometri tilbage
impulsiv_agg_sf <- impulsiv_agg %>%
  left_join(
    ICES_grid_clean %>% select(ICESNAME_norm, geometry),
    by = c("polygonID_norm" = "ICESNAME_norm")
  ) %>%
  st_as_sf(crs = target_crs)

## ------------------------------------------------------------------
## 5. Klip til undersøgelsesområdet
## ------------------------------------------------------------------
impulsiv_koge <- impulsiv_agg_sf %>%
  st_make_valid() %>%
  st_intersection(assessment_area_dissolved)

message("ICES polygoner med data i Køge Bugt: ", nrow(impulsiv_koge))


## ------------------------------------------------------------------
## 6. Intersection med 250m grid og beregn fraktion
## ------------------------------------------------------------------
impulsiv_grid <- st_intersection(grid, impulsiv_koge) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(area_intersect = as.numeric(st_area(.)))

# Beregn fraktion × normaliseret duration per grid-celle
impulsiv_frac <- impulsiv_grid %>%
  st_drop_geometry() %>%
  left_join(grid_area, by = "id") %>%
  mutate(
    area_frac         = area_intersect / area_grid,
    area_frac         = pmin(area_frac, 1),
    # Vægtet værdi: fraktion af cellen × normaliseret duration
    value_weighted    = area_frac * duration_norm_sum
  ) %>%
  group_by(id) %>%
  summarise(
    value = sum(value_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(value = pmin(value, 1))

message("Antal grid-celler med impulsiv lyd: ", nrow(impulsiv_frac))

## ------------------------------------------------------------------
## 7. Lav raster
## ------------------------------------------------------------------
r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = paste0("EPSG:", target_crs)
)

grid_impulsiv <- grid %>%
  left_join(impulsiv_frac %>% select(id, value), by = "id") %>%
  mutate(value = ifelse(is.na(value), 0, value))

r_impulsiv <- terra::rasterize(
  terra::vect(grid_impulsiv),
  r_template,
  field      = "value",
  fun        = "max",
  background = NA
)
r_impulsiv[r_impulsiv == 0] <- NA

# Gem tif
terra::writeRaster(
  r_impulsiv,
  file.path(PATHS$output_pressure_tif, "impulsiv_lyd_norm.tif"),
  overwrite = TRUE
)
message("Gemt: impulsiv_lyd_norm.tif")

## ------------------------------------------------------------------
## 8. Plot
## ------------------------------------------------------------------
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)

impulsiv_sf_plot <- r_impulsiv %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = 1) %>%
  st_transform(crs = target_crs)

map_impulsiv <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = impulsiv_sf_plot, aes(fill = value), color = NA) +
  scale_fill_viridis_c(
    name     = "Impulsiv lyd\n(norm. duration)",
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

map_impulsiv

ggsave(
  plot     = map_impulsiv,
  filename = file.path(PATHS$output_pressure_png, "\\impulsiv_lyd.png"),
  bg       = NULL,
  height   = 18,
  width    = 18,
  dpi      = 300
)