#-------------------------------- Råstofsindvinding og områder udlagt til råstofsinvinding --------------------------------#
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

# ── 1. Hent råstofsområder fra WFS (Havplan) ─────────────────────────────────
bbox_wgs84 <- st_bbox(st_transform(assessment_area_dissolved, crs = 4326))

request_havplan <- paste0(
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

havplan <- st_read(request_havplan) %>%
  st_transform(crs = target_crs)

udviklingszone_raastof <- havplan %>%
  filter(zone_title_prefix == "Udviklingszone til råstofindvinding (R)") %>%
  st_make_valid()

# ── 2. Hent aktive og expired råstofsområder fra EMODnet ─────────────────────
raastof_emodnet <- st_read(file.path(PATHS$input_pressure, "raastof/EMODnet_HA_AggregatesExtraction_20250902/EMODnet_HA_Aggregates_pg_20250902.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

raastof_emodnet_koge <- st_intersection(raastof_emodnet, assessment_area_dissolved)

raastof_aktiv_koge <- raastof_emodnet_koge %>%
  filter(STATUS == "Active")

raastof_expired_koge <- raastof_emodnet_koge %>%
  filter(STATUS != "Active")

# ── 3. Funktion til fraktion-beregning ───────────────────────────────────────
calc_frac <- function(lag_sf, grid, grid_area) {
  intersection <- st_intersection(grid, lag_sf) %>%
    filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    mutate(area_intersect = as.numeric(st_area(.)))
  
  if (nrow(intersection) == 0) {
    message("  → Ingen overlap med grid")
    return(NULL)
  }
  
  frac <- intersection %>%
    st_drop_geometry() %>%
    left_join(grid_area, by = "id") %>%
    mutate(
      area_frac = area_intersect / area_grid,
      area_frac = pmin(area_frac, 1)
    ) %>%
    select(id, area_frac)
  
  return(frac)
}

# ── 4. Funktion til rasterisering ─────────────────────────────────────────────
make_raster <- function(frac_df, grid, assessment_area_vect, target_crs) {
  r_template <- terra::rast(
    extent     = terra::ext(assessment_area_vect),
    resolution = 250,
    crs        = paste0("EPSG:", target_crs)
  )
  
  grid_frac <- grid %>%
    left_join(frac_df %>% select(id, area_frac), by = "id") %>%
    mutate(area_frac = ifelse(is.na(area_frac), 0, area_frac))
  
  r <- terra::rasterize(
    terra::vect(grid_frac),
    r_template,
    field      = "area_frac",
    fun        = "max",
    background = NA
  )
  r[r == 0] <- NA
  return(r)
}

# ── 5. Beregn fraktioner ──────────────────────────────────────────────────────
message("Beregner: Udviklingszone råstof")
frac_udviklingszone <- calc_frac(udviklingszone_raastof, grid, grid_area)

message("Beregner: Aktive råstofsområder")
frac_aktiv <- calc_frac(raastof_aktiv_koge, grid, grid_area)

message("Beregner: Expired råstofsområder")
frac_expired <- calc_frac(raastof_expired_koge, grid, grid_area)

# ── 6. Lav rasters ────────────────────────────────────────────────────────────
r_udviklingszone <- make_raster(frac_udviklingszone, grid, assessment_area_vect, target_crs)
r_aktiv          <- make_raster(frac_aktiv,          grid, assessment_area_vect, target_crs)
r_expired        <- make_raster(frac_expired,         grid, assessment_area_vect, target_crs)

# ── 7. Gem tif filer ──────────────────────────────────────────────────────────
dir.create(file.path(PATHS$output_pressure_tif), recursive = TRUE, showWarnings = FALSE)

terra::writeRaster(r_udviklingszone, file.path(PATHS$output_pressure_tif, "raastof_udviklingszone_frac.tif"), overwrite = TRUE)
terra::writeRaster(r_aktiv,          file.path(PATHS$output_pressure_tif, "raastof_aktiv_frac.tif"),          overwrite = TRUE)
terra::writeRaster(r_expired,         file.path(PATHS$output_pressure_tif, "raastof_expired_frac.tif"),        overwrite = TRUE)

message("Alle tif filer gemt")

# ── 8. Lav sf objekter til plot ───────────────────────────────────────────────
to_sf <- function(r, value_col) {
  r %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    st_make_valid() %>%
    rename("value" = 1) %>%
    st_transform(crs = target_crs)
}

udviklingszone_sf <- to_sf(r_udviklingszone)
aktiv_sf          <- to_sf(r_aktiv)
expired_sf        <- to_sf(r_expired)

# ── 9. Indlæs baggrundskort ───────────────────────────────────────────────────
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)

# ── 10. Plot-funktion ─────────────────────────────────────────────────────────
plot_raastof <- function(sf_data, titel, farve_navn) {
  ggplot() +
    geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
    geom_sf(data = sf_data, aes(fill = value), color = NA) +
    scale_fill_viridis_c(
      name     = farve_navn,
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
}

# ── 11. Lav plots ─────────────────────────────────────────────────────────────
map_udviklingszone <- plot_raastof(udviklingszone_sf, "Udviklingszone råstof", "Fraktion\n(0-1)")
map_aktiv          <- plot_raastof(aktiv_sf,          "Aktive råstofsområder", "Fraktion\n(0-1)")
map_expired        <- plot_raastof(expired_sf,         "Expired råstofsområder","Fraktion\n(0-1)")

map_udviklingszone
map_aktiv
map_expired

# ── 12. Gem plots ─────────────────────────────────────────────────────────────
ggsave(plot = map_udviklingszone, filename = file.path(PATHS$output_pressure_png, "\\raastof_udviklingszone.png"), bg = NULL, height = 18, width = 18, dpi = 300)
ggsave(plot = map_aktiv,          filename = file.path(PATHS$output_pressure_png, "\\raastof_aktiv.png"),          bg = NULL, height = 18, width = 18, dpi = 300)
ggsave(plot = map_expired,         filename = file.path(PATHS$output_pressure_png, "\\raastof_expired.png"),        bg = NULL, height = 18, width = 18, dpi = 300)


