#-------------------------------- Råstofsindvinding og områder udlagt til råstofsinvinding --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()

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
  filter(STATUS == "Active") %>%
  st_union() %>%
  st_make_valid()

raastof_expired_koge <- raastof_emodnet_koge %>%
  filter(STATUS != "Active") %>%
  st_union() %>%
  st_make_valid()

# ── 3. Funktion til fraktion-beregning ───────────────────────────────────────
calc_frac <- function(lag_sf, grid_sf) {
  
  intersection <- st_intersection(grid_sf, lag_sf) %>%
    mutate(area_intersect = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(area_intersect = sum(area_intersect, na.rm = TRUE), .groups = "drop")
  
  frac <- grid_sf %>%
    left_join(intersection, by = "id") %>%
    mutate(value = tidyr::replace_na(area_intersect,0),
           value = value / area_grid
    )
  return(frac)
}

# ── 4. Funktion til rasterisering ─────────────────────────────────────────────
make_raster <- function(grid_frac, grid_raster) {

  
  r <- terra::rasterize(
    terra::vect(grid_frac),
    grid_raster,
    field      = "value",
    fun        = "max",
    background = NA
  )
  return(r)
}

# ── 5. Beregn fraktioner ──────────────────────────────────────────────────────
message("Beregner: Udviklingszone råstof")
frac_udviklingszone <- calc_frac(udviklingszone_raastof, grid_sf)

message("Beregner: Aktive råstofsområder")
frac_aktiv <- calc_frac(raastof_aktiv_koge, grid_sf)

message("Beregner: Expired råstofsområder")
frac_expired <- calc_frac(raastof_expired_koge, grid_sf)

# ── 6. Lav rasters ────────────────────────────────────────────────────────────
r_udviklingszone <- make_raster(frac_udviklingszone, grid_raster)
r_aktiv          <- make_raster(frac_aktiv,grid_raster)
r_expired        <- make_raster(frac_expired,grid_raster)

# ── 7. Gem tif filer ──────────────────────────────────────────────────────────

terra::writeRaster(r_udviklingszone, file.path(PATHS$output_pressure_tif,"fysisk_forstyrrelse", "raastof_udviklingszone_frac.tif"), overwrite = TRUE)
terra::writeRaster(r_aktiv,          file.path(PATHS$output_pressure_tif,"fysisk_forstyrrelse", "raastof_aktiv_frac.tif"),          overwrite = TRUE)
terra::writeRaster(r_expired,         file.path(PATHS$output_pressure_tif,"fysisk_forstyrrelse", "raastof_expired_frac.tif"),        overwrite = TRUE)

# ── 8. Lav sf objekter til plot ───────────────────────────────────────────────
to_sf <- function(r, value_col) {
  r %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    st_make_valid()
  }

udviklingszone_sf <- to_sf(r_udviklingszone)
aktiv_sf          <- to_sf(r_aktiv)
expired_sf        <- to_sf(r_expired)

# ── 10. Plot-funktion ─────────────────────────────────────────────────────────
plot_raastof <- function(sf_data) {
  
  ggplot() +
    geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
    geom_sf(data = sf_data, aes(fill = value), color = NA) +
    color_viridis+
    boundary+
    theme_minimal()+
    my_theme+
    north_arrow+
    scale_bar
}

# ── 11. Lav plots ─────────────────────────────────────────────────────────────
map_udviklingszone <- plot_raastof(udviklingszone_sf)
map_aktiv          <- plot_raastof(aktiv_sf)
map_expired        <- plot_raastof(expired_sf)

map_udviklingszone
map_aktiv
map_expired


gem_trim(map_udviklingszone,
         file.path(PATHS$output_pressure_png, "fysisk_forstyrrelse","map_udviklingszone.png"),
         dpi = 360, width_cm = 18)

gem_trim(map_aktiv,
         file.path(PATHS$output_pressure_png, "fysisk_forstyrrelse","map_aktiv.png"),
         dpi = 360, width_cm = 18)
gem_trim(map_expired,
         file.path(PATHS$output_pressure_png, "fysisk_forstyrrelse","map_expired.png"),
         dpi = 360, width_cm = 18)

# ── 12. Gem plots ─────────────────────────────────────────────────────────────
ggsave(plot = map_udviklingszone, filename = file.path(PATHS$output_pressure_png,"fysisk_forstyrrelse", "\\raastof_udviklingszone.png"), bg = NULL, height = 18, width = 18, dpi = 300)
ggsave(plot = map_aktiv,          filename = file.path(PATHS$output_pressure_png,"fysisk_forstyrrelse", "\\raastof_aktiv.png"),          bg = NULL, height = 18, width = 18, dpi = 300)
ggsave(plot = map_expired,         filename = file.path(PATHS$output_pressure_png,"fysisk_forstyrrelse", "\\raastof_expired.png"),        bg = NULL, height = 18, width = 18, dpi = 300)


