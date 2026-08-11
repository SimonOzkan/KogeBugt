#---------------------- Offshore vindmølleparker - faste konstruktioner -----------------
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
## 1. Indlæs vindmøllepark polygoner
## ------------------------------------------------------------------
turbines <- st_read(file.path(PATHS$input_pressure, "/havvind/EMODnet_HA_Energy_WindFarms_20260710/EMODnet_HA_Energy_WindFarms_pg_20260710.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

# Tjek unikke STATUS værdier
message("Unikke STATUS værdier: ")
print(unique(turbines$STATUS))

turbines_koge <- st_intersection(turbines, assessment_area_dissolved) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 2. Funktion til fraktion-beregning
## ------------------------------------------------------------------
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
    group_by(id) %>%
    summarise(
      area_frac = sum(area_frac, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(area_frac = pmin(area_frac, 1))
  
  return(frac)
}

## ------------------------------------------------------------------
## 3. Funktion til rasterisering
## ------------------------------------------------------------------
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

## ------------------------------------------------------------------
## 4. Filtrer per STATUS og beregn fraktioner
## ------------------------------------------------------------------
turbines_production <- turbines_koge %>% filter(STATUS == "Production")
turbines_approved   <- turbines_koge %>% filter(STATUS == "Approved")
turbines_planned    <- turbines_koge %>% filter(STATUS == "Planned")

frac_production <- calc_frac(turbines_production, grid, grid_area)

frac_approved <- calc_frac(turbines_approved, grid, grid_area)

frac_planned <- calc_frac(turbines_planned, grid, grid_area)

## ------------------------------------------------------------------
## 5. Lav rasters
## ------------------------------------------------------------------
r_production <- make_raster(frac_production, grid, assessment_area_vect, target_crs)
r_approved   <- make_raster(frac_approved,   grid, assessment_area_vect, target_crs)
r_planned    <- make_raster(frac_planned,    grid, assessment_area_vect, target_crs)

# Gem tif filer
terra::writeRaster(r_production, file.path(PATHS$output_pressure_tif, "vindmoelleparker_production_frac.tif"), overwrite = TRUE)
terra::writeRaster(r_approved,   file.path(PATHS$output_pressure_tif, "vindmoelleparker_approved_frac.tif"),   overwrite = TRUE)
terra::writeRaster(r_planned,    file.path(PATHS$output_pressure_tif, "vindmoelleparker_planned_frac.tif"),    overwrite = TRUE)

message("Alle tif filer gemt")

## ------------------------------------------------------------------
## 6. Lav sf til plot
## ------------------------------------------------------------------
to_sf <- function(r) {
  r %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    st_make_valid() %>%
    rename("value" = 1) %>%
    st_transform(crs = target_crs)
}

production_sf_plot <- to_sf(r_production) %>%
  mutate(status = "aktiv")
approved_sf_plot   <- to_sf(r_approved)%>%
  mutate(status = "godkendt")
planned_sf_plot    <- to_sf(r_planned)%>%
  mutate(status = "planlagt")

sf_turbne_comb <- bind_rows(production_sf_plot,approved_sf_plot) %>%
  bind_rows(planned_sf_plot)

## ------------------------------------------------------------------
## 7. Indlæs baggrundskort og plot-funktion
## ------------------------------------------------------------------
map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)


viridis_start_color <- viridis_pal()(1)

plot_vindmoeller <- function(sf_data, titel) {
  ggplot() +
    geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
    geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
    geom_sf(data = sf_data, aes(fill = value), color = NA) +
    color_viridis+
    boundary+
    theme_minimal()+
    my_theme+
    north_arrow+
    scale_bar
}


## ------------------------------------------------------------------
## 8. Lav og gem plots
## ------------------------------------------------------------------
map_production <- plot_vindmoeller(production_sf_plot, "Vindmølleparker\nProduction")
map_approved   <- plot_vindmoeller(approved_sf_plot,   "Vindmølleparker\nApproved")
map_planned    <- plot_vindmoeller(planned_sf_plot,    "Vindmølleparker\nPlanned")

map_production
map_approved
map_planned

ggsave(plot = map_production, filename = file.path(PATHS$output_pressure_png, "\\vindmoelleparker_production.png"), bg = NULL, height = 18, width = 18, dpi = 300)
ggsave(plot = map_approved,   filename = file.path(PATHS$output_pressure_png, "\\vindmoelleparker_approved.png"),   bg = NULL, height = 18, width = 18, dpi = 300)
ggsave(plot = map_planned,    filename = file.path(PATHS$output_pressure_png, "\\vindmoelleparker_planned.png"),    bg = NULL, height = 18, width = 18, dpi = 300)

