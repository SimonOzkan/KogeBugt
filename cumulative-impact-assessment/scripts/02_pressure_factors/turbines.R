#---------------------- Offshore vindmølleparker - faste konstruktioner -----------------
source("scripts/00_setup.R")
PATHS <- set_project_paths()
target_crs <- 25832



## ------------------------------------------------------------------
## 1. Indlæs vindmøllepark polygoner
## ------------------------------------------------------------------
turbines <- st_read(file.path(PATHS$input_pressure, "/havvind/EMODnet_HA_Energy_WindFarms_20260710/EMODnet_HA_Energy_WindFarms_pg_20260710.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

# Tjek unikke STATUS værdier
print(unique(turbines$STATUS))

turbines_koge <- st_intersection(turbines, assessment_area_dissolved) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 2. Funktion til fraktion-beregning
## ------------------------------------------------------------------
calc_frac <- function(lag_sf, grid_sf, grid_area) {
  intersection <- st_intersection(grid_sf, lag_sf) %>%
    mutate(area_intersect = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(area_intersect = sum(as.numeric(area_intersect), na.rm = TRUE), .groups = "drop")
  
  if (nrow(intersection) == 0) {
    message("  → Ingen overlap med grid")
    return(NULL)
  }
  
  frac <- grid_sf %>%
    left_join(intersection, by = "id") %>%
    mutate(
      value = tidyr::replace_na(as.numeric(area_intersect), 0),  # ingen vindmøller
      value = value / area_grid
    ) %>%
    st_make_valid()
  
  return(frac)
}

## ------------------------------------------------------------------
## 3. Funktion til rasterisering
## ------------------------------------------------------------------
make_raster <- function(frac_df, grid, assessment_area_vect, target_crs) {
  
  
  r <- terra::rasterize(
    terra::vect(frac_df),
    grid_raster,
    field      = "value",
    fun        = "max",
    background = NA
  )
  return(r)

}

## ------------------------------------------------------------------
## 4. Filtrer per STATUS og beregn fraktioner
## ------------------------------------------------------------------
turbines_production <- turbines_koge %>% filter(STATUS == "Production")
turbines_approved   <- turbines_koge %>% filter(STATUS == "Approved")
turbines_planned    <- turbines_koge %>% filter(STATUS == "Planned")

frac_production <- calc_frac(turbines_production, grid_sf, grid_area)

frac_approved <- calc_frac(turbines_approved, grid_sf, grid_area)

frac_planned <- calc_frac(turbines_planned, grid_sf, grid_area)

## ------------------------------------------------------------------
## 5. Lav rasters
## ------------------------------------------------------------------
r_production <- make_raster(frac_production, grid_raster)
r_approved   <- make_raster(frac_approved,   grid_raster)
r_planned    <- make_raster(frac_planned,    grid_raster)


# Gem tif filer
terra::writeRaster(r_production, file.path(PATHS$output_pressure_tif, "anlaeg/vindmoelleparker_production_frac.tif"), overwrite = TRUE)
terra::writeRaster(r_approved,   file.path(PATHS$output_pressure_tif, "anlaeg/vindmoelleparker_approved_frac.tif"),   overwrite = TRUE)
terra::writeRaster(r_planned,    file.path(PATHS$output_pressure_tif, "anlaeg/vindmoelleparker_planned_frac.tif"),    overwrite = TRUE)

## ------------------------------------------------------------------
## 6. Lav sf til plot
## ------------------------------------------------------------------
to_sf <- function(r) {
  r %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    st_make_valid() %>%
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

plot_vindmoeller <- function(sf_data) {
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


## ------------------------------------------------------------------
## 8. Lav og gem plots
## ------------------------------------------------------------------
map_production <- plot_vindmoeller(production_sf_plot)
map_approved   <- plot_vindmoeller(approved_sf_plot)
map_planned    <- plot_vindmoeller(planned_sf_plot)

map_production
map_approved
map_planned

gem_trim(map_production,
         file.path(PATHS$output_pressure_png, "anlaeg\\vindmoelleparker_production.png"),
         dpi = 360, width_cm = 18)

gem_trim(map_approved,
         file.path(PATHS$output_pressure_png, "anlaeg\\vindmoelleparker_approved.png"),
         dpi = 360, width_cm = 18)

gem_trim(map_planned,
         file.path(PATHS$output_pressure_png, "anlaeg\\vindmoelleparker_planned.png"),
         dpi = 360, width_cm = 18)

comb_map <- map_production + map_approved / map_planned + plot_layout(guides = "collect")

ggsave(plot = comb_map,   filename = file.path(PATHS$output_pressure_png, "anlaeg\\vindmoelleparker_comb.png"),   bg = NULL, height = 18, width = 18, dpi = 300)


