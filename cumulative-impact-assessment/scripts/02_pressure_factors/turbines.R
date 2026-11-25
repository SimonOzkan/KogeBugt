#---------------------- Offshore vindmølleparker - faste konstruktioner -----------------
source("scripts/00_setup.R")
PATHS <- set_project_paths()

max_dist = 4300
## ------------------------------------------------------------------
## 1. Indlæs vindmøllepark polygoner
## ------------------------------------------------------------------
turbines <- st_read(file.path(PATHS$input_pressure, "/havvind/EMODnet_HA_Energy_WindFarms_20260710/EMODnet_HA_Energy_WindFarms_pg_20260710.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

# Tjek unikke STATUS værdier
print(unique(turbines$STATUS))

turbines_koge <- st_intersection(turbines, assessment_area_dissolved) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 2. Funktion til fraktion-beregning
## ------------------------------------------------------------------
# Celler som en bro rører = 1, øvrige celler i området = 0, udenfor = NA
sf_til_rast <- function(x) {
  x %>%
    terra::vect() %>%
    terra::rasterize(grid_raster, field = 1, touches = TRUE, background = 0) %>%
    terra::mask(grid_raster)
}


## ------------------------------------------------------------------
## 3. Funktion til rasterisering
## ------------------------------------------------------------------

## ------------------------------------------------------------------
## 4. Filtrer per STATUS og beregn fraktioner
## ------------------------------------------------------------------
turbines_production <- turbines_koge %>% filter(STATUS == "Production")
turbines_approved   <- turbines_koge %>% filter(STATUS == "Approved")
turbines_planned    <- turbines_koge %>% filter(STATUS == "Planned")

frac_production <- sf_til_rast(turbines_production)
names(frac_production) <- "turb_prod"
frac_approved <- sf_til_rast(turbines_approved)

frac_planned <- sf_til_rast(turbines_planned)

par(mfrow = c(3,1))
plot(frac_production)
plot(frac_approved)
plot(frac_planned)

## ------------------------------------------------------------------
## 5. Lav rasters
## ------------------------------------------------------------------
par(mfrow = c(1,1))
frac_production_decay <- decay_focal(frac_production, max_dist)

plot(frac_production_decay)

# Gem tif filer
terra::writeRaster(frac_production_decay, file.path(PATHS$output_pressure_tif, "iei/vindmoelleparker_production_decay.tif"), overwrite = TRUE)
terra::writeRaster(r_production, file.path(PATHS$output_pressure_tif, "all_tif/vindmoelleparker_production_frac.tif"), overwrite = TRUE)

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

production_sf_plot <- to_sf(frac_production) %>%
  mutate(status = "aktiv") %>%
  rename("value" = "turb_prod")
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
         file.path(PATHS$output_pressure_png, "iei\\vindmoelleparker_production.png"),
         dpi = 360, width_cm = 18)

gem_trim(map_approved,
         file.path(PATHS$output_pressure_png, "anlaeg\\vindmoelleparker_approved.png"),
         dpi = 360, width_cm = 18)

gem_trim(map_planned,
         file.path(PATHS$output_pressure_png, "anlaeg\\vindmoelleparker_planned.png"),
         dpi = 360, width_cm = 18)

comb_map <- map_production + map_approved / map_planned + plot_layout(guides = "collect")

ggsave(plot = comb_map,   filename = file.path(PATHS$output_pressure_png, "anlaeg\\vindmoelleparker_comb.png"),   bg = NULL, height = 18, width = 18, dpi = 300)


