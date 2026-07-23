#---------------------- Klapning (dumpning af opmudret materiale) -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs
target_crs <- 25832

# Indlæs grid og undersøgelsesområde fra de forskellige project paths

grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(., crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = st_area(.)) %>%
  st_drop_geometry(.)

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)

# EMODnet oversigt over planlagte, aktive og godkendte offshore havvindmølleparker

turbines <- st_read(file.path(PATHS$input_pressure, "/havvind/EMODnet_HA_Energy_WindFarms_20260710/EMODnet_HA_Energy_WindFarms_pg_20260710.shp"))

turbines_proj <- turbines %>%
  st_transform(.,crs = target_crs)
# Klip/intersect til undersøgelsesområde

turbines_koge <- st_intersection(turbines_proj,assessment_area_dissolved)

ggplot()+
  geom_sf(data = turbines_koge, aes(fill = STATUS), color = NA)+
  theme_minimal()
## Intersection med grid 

turbines_area <- st_intersection(turbines_koge, grid) %>%
  mutate(area_turbine = st_area(.)) %>%
  left_join(grid_area, by = "id") %>%
  mutate(value = as.numeric(area_turbine) / as.numeric(area_grid),
         value = pmin(value, 1)) %>%
  st_make_valid()


## Konverter til raster

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

status_turbines <- unique(turbines_area$STATUS)

for (status in status_turbines) {
  
  message("Laver raster for status: ", status)
  
  turbine_rast <- terra::rasterize(
    terra::vect(turbines_area %>% filter(STATUS == status)),
    r_template,
    field      = "value",
    fun        = "max",      # hvis overlap: tag max fraktion
    background = NA          # celler udenfor assessment area sættes NA
  )
  
  status_filename <- status %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]", "")
  
  terra::writeRaster(
    turbine_rast,
    filename  = file.path(PATHS$output_pressure_tif, paste0("turbines_", status_filename, ".tif")),
    overwrite = TRUE
  )
  
  message("  → Gemt: turbines_", status_filename, ".tif")
}


############### Plotting for bilag ################


map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)


viridis_start_color <- viridis_pal()(1)

map_turbines <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = map_baltic_sea, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = turbines_area,
          aes(fill = value), color = NA) +
  scale_fill_viridis_c(name = "Turbines", limits = c(0, 1)) +
  facet_wrap(~ STATUS, ncol = 1, strip.position = "top") +
  coord_sf(
    crs  = 25832,
    xlim = c(696427, 775958),
    ylim = c(6096053, 6179593)
  ) +
  theme_minimal() +
  theme(
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(size = 14),
    legend.text      = element_text(size = 12),
    strip.text       = element_text(size = 14, face = "bold"),
    axis.ticks = element_blank(),
    plot.margin = grid::unit(c(2, 2, 2, 2), units = "mm"),
    axis.ticks.length = unit(0, "pt")
  ) +
  annotation_scale(
    location    = "br",
    width_hint  = 0.15,
    height      = unit(0.3, "cm"),
    bar_cols    = c("black", "white"),
    text_cex    = 0.9
  )

map_turbines

ggsave(plot = map_turbines,
       filename = file.path(PATHS$output_pressure_png, "\\turbines_by_status.png"),
       bg = NULL,
       height = 10,
       width = 24,
       dpi = 300)





