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


## Indlæs klapningsdata (se data folder / metadata_log)

klapning <- st_read(file.path(PATHS$input_pressure, "\\klapning\\EMODnet_HA_DredgeSpoilDumping_20251105\\EMODnet_HA_DredgeSpoilDumping_pg_Locations_20251105.shp")) %>%
  st_transform(., crs = target_crs) %>%
  st_make_valid()

# TODO: Forhør med Ciaran om der kun tages højde for område og ikke hvad/hvor meget der dumpes.
# Hvis ikke, anvend HELCOM-laget i stedet (samme geografiske data, men simplificeret
# attributinformation), så dubletter mellem de to kilder undgås. Indlæses her til senere brug,
# men indgår ikke i den aktuelle beregning.
klapning_HELCOM <- st_read(file.path(PATHS$input_pressure, "\\klapning\\_ags_depositing_poly_36_2_2024\\depositing_poly_36_2_2024.shp")) %>%
  st_transform(., crs = target_crs)


## Intersection med grid

klapning_oresund <- st_intersection(klapning, grid) %>%
  filter(SITE_NAME != "Avlandshage Klapplads") %>%  # står ikke på Miljøministeriets oversigt over klappladser
  mutate(area_klapning = st_area(.))

klapning_area <- klapning_oresund %>%
  left_join(grid_area, by = "id") %>%
  mutate(value = as.numeric(area_klapning) / as.numeric(area_grid),
         value = pmin(value, 1)) %>%
  st_make_valid()


## Konverter til raster

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

klapning_rast <- terra::rasterize(
  terra::vect(klapning_area),
  r_template,
  field      = "value",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

plot(klapning_rast)

terra::writeRaster(
  klapning_rast,
  filename  = file.path(PATHS$output_pressure_tif, "\\klapning.tif"),
  overwrite = TRUE
)


############### Plotting for bilag ################

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

viridis_start_color <- viridis_pal()(1)

map_klapning <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = map_baltic_sea, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = klapning_area,
          aes(fill = value), color = NA) +
  scale_fill_viridis_c(name = "Klapning", limits = c(0, 1)) +
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
    legend.position  = c(0.81, 0.90),
    legend.justification = "center",
    legend.title     = element_text(size = 20),
    legend.text      = element_text(size = 18),
    axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0, 0, 0, 0), units = "mm"),
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
    location    = "br",
    width_hint  = 0.05,
    height      = unit(0.4, "cm"),
    bar_cols    = c("black", "white"),
    pad_x       = unit(0.2, "cm"),
    pad_y       = unit(1.5, "cm"),
    text_cex    = 1.2
  )

map_klapning

ggsave(plot = map_klapning,
       filename = file.path(PATHS$output_pressure_png, "\\klapning.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
