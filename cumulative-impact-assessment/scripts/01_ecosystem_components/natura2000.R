#---------------------- Natura 2000 -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

#Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs 
target_crs <- 25832

# Indlæs grid, undersøgelsesområde og data fra de forskellige project paths

grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(., crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = st_area(.) ) %>%
  st_drop_geometry(.)

assessment_area_dissolved <-st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(.,crs=target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)

## indlæs Natura 2000 områder (se data folder)

natura2000 <- st_read(file.path(PATHS$input_ecosystem, "\\natura2000\\eea_v_3035_100_k_natura2000_p_2024_v01_r00\\eea_v_3035_100_k_natura2000_p_2024_v01_r00\\SHP\\Natura2000_end2024_epsg3035.shp"))

# Overlap mellem Natura 2000 habitat områder håndteres ved Union

# Først "klippes" der til undersøgelsesområdet for at optimering af union hastighed
natura2000_clipped <- natura2000 %>%
  st_make_valid() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  st_intersection(assessment_area_dissolved)

natura2000_union <- natura2000_clipped %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf()


## Intersect med grid
natura2000_intersect <- st_intersection(natura2000_union,grid) %>%
  st_make_valid()

# Find fraktion are hver gridcelle dækket af natura2000 og sæt til value for raster (0-1)
natura2000_area <-natura2000_intersect %>%
  mutate(area_natura = st_area(.)) %>%
  left_join(grid_area, by = "id" ) %>%
  mutate(value = as.numeric(area_natura) / as.numeric(area_grid),
         value = pmin(value, 1)) 


# Konverter til raster

r_template <- terra::rast(
  extent     = terra::ext(terra::vect(assessment_area_dissolved)),
  resolution = 250,
  crs        = "EPSG:25832"
)


natura2000_rast <- terra::rasterize(
  terra::vect(natura2000_area),
  r_template,
  field    = "value",
  fun      = "max",      # der bør ikke være overlap, derfor tages max
  background = NA        # celler udenfor assessment area → NA
)

plot(natura2000_rast)


terra::writeRaster(
  natura2000_rast,
  filename = file.path(PATHS$output_ecosystem_tif, "\\natura2000.tif"),
  overwrite = TRUE
)



############### Plotting for bilag ################

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area,"/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(.,crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_natura2000 <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = map_baltic_sea, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = natura2000_area, 
          aes(fill = value), color = NA) +
  scale_fill_viridis_c(name = "Natura 2000", limits = c(0,1))+
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
    plot.margin=grid::unit(c(0,0,0,0), units = "mm"),
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

map_natura2000

ggsave(plot = map_natura2000,
       filename = file.path(PATHS$output_ecosystem_png, "\\natura2000.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)



