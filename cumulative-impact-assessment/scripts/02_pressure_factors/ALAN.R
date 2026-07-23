############ Kunstigt lys (ALAN) ############# 
        # Paula der har leveret datalaget

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()


# Sæt crs
target_crs <- 25832


# Læs kunstigt lys
ALAN_raw <- terra::rast(file.path(PATHS$input_pressure, "/Kunstigt lys/ALAN_Koge.tif")) 

## fejl i projekseringen i den oprindelige tif fil
ALAN_fixed <- ALAN_raw           # start forfra fra det RÅ, oprindelige raster
crs(ALAN_fixed) <- "EPSG:25832"  # omdøb kun mærkatet - ingen project() involveret

ext(ALAN_fixed)   # bør nu vise: 697750, 776000, 6096000, 6179500 (de ORIGINALE tal, bare med rigtigt CRS-navn)

#Normaliser
ALAN_log <- log10(1+ALAN_fixed)  # 
ALAN_log_min <- terra::global(ALAN_log, "min", na.rm = TRUE)[1, 1]
ALAN_log_max <- terra::global(ALAN_log, "max", na.rm = TRUE)[1, 1]
ALAN_norm <- (ALAN_log - ALAN_log_min) / (ALAN_log_max - ALAN_log_min)

# lav til sf objekt for plotting
ALAN_sf <- ALAN_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = "Artificial.light.at.night") %>%
  st_transform(., crs = target_crs)

st_bbox(ALAN_sf)

############### Plotting for bilag ################

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

viridis_start_color <- viridis_pal()(1)

map_ALAN<- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = ALAN_sf,
          aes(fill = value), color = NA) +
  scale_fill_viridis_c(name = "Kunstigt lys", limits = c(0, 1)) +
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

map_ALAN

ggsave(plot = map_ALAN,
       filename = file.path(PATHS$output_pressure_png, "\\ALAN.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)



