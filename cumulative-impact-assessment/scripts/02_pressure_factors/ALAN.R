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

#Fjerner 0 værdier tæt på kysten der er opstået grunden opløsligheden af det originale datalag
# Behold de faktiske værdier, men sæt 0-celler til NA
ALAN_fixed[ALAN_fixed == 0] <- NA



#Normaliser
ALAN_log <- log10(1+ALAN_fixed)  # 
ALAN_log_min <- terra::global(ALAN_log, "min", na.rm = TRUE)[1, 1]
ALAN_log_max <- terra::global(ALAN_log, "max", na.rm = TRUE)[1, 1]
ALAN_norm <- (ALAN_log - ALAN_log_min) / (ALAN_log_max - ALAN_log_min)

## Lav rasterlag for normaliseret værdier

terra::writeRaster(
  ALAN_norm,
  filename  = file.path(PATHS$output_pressure_tif, "ALAN_lognorm.tif"),
  overwrite = TRUE
)


# lav til sf objekt for plotting
ALAN_sf <- ALAN_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = "Artificial.light.at.night") %>%
  st_transform(., crs = target_crs)

st_bbox(ALAN_sf)

############### Plotting for bilag ################

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

viridis_start_color <- viridis_pal()(1)

map_ALAN<- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = ALAN_sf,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_ALAN

ggsave(plot = map_ALAN,
       filename = file.path(PATHS$output_pressure_png, "\\ALAN.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)



