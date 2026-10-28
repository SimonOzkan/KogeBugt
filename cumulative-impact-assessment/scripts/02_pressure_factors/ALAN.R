############ Kunstigt lys (ALAN) ############# 
        # Paula der har leveret datalaget

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()



# Læs kunstigt lys
ALAN_raw <- terra::rast(file.path(PATHS$input_pressure, "/Kunstigt lys/ALAN_Koge.tif")) 

## fejl i projekseringen i den oprindelige tif fil
ALAN_fixed <- ALAN_raw           # start forfra fra det RÅ, oprindelige raster
crs(ALAN_fixed) <- "EPSG:25832"  # omdøb kun mærkatet - ingen project() involveret

#Fjerner 0 værdier tæt på kysten der er opstået grunden opløsligheden af det originale datalag
# Behold de faktiske værdier, men sæt 0-celler til NA
ALAN_fixed[ALAN_fixed == 0] <- NA


ALAN_koge <- terra::project(ALAN_fixed, grid_raster, method = "bilinear")

ALAN_fill <- fill_na(ALAN_koge, grid_raster, w = 3)

plot(ALAN_fill)
#Normaliser
ALAN_log <- log10(1+ALAN_fill)  # 
ALAN_log_min <- terra::global(ALAN_log, "min", na.rm = TRUE)[1, 1]
ALAN_log_max <- terra::global(ALAN_log, "max", na.rm = TRUE)[1, 1]
ALAN_norm <- (ALAN_log - ALAN_log_min) / (ALAN_log_max - ALAN_log_min)


plot(ALAN_norm)
## Lav rasterlag for normaliseret værdier

terra::writeRaster(
  ALAN_norm,
  filename  = file.path(PATHS$output_pressure_tif, "lyd_energi","ALAN_lognorm.tif"),
  overwrite = TRUE
)


# lav til sf objekt for plotting
ALAN_sf <- ALAN_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = "Artificial.light.at.night")


############### Plotting for bilag ################


map_ALAN<- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = ALAN_sf,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_ALAN


gem_trim(map_ALAN,
         file.path(PATHS$output_pressure_png, "lyd_energi","ALAN.png"),
         dpi = 360, width_cm = 18)



