#---------------------- Fund og fortidsminder -----------------

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
grid_raster <- terra::rast(file.path(PATHS$input_assessment_area, "/geotif/assessment_area.tif"))

## Indlæs 

#https://www.kulturarv.dk/fundogfortidsminder/Download/
anlaeg_areal <- st_read(file.path(PATHS$input_ecosystem, "fortidsminder/anlaeg_areal_all_25832_shp/anlaeg_areal_all_25832.shp"))

# WFS fra arealinformation
# ── 1. Hent råstofsområder fra WFS (Havplan) ─────────────────────────────────

request_fortidsminder <- paste0(
  "https://www.kulturarv.dk/ffgeoserver/public/wfs",
  "?service=WFS",
  "&version=1.0.0",
  "&request=GetFeature",
  "&typeNames=public:fundogfortidsminder_areal_beskyttelse",
  "&outputFormat=application/json"
)

fortidsminder <- st_read(request_fortidsminder) %>%
  st_transform(crs = target_crs) %>%
  select(-id)


#--------- intersection med undersøgelsesområdet

# Først "klippes" der til undersøgelsesområdet for at optimering af union hastighed
fortidsminder_koge <- st_intersection(fortidsminder,grid)

message("Antal fortidsminder i undersøgelsesområdet: ", paste(nrow(fortidsminder_koge)))

# Find fraktion are hver gridcelle dækket af fortidsminder og sæt til value for raster (0-1)
fortidsminder_koge_area <-fortidsminder_koge %>%
  mutate(area_fortids = st_area(.)) %>%
  left_join(grid_area, by = "id" ) %>%
  mutate(value = as.numeric(area_fortids) / as.numeric(area_grid),
         value = pmin(value, 1)) 


# Konverter til raster


fortids_rast <- terra::rasterize(
  terra::vect(fortidsminder_koge_area),
  grid_raster,
  field    = "value",
  fun      = "max",      # der bør ikke være overlap, derfor tages max
  background = NA        # celler udenfor assessment area → NA
)

plot(fortids_rast)


terra::writeRaster(
  fortids_rast,
  filename = file.path(PATHS$output_ecosystem_tif, "\\fortids.tif"),
  overwrite = TRUE
)



############### Plotting for bilag ################

map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_fortidsminder<- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = fortidsminder_koge_area, color = "yellow", size = 3) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_fortidsminder

ggsave(plot = map_fortidsminder,
       filename = file.path(PATHS$output_ecosystem_png,"/arkaeologi/" ,"fortidsminder.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)

