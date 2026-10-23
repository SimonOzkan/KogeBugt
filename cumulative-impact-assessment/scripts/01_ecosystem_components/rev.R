#---------------------- Rev -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

## Hent Rev-lag fra MST's WFS-service (se data folder / metadata_log)

wfs_url <- "https://gisportal.mst.dk/server/services/ekstern/KDI_anlaeg_paa_soeterritoriet/MapServer/WFSServer"

request <- paste0(
  wfs_url,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=KDI_anlaeg_paa_soeterritoriet:Anlaeg_paa_soeterritoriet__polygon_",
  "&outputFormat=GEOJSON"
)

anlaeg_polygon <- st_read(request) %>%
  st_transform(crs = target_crs)

rev_poly <- anlaeg_polygon %>%
  filter(TYPE == "Rev",
         KATEGORI == "Natur(gen)opretning")


## Intersect med grid
rev_intersect <- st_intersection(rev_poly, grid_sf) %>%
  st_make_valid()

# Find fraktion af hver gridcelle dækket af rev og sæt til value for raster (0-1)
rev_area <- rev_intersect %>%
  mutate(area_rev = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_rev_sum = sum(area_rev, na.rm = TRUE), .groups = "drop") 

rev_genopretning <- left_join(grid_sf, rev_area) %>%
  mutate(value =area_rev_sum/area_grid) %>%
  mutate(value = ifelse(is.na(value),0,value))
  st_make_valid()

# Konverter til raster
rev_rast <- terra::rasterize(
  terra::vect(rev_genopretning),
  grid_raster,
  field    = "value",
  fun      = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = 0        # celler udenfor assessment area → NA
)

# mask
rev_rast_masked <- terra::mask(rev_rast, grid_raster)

## Tjek manglende felter
global(is.na(out) & !is.na(template), "sum")[1, 1]
#
terra::writeRaster(
  rev_rast_masked,
  filename = file.path(PATHS$output_ecosystem_tif, "vigtige_habs","\\rev.tif"),
  overwrite = TRUE
)


############### Plotting for bilag ################
rev_rast_masked_sf <- rev_rast_masked %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_rev <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = rev_rast_masked_sf,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_rev

ggsave(plot = map_rev,
       filename = file.path(PATHS$output_ecosystem_png,"vigtige_habs", "\\rev.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
