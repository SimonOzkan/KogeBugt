#---------------------- saltmarskes -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

## saltmarskes #https://habitats.oceanplus.org/
saltmarsk <- st_read(file.path(PATHS$input_ecosystem, "saltmarsk","WCMC027_Saltmarsh_v6_1","WCMC027_Saltmarshes_Py_v6_1.shp")) 


## Intersect med grid
saltmarskes_intersect <- saltmarsk %>%
  st_transform(., crs = target_crs) %>%
  st_intersection(grid_sf)

# Find fraktion af hver gridcelle dækket af saltmarskes og sæt til value for raster (0-1)
saltmarskes_area <- saltmarskes_intersect %>%
  mutate(area_saltmarskes = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_saltmarskes_sum = sum(area_saltmarskes, na.rm = TRUE), .groups = "drop") 

saltmarskes_genopretning <- left_join(grid_sf, saltmarskes_area) %>%
  mutate(value =area_saltmarskes_sum/area_grid) %>%
  mutate(value = ifelse(is.na(value),0,value)) %>%
  st_make_valid()


# Konverter til raster
saltmarskes_rast <- terra::rasterize(
  terra::vect(saltmarskes_genopretning),
  grid_raster,
  field    = "value",
  fun      = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = 0        # celler udenfor assessment area → NA
)

# mask
saltmarskes_rast_masked <- terra::mask(saltmarskes_rast, grid_raster)

## Tjek manglende felter
global(is.na(saltmarskes_rast_masked) & !is.na(grid_raster), "sum")[1, 1]
#
terra::writeRaster(
  saltmarskes_rast_masked,
  filename = file.path(PATHS$output_ecosystem_tif, "vigtige_habs","\\saltmarskes.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################
saltmarskes_rast_masked_sf <- saltmarskes_rast_masked %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  filter(!value == 0)

map_saltmarskes <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = saltmarskes_rast_masked_sf,
          aes(fill = "Tilstede"), shape = 21, color = NA, size = 1.5) +
  scale_fill_manual(name = NULL,
                    values = c("Tilstede" = "red", "Ikke tilstede" = "#9FBDD8"))+
  boundary+
  theme_minimal()+
  my_theme+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 16))+
  north_arrow+
  scale_bar


map_saltmarskes

ggsave(plot = map_saltmarskes,
       filename = file.path(PATHS$output_ecosystem_png,"vigtige_habs", "\\saltmarskes.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
