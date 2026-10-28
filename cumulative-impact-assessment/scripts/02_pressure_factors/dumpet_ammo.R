#-------------------------------- Dumpet kemisk ammunition --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()

## Indlæs ammunitiontærområder (polygoner)
ammunition <- st_read(file.path(PATHS$input_pressure, "affald",
                                "EMODnet_HA_WasteDisposal_DumpedMunitions_20260609",
                                "EMODnet_HA_WasteDisposal_DumpedMunitions_pg_20260609.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  dplyr::distinct()

## ---- intersection med grid -> areal pr. celle (INGEN buffer; polygoner som de er) ----
ammunition_grid <- st_intersection(ammunition, grid_sf) %>%
  st_make_valid() %>%
  mutate(area_ammunition = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_ammunition = sum(area_ammunition), .groups = "drop")

## ---- start fra HELE grid: fraktion pr. celle, NA -> 0 ----
ammunition_gridded <- grid_sf %>%
  left_join(ammunition_grid, by = "id") %>%
  mutate(
    area_ammunition = tidyr::replace_na(as.numeric(area_ammunition), 0),  # ingen ammunition = 0
    value = pmin(as.numeric(area_ammunition) / as.numeric(area_grid), 1)
  ) %>%
  st_as_sf()

message("Antal grid-celler i alt: ", nrow(ammunition_gridded),
        " – heraf med ammunition: ", sum(ammunition_gridded$value > 0))

## ---- raster ----
ammunition_rast <- terra::rasterize(
  terra::vect(ammunition_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",      # præcis én værdi pr. id
  background = NA          # celler udenfor assessment area -> NA
)

plot(ammunition_rast)

terra::writeRaster(ammunition_rast,
                   file.path(PATHS$output_pressure_tif, "affald", "ammunitiontaeromraader.tif"),
                   overwrite = TRUE)

############### Plotting for bilag ################


# lav til sf objekt for plotting
ammunition_rast_sf <- ammunition_rast %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() 


############### Plotting for bilag ################


map_ammo<- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = ammunition_rast_sf,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_ammo


gem_trim(map_ammo,
         file.path(PATHS$output_pressure_png, "affald", "ammunitiontaeromraader.png"),
         dpi = 360, width_cm = 18)
