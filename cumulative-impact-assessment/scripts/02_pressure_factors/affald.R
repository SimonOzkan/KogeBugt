#-------------------------------- Marint affald --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()


malt <-read_csv2(file.path(PATHS$input_pressure, "affald","MALT_EQR_per_category.csv"))

#### One out all out
malt_worst <- malt%>%
  group_by(GRIDCODE) %>%
  slice_min(EQR, n = 1, with_ties = FALSE)
  
grid_eea <-st_read(file.path(PATHS$input_pressure, "affald","assessment_grid_100_20.shp"))

malt_sf <- left_join(grid_eea,malt_worst, by = c("GRIDCOD" = "GRIDCODE")) %>%
  st_make_valid() %>%
  st_transform(.,crs = target_crs) %>%
  st_intersection(assessment_area_dissolved) %>%
  mutate(EQR = as.numeric(EQR))

malt_gridded <- st_intersection(malt_sf, grid_sf) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(EQR = mean(EQR, na.rm = TRUE), .groups = "drop" )

malt_gridded_sf <- left_join(grid_sf, malt_gridded)

MALT_raster <- terra::rasterize(
  terra::vect(malt_gridded_sf),
  grid_raster,
  field      = "EQR",
  fun        = "max",
  background = NA       # <- enhver celle, bufferen RØRER, får værdien
) 
  
#names(MALT_raster) <- "value"
plot(MALT_raster)

malt_raster_fill <- fill_na(MALT_raster, grid_raster, w = 3)

plot(malt_raster_fill)

terra::writeRaster(malt_raster_fill,
                   file.path(PATHS$output_pressure_tif, "affald", "MALT_eqr.tif"),
                   overwrite = TRUE)

###############################################


malt_sf_grid <- malt_raster_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()


# Industri 
malt_eqr <- malt_sf_grid %>%
  mutate(Status = case_when(
    EQR >= 0.8 ~ "High",
    EQR >= 0.6 & EQR< 0.8 ~ "Good",
    EQR >= 0.4 & EQR< 0.6 ~ "Moderate",
    EQR >= 0.2 & EQR< 0.4 ~ "Poor",
    EQR >= 0 & EQR < 0.2 ~ "Bad",
    is.na(EQR) ~ "No Data"),
  Status = factor(Status, levels = c("High", "Good","Moderate", "Poor","Bad",  "No Data","Not Included"))) 

map_MALT <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = malt_eqr, aes(fill = Status), color = NA) +
  scale_fill_manual(
    values = c(
      "Bad" = "red",
      "Poor" = "orange",
      "Moderate" = "yellow",
      "Good" = "green",
      "High" = "blue",
      "No Data" = "grey91"
    )
  ) +
  boundary +
  theme_minimal() +
  my_theme +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))+
  north_arrow +
  scale_bar
map_MALT

gem_trim(map_MALT,
         file.path(PATHS$output_pressure_png, "affald\\MALT.png"),
         dpi = 360, width_cm = 18)

