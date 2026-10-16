#-------------------------------- Marint affald --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()


malt <-read_csv2(file.path(PATHS$input_pressure, "affald","MALT_EQR_per_category.csv"))
grid_eea <-st_read(file.path(PATHS$input_pressure, "affald","assessment_grid_100_20.shp"))

malt_sf <- left_join(grid_eea,malt, by = c("GRIDCOD" = "GRIDCODE")) %>%
  st_make_valid() %>%
  st_transform(.,crs = target_crs) %>%
  st_intersection(assessment_area_dissolved) 


MALT_raster <- terra::rasterize(
  terra::vect(malt_sf),
  grid_raster,
  field      = "EQR",
  fun        = "max",
  background = NA,
  touches    = TRUE        # <- enhver celle, bufferen RØRER, får værdien
) %>%
  terra::mask(assessment_area_vect)

#names(MALT_raster) <- "value"
plot(MALT_raster)


terra::writeRaster(affald_raster,
                   file.path(PATHS$output_pressure_tif, "affald", "MALT_eqr.tif"),
                   overwrite = TRUE)

###############################################

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

malt_sf_grid <- left_join(grid_eea,malt, by = c("GRIDCOD" = "GRIDCODE")) %>%
  st_make_valid() %>%
  st_transform(.,crs = target_crs) %>%
  st_intersection(grid) 


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
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  #geom_sf(data = assessment_area_dissolved, fill = NA, color = "white", alpha = 1) +
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
  north_arrow +
  scale_bar
map_MALT

ggsave(plot = map_MALT,
       filename = file.path(PATHS$output_pressure_png, "affald", "MALT.png"),
       bg = "white", height = 18, width = 18, dpi = 300)

