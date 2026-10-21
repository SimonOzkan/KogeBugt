#---------------------- Områder egnet til Marsvin -----------------
# EMODnet datalag
# https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/6d617269-6e65-696e-666f-000000008820 
# Indlæs pakker og set path fra source setup fil
#----------------------------------- Bypass ----------------------- ##
source(here::here("scripts/00_setup.R"))
PATHS <- set_project_paths()
#####
marsvin_nc_path <- file.path(PATHS$input_ecosystem,"/Pattedyr/porpoiseHabitatSuitability_with_SeasonYears.nc")

marsvin_r <- terra::rast(marsvin_nc_path, subds = "habitat_suitability")

# Behold kun lag 5 (Summer 2013-2022) og 6 (Winter 2013-2022)
r_2013 <- marsvin_r[[c(5, 6)]]
names(r_2013) <- c("summer_2013_2022", "winter_2013_2022")

plot(r_2013[["summer_2013_2022"]])

r_mean_2013_2022 <- (r_2013[["summer_2013_2022"]]+r_2013[["winter_2013_2022"]])/2
names(r_mean_2013_2022) <- "value"


marsvin_250 <- terra::project(r_mean_2013_2022, grid_raster, method = "bilinear")


fill_na <- function(r, template, w = 3, max_iter = 200) {
  out <- r
  for (i in seq_len(max_iter)) {
    # tæl NA-celler, der ligger INDEN for template-området
    na_tilbage <- global(is.na(out) & !is.na(template), "sum")[1, 1]
    if (na_tilbage == 0) break
    filled <- terra::focal(out, w = w, fun = "mean", na.rm = TRUE)  # nabo-gennemsnit
    out    <- terra::cover(out, filled)                             # behold eksisterende, fyld kun NA
  }
  mask(out, template)   
}

marsvin_fill <- fill_na(marsvin_250, grid_raster, w = 3)


plot(marsvin_fill)


terra::writeRaster(
  marsvin_fill,
  filename = file.path(PATHS$output_ecosystem_tif,"pattedyr", "/marsvin_250.tif"),
  overwrite = TRUE
)



# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
marsvin_sf <- marsvin_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()


map_marsvin <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = marsvin_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_marsvin


gem_trim(map_marsvin,
         file.path(PATHS$output_ecosystem_png,"pattedyr","marsvin_dist_emodnet.png"),
         dpi = 360, width_cm = 18)



