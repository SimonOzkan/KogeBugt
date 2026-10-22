#---------------------- Fund og fortidsminder -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
#Hent tilgængelige project paths
PATHS <- set_project_paths()

## Indlæs 

#Danmark fortidsminder og arkæologi
#https://www.kulturarv.dk/fundogfortidsminder/Download/
anlaeg_areal <- st_read(file.path(PATHS$input_ecosystem, "fortidsminder/anlaeg_areal_all_25832_shp/anlaeg_areal_all_25832.shp")) 
#Sverige fortidsminder og arkæologi
#https://www.raa.se/hitta-information/oppna-data/oppna-data-portal/
anlaeg_sverige <- st_read(file.path(PATHS$input_ecosystem, "fortidsminder/lämningar_sverige.gpkg"),layer = "lämningar_sverige_polygon")
anlaeg_sverige <- st_transform(anlaeg_sverige, crs = target_crs)


#--------- intersection med undersøgelsesområdet og union for at undgå overlap mellem lag

df_dk_union <- st_intersection(anlaeg_areal, assessment_area_dissolved) %>%
  st_union() %>%
  st_as_sf() %>%
  st_make_valid()

df_sve_union <- st_intersection(anlaeg_sverige, assessment_area_dissolved) %>%
  st_union() %>%
  st_as_sf() %>%
  st_make_valid()


#### Intersection med grid og best areal og fraktionen 
df_dk_koge <- st_intersection(df_dk_union,grid_sf) 

df_sve_koge <- st_intersection(df_sve_union,grid_sf) 

df1 <- df_dk_koge %>%
  mutate(A_fortid = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(A_fortid = sum(A_fortid, na.rm = TRUE), .groups = "drop")


df2 <- df_sve_koge %>%
  mutate(A_fortid = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(A_fortid = sum(as.numeric(A_fortid), na.rm = TRUE), .groups = "drop")


# Find fraktion are hver gridcelle dækket af fortidsminder og sæt til value for raster (0-1)
df_bind <- bind_rows(df1,df2) %>%
  left_join(grid_sf) %>%
  mutate(value = A_fortid / area_grid) %>%
  st_as_sf() %>%
  st_make_valid()

# Konverter til raster


fortids_rast <- terra::rasterize(
  terra::vect(df_bind),
  grid_raster,
  field    = "value",
  fun      = "max",      
  background = 0        # Sætter andre celler til 0
)

# mask til undersøgelsesområdet
fortids_rast_masked <- terra::mask(fortids_rast, grid_raster)

plot(fortids_rast_masked)

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

fortids_rast_filled <- fill_na(fortids_rast_masked, grid_raster, w = 3)


### gem raster
terra::writeRaster(
  fortids_rast_filled,
  filename = file.path(PATHS$output_ecosystem_tif, "fortid","fortids.tif"),
  overwrite = TRUE
)



############### Plotting for bilag ################
fortids_sf <- fortids_rast_filled %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 

map_fortidsminder<- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  # geom_sf(data = assessment_area_dissolved,
  #         aes(fill = NA), color = "black", alpha = 1) +
  geom_sf(data = fortids_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_fortidsminder


gem_trim(map_fortidsminder,
         file.path(PATHS$output_ecosystem_png, "fortid", "fortidsminder.png"),
         dpi = 360, width_cm = 18)
