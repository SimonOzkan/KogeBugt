#----------------------------------- Bypass ----------------------- ##
source(here::here("scripts/00_setup.R"))
PATHS <- set_project_paths()

## ------------------------------------------------------------------
## Hent Bypass data fra WFS
## ------------------------------------------------------------------
wfs_url_bypass <- "https://gisportal.mst.dk/server/services/ekstern/KDI_Bypass/MapServer/WFSServer"

request_bypass <- paste0(
  wfs_url_bypass,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=esri:Bypassomraader",
  "&outputFormat=GEOJSON"
)

bypass_all <- st_read(request_bypass) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  dplyr::select(OBJECTID,Maengde, geometry) %>%
  mutate(area_bypass = as.numeric(st_area(.)),
         m3_per_km2 = (as.numeric(Maengde) / area_bypass )*10^6,
         m3_per_km2_lg = log10(m3_per_km2+1))

#
max_bypass <- max(bypass_all$m3_per_km2, na.rm = TRUE)
max_bypass_log <- max(bypass_all$m3_per_km2_lg, na.rm = TRUE)


bypass_cells <- st_intersection(bypass_all, grid_sf) %>%
  st_drop_geometry() %>%
  mutate(value = m3_per_km2, 
         value_lg      = m3_per_km2_lg,                # log AF celle-intensiteten
         value_norm    = value    / max_bypass,             # min=0 -> = (value-0)/(max-0)
         value_norm_lg = value_lg / max_bypass_log) %>%          # max_bypass_log = log10(max_bypass + 1)       # 0-1
  dplyr::select(id,value,value_norm,value_lg,value_norm_lg )


## 3. Fordel til grid, 0 hvor ingen bypass ----------------------------------
bypass_gridded <- grid_sf %>%
  left_join(bypass_cells, by = "id") %>%
  mutate(value_norm = tidyr::replace_na(value_norm, 0),
         value_norm_lg = tidyr::replace_na(value_norm_lg,0)) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 4. Lav til raster
## ------------------------------------------------------------------

## ------------------------------------------------------------------
## 3 Konverter til raster og gem
## ------------------------------------------------------------------

bypass_rast <- terra::rasterize(
  terra::vect(bypass_gridded),
  grid_raster,
  field      = "value_norm",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)



bypass_rast_lg_norm <- terra::rasterize(
  terra::vect(bypass_gridded),
  grid_raster,
  field      = "value_norm_lg",
  fun        = "max",      # hvis overlap: tag max fraktion
  background = NA          # celler udenfor assessment area sættes NA
)

par(mfrow = c(1,2))
plot(bypass_rast)
plot(bypass_rast_lg_norm)
# bypass 8900
max_dist <- 6400

bypass_decay <- decay_focal(bypass_rast, max_dist)
bypass_lg_decay <- decay_focal(bypass_rast_lg_norm, max_dist)


par(mfrow = c(1,2))
plot(bypass_decay)
plot(bypass_lg_decay)



save_raster <-function(r, save_name){
  terra::writeRaster(
    r,
    filename = file.path(PATHS$output_pressure_tif, "iei",paste0(save_name,".tif")),
    overwrite = TRUE
  )
}

save_raster(bypass_decay, save_name = "bypass_decay_norm")
save_raster(bypass_lg_decay, save_name = "bypass_lg_decay")
## ------------------------------------------------------------------
## Plot 
## ------------------------------------------------------------------

bypass_sf <- r_bypass_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_bypass <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = bypass_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_bypass

gem_trim(map_bypass,
         file.path(PATHS$output_pressure_png, "anlaeg", "bypass.png"),
         dpi = 360, width_cm = 18)

