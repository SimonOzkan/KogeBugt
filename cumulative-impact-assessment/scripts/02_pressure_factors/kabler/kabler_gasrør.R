#----------------------------------- Kabler og gasrør -----------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()
## --- WFS-kilder ---
wfs_url_anlaeg_linje  <- "https://gisportal.mst.dk/server/services/ekstern/KDI_anlaeg_paa_soeterritoriet/MapServer/WFSServer"
wfs_url_el_kabel      <- "https://agis.energinet.dk/server/services/INSPIRE/XP_el_Inspir/MapServer/WFSServer"
wfs_url_olie_gas_pipe <- "https://ows.emodnet-humanactivities.eu/wfs"

## --- 1. Hent data ---
anlaeg_linje <- st_read(paste0(wfs_url_anlaeg_linje,
                               "?service=WFS&version=2.0.0&request=GetFeature",
                               "&typeNames=KDI_anlaeg_paa_soeterritoriet:Anlaeg_paa_soeterritoriet__linje_",
                               "&outputFormat=GEOJSON")) %>% st_transform(target_crs)

kabler_mst <- anlaeg_linje %>%
  filter(TYPE %in% c("Kabler, rørledninger og klimanalæg"))

kabler_energinet <- st_read(paste0(wfs_url_el_kabel,
                                   "?service=WFS&version=2.0.0&request=GetFeature",
                                   "&typeNames=XP_el_Inspir:KabelTrace&outputFormat=GEOJSON"), quiet = TRUE) %>%
  st_transform(target_crs)

pipelines_emodnet <- st_read(paste0(wfs_url_olie_gas_pipe,
                                    "?service=WFS&version=2.0.0&request=GetFeature",
                                    "&typeNames=emodnet:pipelines&outputFormat=application/json"), quiet = TRUE) %>%
  st_transform(target_crs)

## --- 2. Klip til undersøgelsesområdet ---
kabler_koge_mst <-   kabler_mst %>%
  st_intersection(assessment_area_dissolved) %>%
  transmute(kilde = "MST")
  

# ### Kontek elektrode anglæg anvendes om preslag i kemisk forurening
# kontek_elektrode <- kabler_energinet %>%
#   filter(Spaending == "Elektrode")
# st_write(kontek_elektrode,file.path(PATHS$input_pressure, "kemisk_forurening","kontek_elektrode","kontek_elektrode.shp"), append = FALSE)  


kabler_koge_energi <-   kabler_energinet %>%
  filter(!Spaending == "Elektrode") %>%
  st_intersection(assessment_area_dissolved) %>%
  transmute(kilde = "energinet") 
  
## Samlet kabler
kabler_koge <- bind_rows(kabler_koge_mst,kabler_koge_energi)


### gasrør
gasrør_koge <- pipelines_emodnet %>%
  st_intersection(assessment_area_dissolved) 

plot(st_geometry(gasrør_koge))
## --- 3. Funktion: linjer -> korridor -> arealfraktion pr. celle -> raster ---.
# Erstat med officiel kabel-/rørkorridorbredde for danske farvande, hvis den findes.
buffer_width <- 5 # Sat til 5 meter, hvilket formentlig fortsat er lidt større end rigtigt. Der kommer effekt-distance på

korridor_frac <- function(linjer, grid_sf, grid_raster, buffer_width = 5) {
  korridor <- linjer %>%
    st_buffer(buffer_width) %>%
    st_union() %>% st_make_valid() %>% st_as_sf()
  
  # areal af korridor pr. gridcelle
  frac_tbl <- st_intersection(korridor, grid_sf) %>%
    st_make_valid() %>%
    mutate(area_korridor = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(area_korridor = sum(area_korridor, na.rm = TRUE), .groups = "drop")
  
  # join til ALLE celler: fraktion = korridorareal / celleareal, NA -> 0
  grid_frac <- grid_sf %>%
    left_join(frac_tbl, by = "id") %>%
    mutate(area_cell = as.numeric(st_area(.)),
           value = ifelse(is.na(area_korridor), 0, area_korridor / area_cell),
           value = pmin(value, 1))                 # clamp mod floating point
  
  r <- terra::rasterize(terra::vect(grid_frac),
                        grid_raster,
                        field = "value", 
                        fun = "max",
                        background = NA) %>%
    terra::mask(grid_raster)                        # indenfor område: 0..1, udenfor: NA
  
  list(raster = r, korridor = korridor, grid_frac = grid_frac)
}

kabler <- korridor_frac(kabler_koge, grid_sf, grid_raster, buffer_width)
gasrør <- korridor_frac(gasrør_koge, grid_sf, grid_raster, buffer_width)

## --- 4. Gem raster ---
plot(kabler$raster, main = "Kabler og rørledninger – arealfraktion")
plot(gasrør$raster, main = "Gasrør – arealfraktion")



terra::writeRaster(kabler$raster,
                   file.path(PATHS$output_pressure_tif,"kabler","kabler.tif"), overwrite = TRUE)
terra::writeRaster(gasrør$raster,
                   file.path(PATHS$output_pressure_tif,"kabler" ,"gasrør.tif"), overwrite = TRUE)

#### All tif samlet ######
terra::writeRaster(kabler$raster,
                   file.path(PATHS$output_pressure_tif,"all_tif","kabler.tif"), overwrite = TRUE)
terra::writeRaster(gasrør$raster,
                   file.path(PATHS$output_pressure_tif,"all_tif" ,"gasrør.tif"), overwrite = TRUE)



kabler_sf <- kabler$grid_frac %>%
  filter(!value == 0)


## --- 5. Bilag: kabler
map_kabler <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = kabler_sf,
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
## --- 5. Bilag: gasrør
rør_sf <- gasrør$grid_frac %>%
  filter(!value == 0)
map_pipes <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = rør_sf,
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




gem_trim(map_kabler,
         file.path(PATHS$output_pressure_png, "kabler", "map_kabler_pa.png"),
         dpi = 360, width_cm = 18)

gem_trim(map_pipes,
         file.path(PATHS$output_pressure_png, "kabler", "map_rør_pa.png"),
         dpi = 360, width_cm = 18)
