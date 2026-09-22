#----------------------------------- Kabler og gasrør----------------------- ##

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

## ------------------------------------------------------------------
## WFS-kilder 
## ------------------------------------------------------------------

wfs_url_anlaeg_linje <- "https://gisportal.mst.dk/server/services/ekstern/KDI_anlaeg_paa_soeterritoriet/MapServer/WFSServer"
wfs_url_el_kabel <- "https://agis.energinet.dk/server/services/INSPIRE/XP_el_Inspir/MapServer/WFSServer"
wfs_url_olie_gas_pipe <- "https://ows.emodnet-humanactivities.eu/wfs"


## ------------------------------------------------------------------
## 1. Hent data fra hver kilde
## ------------------------------------------------------------------

# --- MST: Anlæg på søterritoriet, linje-lag (søkabler mv.) ---
request_anlaeg <- paste0(
  wfs_url_anlaeg_linje,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=KDI_anlaeg_paa_soeterritoriet:Anlaeg_paa_soeterritoriet__linje_",
  "&outputFormat=GEOJSON"
)

anlaeg_linje <- st_read(request_anlaeg) %>%
  st_transform(crs = target_crs)


kabler_mst <- anlaeg_linje %>%
  filter(TYPE %in% c("Kabler, rørledninger og klimanalæg"))  


# --- Energinet: INSPIRE el-kabler ---
request_el_kabel <- paste0(
  wfs_url_el_kabel,
  "?service=WFS&version=2.0.0&request=GetFeature",
  "&typeNames=XP_el_Inspir:KabelTrace",
  "&outputFormat=GEOJSON"
)

kabler_energinet <- st_read(request_el_kabel, quiet = TRUE) %>%
  st_transform(crs = target_crs)


# --- EMODnet Human Activities: olie/gas rørledninger ---
request_pipe <- paste0(
  wfs_url_olie_gas_pipe,
  "?service=WFS&version=2.0.0&request=GetFeature",
  "&typeNames=emodnet:pipelines",
  "&outputFormat=application/json"
)

pipelines_emodnet <- st_read(request_pipe, quiet = TRUE) %>%
  st_transform(crs = target_crs)


## ------------------------------------------------------------------
## 2. Klip alle tre kilder til undersøgelsesområdet
## ------------------------------------------------------------------

kabler_mst_koge <- kabler_mst %>%
  st_make_valid() %>%
  st_intersection(assessment_area_dissolved) %>%
  mutate(kilde = "MST_anlaeg", geometry = geometry) %>%
  select(kilde, geometry)

kabler_energinet_koge <- kabler_energinet %>%
  st_make_valid() %>%
  st_intersection(assessment_area_dissolved) %>%
  mutate(kilde = "Energinet") %>%
  select(kilde, geometry)

pipelines_koge <- pipelines_emodnet %>%
  st_make_valid() %>%
  st_intersection(assessment_area_dissolved) %>%
  mutate(kilde = "EMODnet_pipeline") %>%
  select(kilde, geometry)


## ------------------------------------------------------------------
## 3. Saml alle kilder og buffer til en realistisk korridorbredde
## ------------------------------------------------------------------

# NB: Kabler/rørledninger er linjegeometri uden areal i sig selv. For at kunne
# beregne arealfraktion per gridcelle (som de øvrige presfaktor-lag) bufres
# linjerne til en antaget påvirkningskorridor.
# TODO: buffer_width er et PLACEHOLDER-skøn (25 m til hver side = 50 m samlet
# korridor). Undersøg om der findes en officiel sikkerhedszone/kabelkorridor-
# bredde for danske farvande (typisk angivet i meter i lovgivning/retningslinjer
# for søkabler og rørledninger) og erstat med denne, i stedet for et gæt.
buffer_width <- 25

kabler_combined <- bind_rows(kabler_mst_koge, kabler_energinet_koge)

gasrør <- pipelines_koge


kabler_buffered <- kabler_combined %>%
  st_buffer(dist = buffer_width)

gasrør_buffered <- gasrør %>%
  st_buffer(dist = buffer_width)
## ------------------------------------------------------------------
## 4. Union, intersection med grid, og arealfraktion (samme princip som Rev/klapning)
## ------------------------------------------------------------------

kabler_union <- kabler_buffered %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf()

kabler_intersect <- st_intersection(kabler_union, grid) %>%
  st_make_valid() %>%
  dplyr::select(-area_grid)

kabler_area <- kabler_intersect %>%
  mutate(area_kabel = st_area(.)) %>%
  left_join(st_drop_geometry(grid), by = "id") %>%
  mutate(value = as.numeric(area_kabel) / as.numeric(area_grid),
         value = pmin(value, 1))

### Gasrør
gasrør_union <- gasrør_buffered %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf()

gasrør_intersect <- st_intersection(gasrør_union, grid) %>%
  st_make_valid() %>%
  dplyr::select(-area_grid)

gasrør_area <- gasrør_intersect %>%
  mutate(area_gasrør = st_area(.)) %>%
  left_join(st_drop_geometry(grid), by = "id") %>%
  mutate(value = as.numeric(area_gasrør) / as.numeric(area_grid),
         value = pmin(value, 1))

## ------------------------------------------------------------------
## 5. Rasterize og gem .tif
## ------------------------------------------------------------------

kabler_rast <- terra::rasterize(
  terra::vect(kabler_area),
  grid_raster,
  field      = "value",
  fun        = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = NA
)

gasrør_rast <- terra::rasterize(
  terra::vect(gasrør_area),
  grid_raster,
  field      = "value",
  fun        = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = NA
)

plot(kabler_rast, main = "Kabler og rørledninger - arealfraktion")

plot(gasrør_rast, main = "Gasrør - arealfraktion")

terra::writeRaster(
  kabler_rast,
  filename  = file.path(PATHS$output_pressure_tif, "\\kabler.tif"),
  overwrite = TRUE
)

# gasrør
terra::writeRaster(
  gasrør_rast,
  filename  = file.path(PATHS$output_pressure_tif, "\\gasrør.tif"),
  overwrite = TRUE
)

############### Plotting for bilag ################
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

map_kabler_pa <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = kabler_area, color = "yellow", size = 1.5) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar


map_kabler_pa

ggsave(plot = map_kabler_pa,
       filename = file.path(PATHS$output_pressure_png, "anlaeg/map_kabler_pa.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)



map_rør_pa <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = gasrør_area, color = "yellow", size = 1.5) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar



ggsave(plot = map_rør_pa,
       filename = file.path(PATHS$output_pressure_png, "anlaeg/map_rør_pa.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)


