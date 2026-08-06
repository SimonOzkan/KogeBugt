#----------------------------------- Kabler ----------------------- ##

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs
target_crs <- 25832

# Indlæs grid og undersøgelsesområde fra de forskellige project paths

grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(., crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = st_area(.)) %>%
  st_drop_geometry(.)

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)


## ------------------------------------------------------------------
## 0. WFS-kilder (bekræftede lagnavne)
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

kabler_combined <- bind_rows(kabler_mst_koge, kabler_energinet_koge, pipelines_koge)

kabler_buffered <- kabler_combined %>%
  st_buffer(dist = buffer_width)


## ------------------------------------------------------------------
## 4. Union, intersection med grid, og arealfraktion (samme princip som Rev/klapning)
## ------------------------------------------------------------------

kabler_union <- kabler_buffered %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf()

kabler_intersect <- st_intersection(kabler_union, grid) %>%
  st_make_valid()

kabler_area <- kabler_intersect %>%
  mutate(area_kabel = st_area(.)) %>%
  left_join(grid_area, by = "id") %>%
  mutate(value = as.numeric(area_kabel) / as.numeric(area_grid),
         value = pmin(value, 1))


## ------------------------------------------------------------------
## 5. Rasterize og gem .tif
## ------------------------------------------------------------------

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

kabler_rast <- terra::rasterize(
  terra::vect(kabler_area),
  r_template,
  field      = "value",
  fun        = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = NA
)

plot(kabler_rast, main = "Kabler og rørledninger - arealfraktion")

terra::writeRaster(
  kabler_rast,
  filename  = file.path(PATHS$output_pressure_tif, "\\kabler.tif"),
  overwrite = TRUE
)


############### Plotting for bilag ################

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

viridis_start_color <- viridis_pal()(1)

map_kabler <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = map_baltic_sea, fill = viridis_start_color, color = NA, alpha = 1) +
  geom_sf(data = kabler_area, aes(fill = value), color = NA) +
  scale_fill_viridis_c(name = "Kabler", limits = c(0, 1)) +
  coord_sf(
    crs  = 25832,
    xlim = c(696427, 775958),
    ylim = c(6096053, 6179593)
  ) +
  theme_minimal() +
  theme(
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    legend.position  = c(0.81, 0.90),
    legend.justification = "center",
    legend.title     = element_text(size = 20),
    legend.text      = element_text(size = 18),
    axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0, 0, 0, 0), units = "mm"),
    axis.ticks.length = unit(0, "pt")
  ) +
  annotation_north_arrow(
    location    = "br",
    which_north = "true",
    style       = north_arrow_fancy_orienteering,
    pad_x       = unit(3.5, "cm"),
    pad_y       = unit(1.0, "cm"),
    height      = unit(1.8, "cm"),
    width       = unit(1.8, "cm")
  ) +
  annotation_scale(
    location    = "br",
    width_hint  = 0.05,
    height      = unit(0.4, "cm"),
    bar_cols    = c("black", "white"),
    pad_x       = unit(0.2, "cm"),
    pad_y       = unit(1.5, "cm"),
    text_cex    = 1.2
  )

map_kabler

ggsave(plot = map_kabler,
       filename = file.path(PATHS$output_pressure_png, "\\kabler.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)
