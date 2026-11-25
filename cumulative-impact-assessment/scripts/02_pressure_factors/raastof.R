#-------------------------------- Råstofsindvinding og områder udlagt til råstofsinvinding --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()


max_dist <- 5.1 * 10^3 
# ── 1. Hent råstofsområder fra WFS (Havplan) ─────────────────────────────────
bbox_wgs84 <- st_bbox(st_transform(assessment_area_dissolved, crs = 4326))

request_havplan <- paste0(
  "https://havplan.dk/geoserver/havplan/wfs",
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=havplan:Danmarks_havplan_af_28_juni_2024",
  "&outputFormat=application/json",
  "&BBOX=", bbox_wgs84["xmin"], ",", bbox_wgs84["ymin"], ",",
  bbox_wgs84["xmax"], ",", bbox_wgs84["ymax"],
  ",EPSG:4326"
)

havplan <- st_read(request_havplan) %>%
  st_transform(crs = target_crs)

udviklingszone_raastof <- havplan %>%
  filter(zone_title_prefix == "Udviklingszone til råstofindvinding (R)") %>%
  st_make_valid()

# ── 2. Hent aktive og expired råstofsområder fra EMODnet ─────────────────────

## Geometri over områder med råstofsindvending
raastof_emodnet <- st_read(file.path(PATHS$input_pressure, "raastof/EMODnet_HA_AggregatesExtraction_20250902/EMODnet_HA_Aggregates_pg_20250902.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() 
  #dplyr::select(CODE, NAME, ID,STATUS,Year_start,Year_end,SeaBasin,geometry)


### kildeinformation med mængde der indvendes
point_amount <- read_excel("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/raastof/EMODnet_HA_AggregatesExtraction_20250902/EMODnet_HA_AggregatesExtraction_20250902.xls", 
           sheet = "pt_Data_20250902") 
  #dplyr::select(ID, A_ACT_km2, YEAR,PAMOUNT_m3,RAMOUNT_m3,RAMOUNT_m3,EAMOUNT_t)


point_amount %>% filter(grepl("^A-DK|^A-SE", ID)) %>%
  summarise(min = min(YEAR), max = max(YEAR), n = n())
point_amount %>% filter(grepl("^A-DK|^A-SE", ID)) %>% count(YEAR)

pg_amount <- read_excel("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/raastof/EMODnet_HA_AggregatesExtraction_20250902/EMODnet_HA_AggregatesExtraction_20250902.xls", 
                       sheet = "pg_Data_20250902")


periode_r <- 2018:2025                      


amt <- point_amount %>%
  filter(YEAR %in% periode_r,
         grepl("^A-DK|^A-SE", ID)) %>%
  mutate(CODE = str_extract(NOTES, "(?<=Area number:\\s)\\S+") %>% str_trim()) %>%
  group_by(CODE) %>%
  summarise(mean_m3_yr = mean(EAMOUNT_m3, na.rm = TRUE), .groups = "drop")

raastof_all <- raastof_emodnet %>%
  filter(Country %in% c("Denmark","Sweden")) %>%
  st_make_valid() %>%
  left_join(amt, by = "CODE") %>%
  filter(!is.na(mean_m3_yr),
         !Year_end <= 2018) %>%
  group_by(CODE) %>%                                        # ÉN geometri pr. indvindingsområde
  summarise( mean_m3_yr = first(mean_m3_yr), .groups = "drop") %>%   # geometri unioner automatisk
  st_make_valid() %>%
  mutate(area_r        = as.numeric(st_area(.)),            # areal af den SAMLEDE polygon
         m3_per_km2    = (mean_m3_yr / area_r) * 1e6,
         m3_per_km2_lg = log10(m3_per_km2 + 1))


max_r     <- max(raastof_all$m3_per_km2,    na.rm = TRUE)
max_r_log <- max(raastof_all$m3_per_km2_lg, na.rm = TRUE)


raastof_cells <- st_intersection(raastof_all, grid_sf) %>%
  st_drop_geometry() %>%
  mutate(value =m3_per_km2,
         value_lg      = log10(value + 1),                # log AF celle-intensiteten
         value_norm    = value    / max_r,                # min=0 -> = (value-0)/(max-0)
         value_norm_lg = value_lg / max_r_log)            # max_r_log = log10(max_r + 1)

## 2. Fordel til grid, 0 hvor ingen indvinding ────────────────────────────────
raastof_gridded <- grid_sf %>%
  left_join(raastof_cells, by = "id") %>%
  mutate(value_norm    = tidyr::replace_na(value_norm, 0),
         value_norm_lg = tidyr::replace_na(value_norm_lg, 0)) %>%
  st_make_valid()

## 3. Konverter til raster og gem ─────────────────────────────────────────────
raastof_rast <- terra::rasterize(
  terra::vect(raastof_gridded), grid_raster,
  field = "value_norm", fun = "max", background = NA)

raastof_rast_lg_norm <- terra::rasterize(
  terra::vect(raastof_gridded), grid_raster,
  field = "value_norm_lg", fun = "max", background = NA)

par(mfrow = c(1,2))
plot(raastof_rast)
plot(raastof_rast_lg_norm)

### Tilføj decay
raastof_rast_decay <- decay_focal(raastof_rast,max_dist)

raastof_rast_lg_norm_decay <- decay_focal(raastof_rast_lg_norm, max_dist)

plot(raastof_rast_decay)
plot(raastof_rast_lg_norm_decay)


terra::writeRaster(raastof_rast_decay,
                   file.path(PATHS$output_pressure_tif, "restriktive_omraader", "raastof_norm_decay.tif"), overwrite = TRUE)

terra::writeRaster(raastof_rast_lg_norm_decay,
                   file.path(PATHS$output_pressure_tif, "restriktive_omraader", "raastof_log_norm_decay.tif"), overwrite = TRUE)

## 4.1  Plotting for bilag - ikke log transformeret ──────────────────────────
raastof_sf <- raastof_rast %>%
  terra::as.polygons(dissolve = FALSE) %>% st_as_sf() %>% st_make_valid()

map_raastof_norm <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +
  geom_sf(data = raastof_sf, aes(fill = value_norm), color = NA) +
  color_viridis + boundary + theme_minimal() + my_theme + north_arrow + scale_bar
map_raastof_norm
gem_trim(map_raastof_norm,
         file.path(PATHS$output_pressure_png, "restriktive_omraader", "raastof_norm.png"), dpi = 360, width_cm = 18)

## 4.2  Plotting for bilag - log transformeret ───────────────────────────────
raastof_sf_lg <- raastof_rast_lg_norm %>%
  terra::as.polygons(dissolve = FALSE) %>% st_as_sf() %>% st_make_valid()

map_raastof_lg <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +
  geom_sf(data = raastof_sf_lg, aes(fill = value_norm_lg), color = NA) +
  color_viridis + boundary + theme_minimal() + my_theme + north_arrow + scale_bar
map_raastof_lg
gem_trim(map_raastof_lg,
         file.path(PATHS$output_pressure_png, "restriktive_omraader", "raastof_log_norm.png"), dpi = 360, width_cm = 18)
