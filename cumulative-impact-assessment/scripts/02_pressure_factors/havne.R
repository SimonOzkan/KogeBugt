#---------------------- Havne: industri (cargo) + lystbåd (fraktion) -----------------
#   - Industrihavne : vægtet efter godsmængde (import/eksport), normaliseret 0-1
#   - Lystbådshavne : fysisk tilstedeværelse -> arealfraktion pr. celle
# Begge tildeles en buffer, så punkterne registrerer på 250 m-grid'et.

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()


## ------------------------------------------------------------------
## Parametre 
## ------------------------------------------------------------------
# Bufferradius i meter. 
buffer_industri  <- 100
buffer_lystbaad  <- 100


## ------------------------------------------------------------------
## 1. Indlæs havne (EMODnet MainPorts) og klip til området
## ------------------------------------------------------------------

havne_good_traffic <- st_read(file.path(PATHS$input_pressure,
                                        "havne",
                                        "EMODnet_HA_MainPorts_Traffic_20251210",
                                        "EMODnet_HA_MainPorts_Ports2025_20251210.shp")) %>%
  st_transform(crs = target_crs) %>%
  mutate(PORT_ID = as.character(PORT_ID))   # sikr fælles nøgletype til join

havne_koge <- st_intersection(havne_good_traffic, assessment_area_dissolved)


## ------------------------------------------------------------------
## Indlæs godstrafik og rens
## ------------------------------------------------------------------

Goods_Traffic <- read_delim(file.path(PATHS$input_pressure,
                                      "havne",
                                      "EMODnet_HA_MainPorts_Traffic_20251210",
                                      "Goods_Traffic_20251210.csv"),
                            delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(
    PORT_ID = as.character(PORT_ID),
    T_Thousands_of_tonnes = as.numeric(str_replace(T_Thousands_of_tonnes, ",", ".")),
    YEAR_clean = round(YEAR / 1e15)   
  ) %>%
  filter(YEAR_clean %in% c(2018:2025), # filter for relevante år
         CARGO == "TOTAL") %>%          # Cargo har inward, outward og total, hvoraf total også er opdelt på total inward(for et år) og total outward(for et år)
  group_by(PORT_ID, YEAR_clean) %>%
  summarise(tot_yr_cargo=sum(T_Thousands_of_tonnes,na.rm = TRUE), .groups = "drop")


## ------------------------------------------------------------------
## Godsmængde pr. havn + klassificér industri vs. lystbåd
## ------------------------------------------------------------------

cargo_per_port <- havne_koge %>%
  st_drop_geometry() %>%
  left_join(Goods_Traffic, by = "PORT_ID") %>%
  group_by(PORT_ID) %>%
  summarise(mean_cargo = mean(tot_yr_cargo, na.rm = TRUE), .groups = "drop")




# Lystbådshavne har ingen import/eksport -> mean_cargo bliver NaN (is.na() fanger NaN)
havne_industri_df <- cargo_per_port %>% filter(!is.na(mean_cargo))
havne_lystbaad_df <- cargo_per_port %>% filter(is.na(mean_cargo))

message("Industrihavne: ", nrow(havne_industri_df),
        " | Lystbådshavne: ", nrow(havne_lystbaad_df))


# Rejoin punkt-geometri 
havne_geom <- havne_koge %>% 
  dplyr::select(PORT_ID)

industri_pts <- havne_geom %>% 
  inner_join(havne_industri_df, by = "PORT_ID")
lystbaad_pts <- havne_geom %>% 
  inner_join(havne_lystbaad_df, by = "PORT_ID")


## ==================================================================
## LAG 1 - INDUSTRIHAVNE (vægtet efter godsmængde)
## ==================================================================

# Buffer pr. havn; behold mean_cargo. Log-transformer, da godsmængde er stærkt
# højreskæv (få store havne dominerer) - samme logik som skibstrafik/olie.
industri_buffer <- industri_pts %>%
  st_buffer(dist = buffer_industri) %>%
  mutate(value_raw = log10(mean_cargo+1))

industri_rast <- terra::rasterize(
  terra::vect(industri_buffer),
  grid_raster,
  field      = "value_raw",
  fun        = "max",
  background = NA,
  touches    = TRUE        # <- enhver celle, bufferen RØRER, får værdien
) %>%
  terra::mask(assessment_area_vect)

# Normalisér til 0-1 inden for området
industri_norm <- terra::scale_linear(industri_rast)
names(industri_norm) <- "value"

plot(industri_norm)

terra::writeRaster(industri_norm,
                   file.path(PATHS$output_pressure_tif, "anlaeg", "havne_industri_norm.tif"),
                   overwrite = TRUE)


## ==================================================================
## LAG 2 - LYSTBÅDSHAVNE (fysisk tilstedeværelse -> fraktion pr. celle)
## ==================================================================
# Samme fremgangsmåde som fyrtårne/renseanlæg.

lyst_buffer <- lystbaad_pts %>%
  st_buffer(dist = buffer_lystbaad) %>%
  st_as_sf()

lyst_buffer_grid <- st_intersection(lyst_buffer, grid) %>%
  mutate(area_lyst = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_lyst_id = sum(area_lyst), .groups = "drop")

lyst_gridded <- grid %>%
  left_join(lyst_buffer_grid, by = "id") %>%
  mutate(
    area_lyst_id = tidyr::replace_na(as.numeric(area_lyst_id), 0),
    value = as.numeric(area_lyst_id) / as.numeric(area_grid),
    value = pmin(value, 1)
  ) %>%
  dplyr::select(id, value, geometry) %>%
  st_as_sf()

message("Grid-celler med lystbådshavn: ", sum(lyst_gridded$value > 0))

lyst_rast <- terra::rasterize(
  terra::vect(lyst_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",
  background = NA
)

terra::writeRaster(lyst_rast,
                   file.path(PATHS$output_pressure_tif, "anlaeg", "havne_lystbaad_frak.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## Plotting for bilag (ét kort per lag)
## ------------------------------------------------------------------

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)
viridis_start_color <- viridis_pal()(1)

# Industri 
industri_sf <- industri_norm %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()

map_industri <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = industri_sf, aes(fill = value), color = NA) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_industri

ggsave(plot = map_industri,
       filename = file.path(PATHS$output_pressure_png, "anlaeg", "havne_industri.png"),
       bg = "white", height = 18, width = 18, dpi = 300)



# Lystbåd (buffer-fraktion)
map_lystbaad <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = lystbaad_pts, color = "yellow",size = 3) +
  color_viridis +
  boundary +
  theme_minimal() +
  my_theme +
  north_arrow +
  scale_bar
map_lystbaad

ggsave(plot = map_lystbaad,
       filename = file.path(PATHS$output_pressure_png, "anlaeg", "havne_lystbaad.png"),
       bg = "white", height = 18, width = 18, dpi = 300)





