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




## ---- normaliseringsanker fra HELE datasættet (alle havne, ikke kun Køge) ----
cargo_all <- havne_good_traffic %>%          
  st_drop_geometry() %>%
  left_join(Goods_Traffic, by = "PORT_ID") %>%
  group_by(PORT_ID) %>%
  summarise(mean_cargo = mean(tot_yr_cargo, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(mean_cargo))

# max af log10(cargo+1) over ALLE industrihavne = normaliserings-max
max_full <- max(log10(cargo_all$mean_cargo + 1), na.rm = TRUE)
message("Normaliseringsanker (log-cargo max, hele datasættet): ", round(max_full, 3))



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


industri_buffer_id <- st_intersection(grid_sf, industri_buffer) %>%
  st_drop_geometry() %>%
  distinct()
  
industri_buffer_gridded <- left_join(grid_sf, industri_buffer_id) %>%
  mutate(value_raw = tidyr::replace_na(value_raw, 0)) %>%
  st_as_sf() %>%
  st_make_valid


industri_rast <- terra::rasterize(
  terra::vect(industri_buffer_gridded),
  grid_raster,
  field      = "value_raw",
  fun        = "max",
  background = NA
) 



# Mask og evt fill huller 
industri_rast_fill <- fill_na(industri_rast, grid_raster, w = 3)

## Normalisér til 0-1 mod HELE datasættet (anker: 0 = ingen havn, max = travleste havn i datasættet)
industri_norm <- terra::clamp(industri_rast / max_full, 0, 1)
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

lyst_buffer_grid <- st_intersection(lyst_buffer, grid_sf) %>%
  mutate(area_lyst = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_lyst_id = sum(as.numeric(area_lyst)), .groups = "drop")

lyst_gridded <- grid_sf %>%
  left_join(lyst_buffer_grid, by = "id") %>%
  mutate(
    area_lyst_id = tidyr::replace_na(as.numeric(area_lyst_id), 0),
    value = as.numeric(area_lyst_id) / as.numeric(area_grid)
  ) %>%
  st_as_sf()

message("Grid-celler med lystbådshavn: ", sum(lyst_gridded$value > 0))

lyst_rast <- terra::rasterize(
  terra::vect(lyst_gridded),
  grid_raster,
  field      = "value",
  fun        = "max",
  background = NA
)


plot(lyst_rast)
terra::writeRaster(lyst_rast,
                   file.path(PATHS$output_pressure_tif, "anlaeg", "havne_lystbaad_frak.tif"),
                   overwrite = TRUE)


## ------------------------------------------------------------------
## Plotting for bilag (ét kort per lag)
## ------------------------------------------------------------------



# Industri 

map_havne_industri <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = industri_pts,
          aes(fill = "Tilstede"), shape = 21, color = "black", size = 2.5) +
  scale_fill_manual(name = NULL,
                    values = c("Tilstede" = "red", "Ikke tilstede" = "#9FBDD8"))+
  boundary +
  theme_minimal() +
  my_theme +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 16))+
  north_arrow +
  scale_bar
map_havne_industri



gem_trim(map_havne_industri,
         file.path(PATHS$output_pressure_png, "anlaeg", "havne_industri_frak.png"),
         dpi = 360, width_cm = 18)

# Lystbåd (buffer-fraktion)
map_havne_lyst <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = assessment_area_dissolved,
          aes(fill = "Ikke tilstede"), color = "black", alpha = 1) +
  geom_sf(data = lystbaad_pts,
          aes(fill = "Tilstede"), shape = 21, color = "black", size = 2.5) +
  scale_fill_manual(name = NULL,
                    values = c("Tilstede" = "red", "Ikke tilstede" = "#9FBDD8"))+
  boundary +
  theme_minimal() +
  my_theme +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 16))+
  north_arrow +
  scale_bar
map_havne_lyst




gem_trim(map_havne_lyst,
         file.path(PATHS$output_pressure_png, "anlaeg", "havne_lystbaad_frak.png"),
         dpi = 360, width_cm = 18)




