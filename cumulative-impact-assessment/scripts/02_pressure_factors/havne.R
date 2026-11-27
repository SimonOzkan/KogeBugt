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
# Effektdistance
max_dist_industri <- 10.1*10^3
max_dist_lystbaad  <- 5.6*10^3

## ------------------------------------------------------------------
## 1. Indlæs havne (EMODnet MainPorts) og klip til området
## ------------------------------------------------------------------

baltic_sea <-  st_read(file.path(PATHS$input_assessment_area,
                                 "HELCOM",
                                 "HELCOM_waterbodies",
                                 "HELCOM_subbasin_with_coastal_WFD_waterbodies_or_watertypes_2022_eutro.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_union() %>%
  st_buffer(dist = 1000)



havne_good_traffic <- st_read(file.path(PATHS$input_pressure,
                                        "havne",
                                        "EMODnet_HA_MainPorts_Traffic_20251210",
                                        "EMODnet_HA_MainPorts_Ports2025_20251210.shp")) %>%
  st_transform(crs = target_crs) %>%
  mutate(PORT_ID = as.character(PORT_ID))   # sikr fælles nøgletype til join
havne_geom <- havne_good_traffic %>%
  dplyr::select(PORT_ID, geometry, PORT_NAME)


###### Havne fra harbour maps (hentet manuelt for hver havn)
lystbaad <- read_csv(file.path(PATHS$input_pressure,
                   "havne",
                   "havne_oresund_koge_bugt.csv"))


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



## ---- normaliseringsanker fra HELE datasættet (alle havne, ikke kun Køge) ----
cargo_all_baltic <- havne_good_traffic %>%    
  st_filter(baltic_sea) %>% # baserer min og max på havne i Østersøen
  st_drop_geometry() %>%
  left_join(Goods_Traffic, by = "PORT_ID") %>%
  group_by(PORT_ID) %>%
  summarise(mean_cargo = mean(tot_yr_cargo, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(mean_cargo))

# max af log10(cargo+1) over ALLE industrihavne = normaliserings-max
max_full <- max(cargo_all_baltic$mean_cargo, na.rm = TRUE)
max_full_lg <- max(log10(cargo_all_baltic$mean_cargo + 1), na.rm = TRUE)


# Lystbådshavne har ingen import/eksport -> mean_cargo bliver NaN (is.na() fanger NaN)
industri_pts <- cargo_all_baltic %>% 
  left_join(havne_geom, by = "PORT_ID") %>%
  st_as_sf()

## ==================================================================
## LAG 1 - INDUSTRIHAVNE (vægtet efter godsmængde)
## ==================================================================

# Buffer pr. havn; behold mean_cargo. Log-transformer, da godsmængde er stærkt
# højreskæv (få store havne dominerer) - samme logik som skibstrafik/olie.
industri_norm <- industri_pts %>%
  mutate(value_norm = mean_cargo / max_full,
         value_lg_norm = log10(mean_cargo+1) / max_full_lg) %>%
  st_intersection(grid_sf) %>%
  st_drop_geometry()


industri_gridded <- grid_sf %>%
  left_join(industri_norm) %>%
  mutate(value_norm = tidyr::replace_na(value_norm, 0),
         value_lg_norm = tidyr::replace_na(value_lg_norm, 0))

industri_rast <- terra::rasterize(
  terra::vect(industri_gridded),
  grid_raster,
  field      = "value_norm",
  fun        = "max",
  background = NA
) 
industri_rast_lg <- terra::rasterize(
  terra::vect(industri_gridded),
  grid_raster,
  field      = "value_lg_norm",
  fun        = "max",
  background = NA
) 


names(industri_rast_lg) <- "value"

plot(industri_rast_lg)

industri_rast_lg_decay <- decay_focal(industri_rast_lg, max_dist_industri)
plot(industri_rast_lg_decay)


terra::writeRaster(industri_rast_lg_decay,
                   file.path(PATHS$output_pressure_tif, "iei", "havne_industri_norm_decay.tif"),
                   overwrite = TRUE)

## ==================================================================
## LAG 2 - LYSTBÅDSHAVNE fysisk tilstedeværelse -> 1
## ==================================================================
# Samme fremgangsmåde som fyrtårne/renseanlæg.

lystbaad_sf <- lystbaad %>%
  st_as_sf(.,coords = c("x" = "lon","y"="lat"), crs = 4326) %>%
  st_transform(crs = target_crs)


lyst_rast <- terra::rasterize(
  terra::vect(lystbaad_sf),
  tmpl_ext,
  field      = 1,
  touches    = TRUE,
  background = 0)


plot(lyst_rast)

lyst_rast_decay <- decay_focal(lyst_rast, max_dist_lystbaad)

plot(lyst_rast_decay) 

lyst_rast_decay_masked <- lyst_rast_decay %>%
  terra::crop(grid_raster) %>%
  terra::mask(grid_raster)

plot(lyst_rast_decay_masked)

terra::writeRaster(lyst_rast_decay_masked,
                   file.path(PATHS$output_pressure_tif, "iei", "havne_lystbaad_decay.tif"),
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


lystbaad_pts <- lystbaad %>%
  st_as_sf(.,coords = c("x" = "lon","y"="lat"), crs = 4326) %>%
  st_transform(crs = target_crs) 
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




