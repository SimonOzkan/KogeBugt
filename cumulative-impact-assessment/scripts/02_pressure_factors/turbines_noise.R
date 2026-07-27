#---------------------- Offshore vindmølleparker kontinuerlig støj (alle parker) -----------------

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

grid_centroids <- grid %>%
  st_centroid()


## ------------------------------------------------------------------
## 1. Indlæs vindmøllepolygoner (alle parker: aktive, godkendte, planlagte)
## ------------------------------------------------------------------

turbines <- st_read(file.path(PATHS$input_pressure, "/havvind/EMODnet_HA_Energy_WindFarms_20260710/EMODnet_HA_Energy_WindFarms_pg_20260710.shp"))

turbines_proj <- turbines %>%
  st_transform(., crs = target_crs)

turbines_poly_koge <- st_intersection(turbines_proj, assessment_area_dissolved)


## Indlæs faktiske turbinepositioner (punkter), hvor de findes (Lillgrund og Kriegers Flak K2-K3)
turbine_points_raw <- st_read(file.path(PATHS$input_pressure, "/havvind/_ags_wind_offshore_HOLAS3/wind_offshore_HOLAS3.shp")) %>%
  st_transform(., crs = target_crs) %>%
  mutate(NAME = case_when(Name == "Kriegers Flak" ~ "Kriegers Flak K2-K3",
                          .default = Name))


## ------------------------------------------------------------------
## 2. Modelparametre (Tougaard, Hermannsen & Madsen, 2020, JASA 148:2885-2893)
## ------------------------------------------------------------------

C_ref <- 109    # dB re 1 µPa - grand mean ved 100 m, 1 MW, 10 m/s
alpha_generic <- -23.7  # dB/dekade - generisk (tværfarms) afstandshældning
beta  <- 18.5   # dB/dekade - effekt af vindhastighed
gamma <- 13.6   # dB/dekade - effekt af møllestørrelse

wind_speed_ref <- 10  # m/s - reference-vindhastighed, bekræftet retvisende for området

# Lillgrund-specifik kalibrering (Andersson et al., 2011 / Tougaard et al. 2020, Tabel II)
lillgrund_calib <- data.frame(distance = c(160, 400, 1000), Leq = c(102, 92, 86))
fit_lillgrund <- lm(Leq ~ log10(distance), data = lillgrund_calib)
alpha_lillgrund <- unname(coef(fit_lillgrund)[2])
intercept_1m_lillgrund <- unname(coef(fit_lillgrund)[1]) + beta * log10(wind_speed_ref / 12)  # juster 12->10 m/s


## ------------------------------------------------------------------
## 3. Parkoversigt med metadata
## ------------------------------------------------------------------

# NB: turbine_size_MW er samlet effekt / antal møller, hvor begge findes i datasættet.
# For Aflandshage anvendes HOFOR's egne tal (26 møller, 300 MW), IKKE EMODnet's placeholder
# (0 møller, 250 MW), da HOFOR-kilden vurderes mere retvisende.


parks <- tibble::tribble(
  ~NAME,                   ~STATUS,      ~n_turbines, ~turbine_size_MW, ~has_points, ~min_distance,
  "Middelgrunden",         "Production", 20,          2.0,              FALSE,       100,
  "Kriegers Flak K2-K3",   "Production", 72,          8.4,              TRUE,        100,
  "Avedøre Holme",         "Production", 3,           3.6,              FALSE,       100,
  "Lillgrund",             "Production", 48,          2.3,              TRUE,        160,
  # Kriegers Flak II er ét projekt delt i to geografisk adskilte delområder (Nord og Syd),
  # fejlagtigt registreret som to separate polygoner med forskellig STATUS i EMODnet ("Krigers
  # Flak Nord" = Planned, "Kriegers Flak II" = Approved). Begge sættes her til "Approved", da
  # miljøvurderingen (Tabel 1-4) behandler dem som ét samlet projekt. Scenarie 1a/1b (basis,
  # 1.000 MW, 15 MW-møller) er valgt: fordelt 513 MW Nord / 487 MW Syd, jf. footnote-nøglen
  # fra Tabel 1-4 i Krigers Flak Delrapport 2 fra Miljøstyrrelsen (1.770/1.680 MW-fordelingen for 3.450 MW-scenariet, anvendt proportionalt).
  "Krigers Flak Nord",     "Approved",   34,          15.0,             FALSE,       100,
  "Kriegers Flak II",      "Approved",   32,          15.0,             FALSE,       100,
  "Aflandshage",           "Planned",    26,          11.54,            FALSE,       100,  # HOFOR: 26 møller, 300 MW
  "Nordre Flint",          "Planned",    40,          4.0,              FALSE,       100,  # 40 møller, 160 MW
  "Sjollen",               "Planned",    20,          13.5,             FALSE,       100   # ca. 20 møller, 240-300 MW (midtpunkt anvendt)
)


## ------------------------------------------------------------------
## 4. Hjælpefunktioner
## ------------------------------------------------------------------

# Beregn generisk kildeniveau (SL_1m) og afstandshældning (alpha) for en given møllestørrelse
get_park_acoustic_params <- function(park_name, turbine_size_MW) {
  
  if (park_name == "Lillgrund") {
    return(list(SL_1m = intercept_1m_lillgrund, alpha = alpha_lillgrund))
  }
  
  L_100 <- C_ref + gamma * log10(turbine_size_MW / 1) + beta * log10(wind_speed_ref / 10)
  SL_1m <- L_100 - alpha_generic * log10(100)
  
  list(SL_1m = SL_1m, alpha = alpha_generic)
}

# Beregn intensitetsbidrag (µPa²) fra én park til alle gridceller
compute_park_intensity <- function(park_row, turbines_poly_koge, turbine_points_raw, grid_centroids) {
  
  if (is.na(park_row$n_turbines)) {
    message("  → Springer over (manglende data): ", park_row$NAME)
    return(rep(0, nrow(grid_centroids)))
  }
  
  park_polygon <- turbines_poly_koge %>%
    filter(NAME == park_row$NAME)
  
  if (nrow(park_polygon) == 0) {
    message("  → Park ikke fundet i polygonlag: ", park_row$NAME)
    return(rep(0, nrow(grid_centroids)))
  }
  
  # Brug rigtige turbinepositioner hvis de findes, ellers approksimer via polygon
  if (park_row$has_points) {
    turbine_points <- turbine_points_raw %>%
      filter(NAME == park_row$NAME)
    
    if (nrow(turbine_points) == 0) {
      message("  → Punktdata ikke fundet for ", park_row$NAME, ", falder tilbage til polygon-sampling")
      set.seed(1)
      turbine_points <- st_sample(park_polygon, size = park_row$n_turbines, type = "regular") %>%
        st_as_sf() %>%
        st_set_crs(target_crs)
    }
  } else {
    set.seed(1)
    turbine_points <- st_sample(park_polygon, size = park_row$n_turbines, type = "regular") %>%
      st_as_sf() %>%
      st_set_crs(target_crs)
  }
  
  params <- get_park_acoustic_params(park_row$NAME, park_row$turbine_size_MW)
  
  dist_matrix <- st_distance(grid_centroids, turbine_points) %>%
    units::drop_units()
  dist_matrix[dist_matrix < park_row$min_distance] <- park_row$min_distance
  
  L_per_turbine <- params$SL_1m + params$alpha * log10(dist_matrix)
  rowSums(10^(L_per_turbine / 10))  # intensitet (µPa²), ikke dB
}


## ------------------------------------------------------------------
## 5. Beregn støjlag per statuskategori (aktive, godkendte, planlagte hver for sig)
## ------------------------------------------------------------------

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

compute_status_layer <- function(df, status_filter, label) {
  
  message("Beregner støjlag for status: ", label)
  
  parks_subset <- df %>% filter(STATUS == status_filter)
  
  intensity_total <- rep(0, nrow(grid_centroids))
  
  for (i in seq_len(nrow(parks_subset))) {
    park_row <- parks_subset[i, ]
    message(" - ", park_row$NAME)
    intensity_total <- intensity_total + compute_park_intensity(
      park_row, turbines_poly_koge, turbine_points_raw, grid_centroids
    )
  }
  
  L_total <- ifelse(intensity_total > 0, 10 * log10(intensity_total), NA)
  
  noise_grid <- grid %>%
    mutate(value_dB = L_total)
  
  noise_rast_dB <- terra::rasterize(
    terra::vect(noise_grid),
    r_template,
    field      = "value_dB",
    fun        = "max",
    background = NA
  )
  
  
  oise_thresholded <- noise_rast_dB
  noise_thresholded[noise_thresholded < 90] <- NA
  noise_thresholded <- terra::clamp(noise_thresholded, upper = 140, values = TRUE)

  noise_norm <- terra::scale_linear(noise_thresholded)

  names(noise_norm) <- "value"
  
  list(dB = noise_rast_dB, norm = noise_norm)
}

noise_production <- compute_status_layer(parks,"Production", "Aktive parker")
noise_approved   <- compute_status_layer(parks,"Approved", "Godkendte parker")
noise_planned    <- compute_status_layer(parks,"Planned", "Planlagte parker")





## ------------------------------------------------------------------
## 6. Gem rastere
## ------------------------------------------------------------------

terra::writeRaster(noise_production$dB,   file.path(PATHS$output_pressure_tif, "\\turbinestoj_production_dB.tif"), overwrite = TRUE)
terra::writeRaster(noise_production$norm, file.path(PATHS$output_pressure_tif, "\\turbinestoj_production_norm.tif"), overwrite = TRUE)

terra::writeRaster(noise_approved$dB,     file.path(PATHS$output_pressure_tif, "\\turbinestoj_approved_dB.tif"), overwrite = TRUE)
terra::writeRaster(noise_approved$norm,   file.path(PATHS$output_pressure_tif, "\\turbinestoj_approved_norm.tif"), overwrite = TRUE)

terra::writeRaster(noise_planned$dB,      file.path(PATHS$output_pressure_tif, "\\turbinestoj_planned_dB.tif"), overwrite = TRUE)
terra::writeRaster(noise_planned$norm,    file.path(PATHS$output_pressure_tif, "\\turbinestoj_planned_norm.tif"), overwrite = TRUE)


## ------------------------------------------------------------------
## 7. Plotting for bilag (ét kort per statuskategori)
## ------------------------------------------------------------------

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

viridis_start_color <- viridis_pal()(1)

plot_noise_layer <- function(noise_norm_rast, title, filename) {
  
  noise_sf <- noise_norm_rast %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    st_transform(crs = target_crs) %>%
    st_make_valid() %>%
    rename(value = value)
  
  p <- ggplot() +
    geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
    geom_sf(data = map_baltic_sea, fill = viridis_start_color, color = NA, alpha = 1) +
    geom_sf(data = noise_sf, aes(fill = value), color = NA) +
    scale_fill_viridis_c(name = title, limits = c(0, 1)) +
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
  
  ggsave(plot = p,
         filename = file.path(PATHS$output_pressure_png, filename),
         bg = NULL,
         height = 18,
         width = 18,
         dpi = 300)
  
  p
}

plot_noise_layer(noise_production$norm, "Støj (aktive parker)", "\\turbinestoj_production.png")
plot_noise_layer(noise_approved$norm,   "Støj (godkendte parker)", "\\turbinestoj_approved.png")
plot_noise_layer(noise_planned$norm,    "Støj (planlagte parker)", "\\turbinestoj_planned.png")
