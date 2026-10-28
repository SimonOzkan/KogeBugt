#---------------------- Skibstrafik (EMODnet Vessel Density) -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

## ------------------------------------------------------------------
## 1. Udpak alle zip-filer (én gang - spring over hvis allerede udpakket)
## ------------------------------------------------------------------

emodnet_dir <- file.path(PATHS$input_pressure, "AIS skibe","emodnet")

zip_files <- list.files(emodnet_dir, pattern = "\\.zip$", full.names = TRUE, ignore.case = TRUE)

for (zf in zip_files) {
  target_folder <- file.path(emodnet_dir, tools::file_path_sans_ext(basename(zf)))
  
  if (!dir.exists(target_folder)) {
    message("Udpakker: ", basename(zf))
    dir.create(target_folder, recursive = TRUE)
    utils::unzip(zf, exdir = target_folder)
  } else {
    message("Springer over (allerede udpakket): ", basename(zf))
  }
}


## ------------------------------------------------------------------
## Definer skibstype-mapper og funktionelle grupper
## ------------------------------------------------------------------


ship_type_folders <- list(
  cargo      = "CARGO",
  tanker     = "TANKER",
  passenger  = "PASSANGER",
  highspeed  = "HIGHSPEED",
  sailing    = "SAILING",
  pleasure   = "PLEASURE",
  fishing    = "FISHING",
  dredging   = "DREDGINGOURUNDERWATER",
  military = "MILI",
  andre = "OTHER",
  service = "SERVICE",
  tug = "TUG",
  ukendt = "UNKNOWN"
)

# Funktionelle grupper
functional_groups <- list(
  Industri_andre = c("cargo", "tanker", "passenger", "highspeed", "dredging","military","andre","service","tug","ukendt","sailing", "pleasure"),
  #Rekreativ    = c("sailing", "pleasure"),
  Fiskeri    = c("fishing")
  #Andre = c("military","andre","service","tug","ukendt")
)

# Hvilke år skal indgå i gennemsnittet - 2024 NOTE: databrist fra juni (tabt satellitdata)
# Skal vi have 2024 med?

years_to_use <- c(2018,2019,2020,2021,2022,2023)



## ------------------------------------------------------------------
## Hjælpefunktion: indlæs, projicér til template og gennemsnit for én skibstype
## ------------------------------------------------------------------
load_ship_type_average <- function(folder_name, years) {
  folder_path <- file.path(emodnet_dir, folder_name)
  tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  if (length(tif_files) == 0) { warning("Ingen .tif for: ", folder_name); return(NULL) }
  
  file_years <- as.numeric(stringr::str_extract(basename(tif_files), "20[0-2][0-9]"))
  selected   <- tif_files[file_years %in% years]
  if (length(selected) == 0) { warning("Ingen valgte år for: ", folder_name); return(NULL) }
  
  terra::mean(terra::rast(selected), na.rm = TRUE)   # <- FULD udstrækning, INGEN projektion
}
## ------------------------------------------------------------------
## Indlæs og gennemsnit alle skibstyper
## ------------------------------------------------------------------

ship_type_rasters <- list()

for (type_name in names(ship_type_folders)) {
  message("Behandler skibstype: ", type_name)
  ship_type_rasters[[type_name]] <- load_ship_type_average(
    ship_type_folders[[type_name]], years_to_use
  )
}
ship_type_rasters$fishing

## ------------------------------------------------------------------
## Kombiner til funktionelle grupper
## ------------------------------------------------------------------

# Summer


group_rasters <- list()


for (group_name in names(functional_groups)) {
  
  message("Samler gruppe: ", group_name)
  
  types_in_group <- functional_groups[[group_name]]
  rasters_in_group <- ship_type_rasters[types_in_group]
  rasters_in_group <- rasters_in_group[!sapply(rasters_in_group, is.null)]
  
  # Sum af timer/km2/år på tværs af skibstyper
  group_sum <- Reduce(`+`, rasters_in_group)
  
  group_rasters[[group_name]] <- group_sum
}

group_rasters$Industri_andre
## ------------------------------------------------------------------
## Log-normaliser hver gruppe til 0-1
## ------------------------------------------------------------------
# Skibstæthed er kraftigt højreskæv (få celler med meget høj trafik i sejlruterne),
# derfor log-transformation FØR den lineære normalisering.

normalize_log_fuldt <- function(r_full, grid_raster) {
  log_full <- log10(r_full + 1)                               # log på HELE området
  
  rng <- terra::global(log_full, "range", na.rm = TRUE)       # min/max for hele EMODnet-området
  mn  <- rng[1, 1]; mx <- rng[1, 2]
  
  log_koge <- terra::project(log_full, grid_raster, method = "bilinear")  # klip/regrid til projektgrid
  log_koge_fill <- fill_na(log_koge, grid_raster, w = 3)
  
  terra::clamp((log_koge_fill - mn) / (mx - mn), 0, 1)             # normalisér med FULDT min/max
}

group_rasters_norm <- lapply(group_rasters, normalize_log_fuldt, grid_raster)
plot(group_rasters_norm$Industri_andre)

## ------------------------------------------------------------------
## Gem rastere
## ------------------------------------------------------------------

for (group_name in names(group_rasters_norm)) {
  
  terra::writeRaster(
    group_rasters[[group_name]],
    filename  = file.path(PATHS$output_pressure_tif,"skibsfart", paste0("\\shipping_", group_name, "_raw.tif")),
    overwrite = TRUE
  )
  
  terra::writeRaster(
    group_rasters_norm[[group_name]],
    filename  = file.path(PATHS$output_pressure_tif, "skibsfart",paste0("\\shipping_", group_name, "_norm.tif")),
    overwrite = TRUE
  )
  
}


############### Plotting for bilag ################


group_titles <- list(
  Industri_andre = "Skibstrafik (Industri)",
  Rekreativt    = "Skibstrafik (Rekreativt)",
  Fiskeri    = "Skibstrafik (Fiskeri)"
  #Andre   = "Skibstrafik (Andre)"
)

for (group_name in names(group_rasters_norm)) {
  
  group_sf <- group_rasters_norm[[group_name]] %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    st_transform(crs = target_crs) %>%
    st_make_valid()
  
  names(group_sf)[1] <- "value"  # sikrer konsistent kolonnenavn til plotting
  
  map_shipping <- ggplot() +
    geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
    geom_sf(data = group_sf, aes(fill = value, color = after_scale(fill)), linewidth = 0.1) +
    color_viridis+
    boundary+
    theme_minimal()+
    my_theme+
    north_arrow+
    scale_bar
  

  gem_trim(map_shipping,
           file.path(PATHS$output_pressure_png, "skibsfart", paste0("shipping_", group_name, ".png")),
           dpi = 360, width_cm = 18)

}

