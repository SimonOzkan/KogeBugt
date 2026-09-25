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

# Funktionelle grupper (se tidligere diskussion): gruppér efter pres-mekanisme, ikke rå skibstype
functional_groups <- list(
  Industri_andre = c("cargo", "tanker", "passenger", "highspeed", "dredging","military","andre","service","tug","ukendt"),
  Rekreativ    = c("sailing", "pleasure"),
  Fiskeri    = c("fishing")
  #Andre = c("military","andre","service","tug","ukendt")
)

# Hvilke år skal indgå i gennemsnittet - 2024 NOTE: databrist fra juni (tabt satellitdata)
# Skal vi have 2024 med?

years_to_use <- 2019:2023 



## ------------------------------------------------------------------
## Hjælpefunktion: indlæs, projicér til template og gennemsnit for én skibstype
## ------------------------------------------------------------------

load_ship_type_average <- function(folder_name, years) {
  
  folder_path <- file.path(emodnet_dir, folder_name)
  tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  
  if (length(tif_files) == 0) {
    warning("Ingen .tif-filer fundet for: ", folder_name)
    return(NULL)
  }
  
  # Udtræk årstal fra filnavn
  file_years <- stringr::str_extract(basename(tif_files), "20[0-2][0-9]") %>%
    as.numeric()
  
  selected_files <- tif_files[file_years %in% years]
  
  # Projicér til grid
  rasters_aligned <- lapply(selected_files, function(f) {
    terra::rast(f) %>%
      terra::project(grid_raster, method = "bilinear")
  })
  
  # Gennemsnit på tværs af årene/månederne, derefter maskér til assessment area
  avg <- terra::mean(terra::rast(rasters_aligned), na.rm = TRUE)
  terra::mask(avg, assessment_area_vect)
}


## ------------------------------------------------------------------
## 4. Indlæs og gennemsnit alle skibstyper
## ------------------------------------------------------------------

ship_type_rasters <- list()

for (type_name in names(ship_type_folders)) {
  message("Behandler skibstype: ", type_name)
  ship_type_rasters[[type_name]] <- load_ship_type_average(
    ship_type_folders[[type_name]], years_to_use
  )
}


## ------------------------------------------------------------------
## 5. Kombiner til funktionelle grupper
## ------------------------------------------------------------------
# Alle skibstype-rastere ligger allerede på r_template (fra trin 3),
# så vi kan summere direkte uden resample.

group_rasters <- list()

for (group_name in names(functional_groups)) {
  
  message("Samler gruppe: ", group_name)
  
  types_in_group <- functional_groups[[group_name]]
  rasters_in_group <- ship_type_rasters[types_in_group]
  rasters_in_group <- rasters_in_group[!sapply(rasters_in_group, is.null)]
  
  # Sum af timer/km2/måned på tværs af skibstyper
  group_sum <- Reduce(`+`, rasters_in_group)
  
  group_rasters[[group_name]] <- group_sum
}


## ------------------------------------------------------------------
## 6. Log-normaliser hver gruppe til 0-1
## ------------------------------------------------------------------
# Skibstæthed er kraftigt højreskæv (få celler med meget høj trafik i sejlruterne),
# derfor log-transformation FØR den lineære normalisering.

normalize_log <- function(r) {
  r_log <- log1p(r)  # log(1+x), håndterer 0-værdier korrekt
  terra::scale_linear(r_log)
}

group_rasters_norm <- lapply(group_rasters, normalize_log)

## ------------------------------------------------------------------
## 7. Gem rastere
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
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

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
    geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
    geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = NA, alpha = 1) +
    geom_sf(data = group_sf, aes(fill = value, color = after_scale(fill)), linewidth = 0.1) +
    color_viridis+
    boundary+
    theme_minimal()+
    my_theme+
    north_arrow+
    scale_bar
  
  ggsave(plot = map_shipping,
         filename = file.path(PATHS$output_pressure_png,"skibsfart" ,paste0("shipping_", group_name, ".png")),
         bg = NULL,
         height = 18,
         width = 18,
         dpi = 300)
  
  message("Gemt plot: shipping_", group_name, ".png")
}
