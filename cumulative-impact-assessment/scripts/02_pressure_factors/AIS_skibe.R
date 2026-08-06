#---------------------- Skibstrafik (EMODnet Vessel Density) -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs
target_crs <- 25832

# Indlæs undersøgelsesområde
assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)


## ------------------------------------------------------------------
## 1. Udpak alle zip-filer (én gang - spring over hvis allerede udpakket)
## ------------------------------------------------------------------

emodnet_dir <- file.path(PATHS$input_pressure, "/AIS skibe/emodnet")

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
## 2. Definer skibstype-mapper og funktionelle grupper
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
  Industri = c("cargo", "tanker", "passenger", "highspeed", "dredging"),
  Rekreativ    = c("sailing", "pleasure"),
  Fiskeri    = c("fishing"),
  Andre = c("military","andre","service","tug","ukendt")
)

# Hvilke år skal indgå i gennemsnittet - 2024 NOTE: databrist fra juni (tabt satellitdata)
# Skal vi have 2024 med?

years_to_use <- 2019:2024


## ------------------------------------------------------------------
## 3. Hjælpefunktion: indlæs, crop/mask og gennemsnit for én skibstype
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
  
  if (length(selected_files) == 0) {
    warning("Ingen filer matcher de valgte år for: ", folder_name,
            " - tjek regex-mønsteret mod de faktiske filnavne")
    return(NULL)
  }
  
  message("  → ", folder_name, ": ", length(selected_files), " år fundet (",
          paste(sort(file_years[file_years %in% years]), collapse = ", "), ")")
  
  # Indlæs, reprojicer, crop og mask hvert år
  rasters_cropped <- lapply(selected_files, function(f) {
    r <- terra::rast(f) %>%
      terra::project("EPSG:25832")
    terra::crop(r, assessment_area_vect) %>%
      terra::mask(assessment_area_vect)
  })
  
  # Gennemsnit på tværs af årene
  terra::mean(terra::rast(rasters_cropped), na.rm = TRUE)
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

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)

group_rasters <- list()

for (group_name in names(functional_groups)) {
  
  message("Samler gruppe: ", group_name)
  
  types_in_group <- functional_groups[[group_name]]
  rasters_in_group <- ship_type_rasters[types_in_group]
  rasters_in_group <- rasters_in_group[!sapply(rasters_in_group, is.null)]
  
  if (length(rasters_in_group) == 0) {
    warning("Ingen data tilgængelig for gruppe: ", group_name)
    next
  }
  
  # Resample alle til fælles 250m template før sammenlægning
  rasters_resampled <- lapply(rasters_in_group, function(r) {
    terra::resample(r, r_template, method = "bilinear")
  })
  
  # Sum af timer/km2/måned på tværs af skibstyper 
  group_sum <- Reduce(`+`, rasters_resampled)
  
  group_rasters[[group_name]] <- group_sum
}


## ------------------------------------------------------------------
## 6. Log-normaliser hver gruppe til 0-1
## ------------------------------------------------------------------

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
    filename  = file.path(PATHS$output_pressure_tif, paste0("\\shipping_", group_name, "_raw.tif")),
    overwrite = TRUE
  )
  
  terra::writeRaster(
    group_rasters_norm[[group_name]],
    filename  = file.path(PATHS$output_pressure_tif, paste0("\\shipping_", group_name, "_norm.tif")),
    overwrite = TRUE
  )
  
  message("Gemt: shipping_", group_name, "_norm.tif")
}


############### Plotting for bilag ################

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

group_titles <- list(
  Industri = "Skibstrafik (Industri)",
  Rekreativt    = "Skibstrafik (Rekreativt)",
  Fiskeri    = "Skibstrafik (Fiskeri)",
  Andre   = "Skibstrafik (Andre)"
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
    geom_sf(data = map_baltic_sea, fill = "white", color = NA, alpha = 1) +
    geom_sf(data = group_sf, aes(fill = value, color = after_scale(fill)), linewidth = 0.1) +
    scale_fill_viridis_c(name = group_titles[[group_name]], limits = c(0, 1), na.value = "transparent") +
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
  
  ggsave(plot = map_shipping,
         filename = file.path(PATHS$output_pressure_png, paste0("shipping_", group_name, ".png")),
         bg = NULL,
         height = 18,
         width = 18,
         dpi = 300)
  
  message("Gemt plot: shipping_", group_name, ".png")
}
