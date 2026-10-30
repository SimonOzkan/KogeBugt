###### Klimaforandringer temperatur #############

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()



folder_name_ssp126 <- file.path(PATHS$input_pressure, "klimaforandringer","ssp126")
folder_name_ssp245<- file.path(PATHS$input_pressure, "klimaforandringer","ssp245")



## ------------------------------------------------------------------
## Hjælpefunktion: indlæs rasters for området
## ------------------------------------------------------------------
load_climate_raster <- function(folder, parameter) {
  tif_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  if (length(tif_files) == 0) { warning("Ingen .tif i: ", folder); return(NULL) }
  
  keep     <- stringr::str_detect(basename(tif_files),
                                  stringr::regex(parameter, ignore_case = TRUE))
  selected <- tif_files[keep]
  if (length(selected) == 0) { warning("Ingen filer med '", parameter, "' i: ", folder); return(NULL) }
  
  r <- terra::rast(selected)
  names(r) <- tools::file_path_sans_ext(basename(selected))   # navngiv lag efter fil
  r
}
## ------------------------------------------------------------------
## Indlæs 
## ------------------------------------------------------------------

minmax_tabel <- read.csv(file.path(PATHS$input_pressure, "klimaforandringer","ssp245","delta_minmax_fuldt_omraade.csv"))

pH <- load_climate_raster(folder = folder_name_ssp245, parameter = "pH" )

### kun for de normaliserede plots
rast_to_spat_plot <- function(raster, save_dir){
  
  r_i <- raster
    
    name_plot_save <- names(r_i)
  
  names(r_i)[1] <- "value"
  # make sf 
  sf_r_i <- r_i %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf()%>%
    st_make_valid()
  
  ### plot
  
  map_climate <- ggplot() +
    geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
    geom_sf(data = sf_r_i, aes(fill = value, color = after_scale(fill))) +
    color_viridis+
    boundary+
    theme_minimal()+
    my_theme+
    north_arrow+
    scale_bar
  
  
  gem_trim(map_climate,
           file.path(PATHS$output_pressure_png, "klimaforandringer",save_dir, paste0(name_plot_save,".png")),
           dpi = 360, width_cm = 18)
  
}

# Temp
rast_to_spat_plot(thetao$ssp245_thetao_d5_2035_delta_norm, "temp")

plot(thetao$`ssp245_thetao_bottom_2050_2035-2065`)

rast_to_spat_plot(thetao$ssp245_thetao_d5_2050_delta_norm, "temp")

rast_to_spat_plot(thetao$ssp245_thetao_bottom_2050_delta_norm, "temp")

rast_to_spat_plot(thetao$ssp245_thetao_bottom_2035_delta_norm, "temp")


# pH

rast_to_spat_plot(pH$ssp245_pH_bottom_2050_delta_norm_neg, "pH")

rast_to_spat_plot(pH$ssp245_pH_bottom_2035_delta_norm_neg, "pH")

rast_to_spat_plot(pH$ssp245_pH_d5_2035_delta_norm_neg, "pH")

rast_to_spat_plot(pH$ssp245_pH_d5_2050_delta_norm_neg, "pH")



#### Brugt til at dobbelt tjekke data og evt plot enkelte lag hvis ønsket
thetao_koge_diff50 <- thetao$`ssp245_thetao_bottom_2050_2035-2065` -  thetao$`ssp245_thetao_bottom_baseline_1993-2015`

thetao_max <- minmax_tabel$max[minmax_tabel$lag == "thetao_bottom_2050"]
thetao_min <- minmax_tabel$min[minmax_tabel$lag == "thetao_bottom_2050"]

delta_norm50 <- (thetao_koge_diff50 - thetao_min) / (thetao_max - thetao_min)


thetao$ssp245_thetao_bottom_2050_delta_norm
#
pH_245_tst_sf <- pH_245_tst %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid %>%
  rename("value" = "ssp245_pH_bottom_2050_2035-2065")

ggplot()+
  geom_sf(data = pH_245_tst_sf, aes(fill = value), color = NA) +
  scale_fill_viridis_c(
    name     = NULL,
    na.value = NA,
    guide    = guide_colorbar(
      direction       = "vertical",
      barheight       = unit(0.98, "npc"),   
      barwidth        = unit(0.7, "cm"),
      label.position  = "right",
      ticks           = TRUE,
      ticks.linewidth = 0.5,
      frame.colour    = NA
    ))

map_climate <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) +                     # baggrund: relief
  geom_sf(data = pH_245_tst_sf, aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


rast_to_spat_plot(source = pH_245, save_dir = "pH")
## lav til sf 

