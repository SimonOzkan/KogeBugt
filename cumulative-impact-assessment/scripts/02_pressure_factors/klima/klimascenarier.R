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

# Har efterfølgende ændret så min = 0 og de værdier bruges derfor ikke
minmax_tabel <- read.csv(file.path(PATHS$input_pressure, "klimaforandringer","ssp245","delta_minmax_fuldt_omraade.csv"))

pH <- load_climate_raster(folder = folder_name_ssp245, parameter = "pH" )
thetao <- load_climate_raster(folder = folder_name_ssp245, parameter = "thetao")



# temp
temp_bot_2050 <- thetao$"ssp245_thetao_bottom_2050_2035-2065"
temp_bot_baseline<- thetao$"ssp245_thetao_bottom_baseline_1993-2015"

temp_d5_2050 <- thetao$"ssp245_thetao_d5_2050_2035-2065"
temp_d5_baseline<- thetao$"ssp245_thetao_d5_baseline_1993-2015"



# pH
pH_bot_2050 <- pH$"ssp245_pH_bottom_2050_2035-2065"
pH_bot_baseline<- pH$"ssp245_pH_bottom_baseline_1993-2015"

pH_d5_2050 <- pH$"ssp245_pH_d5_2050_2035-2065"
pH_d5_baseline<- pH$"ssp245_pH_d5_baseline_1993-2015"



hent_max <- function(lag_navn) {
  minmax_tabel %>% filter(lag == lag_navn) %>% pull(max)
}

lav_delta_norm <- function(baseline, fremtid, mx, sign = 1) {
  terra::clamp(sign * (fremtid - baseline) / mx, 0, 1)
}

temp_bot_norm <- lav_delta_norm(temp_bot_baseline, temp_bot_2050, hent_max("thetao_bottom_2050"))
temp_d5_norm  <- lav_delta_norm(temp_d5_baseline,  temp_d5_2050,  hent_max("thetao_d5_2050"))


temp_mn <- (temp_bot_norm+temp_d5_norm) /2
plot(temp_mn)

ph_bot_norm <- lav_delta_norm(pH_bot_baseline, pH_bot_2050, hent_max("pH_bottom_2050_pos"), sign = -1)
ph_d5_norm  <- lav_delta_norm(pH_d5_baseline,  pH_d5_2050,  hent_max("pH_d5_2050_pos"),     sign = -1)

ph_mn <- (ph_bot_norm+ph_d5_norm) /2
plot(ph_mn)

rast_save <- function(raster,climate) {
  
  terra::writeRaster(raster,
                     file.path(PATHS$output_pressure_tif, "klimaforandringer",paste0(climate,".tif")),
                     overwrite = TRUE)
}
rast_save(raster = ph_mn,climate = "pH_norm_2050")
rast_save(raster = ph_mn,climate = "pH_ba")


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
