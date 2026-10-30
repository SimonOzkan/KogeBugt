#### Klimaforandringer ##########

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

## ---- parametre ----


# ##### ssp126 #############
# temp5  <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp126\\thetao\\depth 5"
# tempbot <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp126\\thetao\\depth_bottom"
# 
# 
# ph5   <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp126\\ph\\depth 5"
# phbot <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp126\\ph\\depth_bottom"


##### ssp245 ############
temp5   <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp245\\thetao\\depth 5"
tempbot <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp245\\thetao\\depth_bottom"


ph5   <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp245\\ph\\depth 5"
phbot <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\NorthSea\\ssp245\\ph\\depth_bottom"


out_dir <- "C:\\Users\\SIO\\NIVA\\Køge Bugt - General\\data\\Pressure\\klimaforandringer"

# undersøgelsesområde: enten en polygon-fil ELLER en extent

# alternativ: grid_raster <- ext(xmin, xmax, ymin, ymax)

# ## ---- funktion: cellevist gennemsnit over en årsperiode, klippet til grid_raster ----
# periode_gennemsnit <- function(mappe, aar_start, aar_slut, grid_raster, out_fil) {
#   f <- list.files(mappe, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
#   
#   # træk årstal ud af filnavnet: thetao_mean_1993-01_stats.tif -> 1993
#   aar   <- as.integer(sub(".*_(\\d{4})-\\d{2}_.*", "\\1", basename(f)))
#   f_sub <- f[!is.na(aar) & aar >= aar_start & aar <= aar_slut]
#   
#   if (length(f_sub) == 0) stop("Ingen filer i ", aar_start, "-", aar_slut)
#   message(sprintf("%d-%d: %d månedsfiler", aar_start, aar_slut, length(f_sub)))
#   
#   # stak alle månedsfiler -> gennemsnit pr. celle
#   r_mean <- mean(rast(f_sub), na.rm = TRUE)
#   
#   r_max <- terra::global(r_mean, "max", na.rm = TRUE)[1,1]
#   r_min <- terra::global(r_mean, "min", na.rm = TRUE)[1,1]
#   
#   # klip til undersøgelsesområde
#   r_out <- terra::project(r_mean, grid_raster, method = "bilinear")
#   
#   r_out <- fill_na(r_out, grid_raster, w =3)
#   
#   writeRaster(r_out, out_fil, overwrite = TRUE)
#   message("Gemt: ", out_fil)
#   r_out
# }

periode_gennemsnit <- function(mappe, aar_start, aar_slut, grid_raster, out_fil, navn = NULL) {
  f     <- list.files(mappe, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  aar   <- as.integer(sub(".*_(\\d{4})-\\d{2}_.*", "\\1", basename(f)))
  f_sub <- f[!is.na(aar) & aar >= aar_start & aar <= aar_slut]
  if (length(f_sub) == 0) stop("Ingen filer i ", aar_start, "-", aar_slut)
  message(sprintf("%d-%d: %d månedsfiler", aar_start, aar_slut, length(f_sub)))
  
  r_mean <- mean(rast(f_sub), na.rm = TRUE)                 # FULD udstrækning
  
  rng   <- terra::global(r_mean, "range", na.rm = TRUE)     # absolut min/max, HELE området
  r_min <- rng[1, 1]; r_max <- rng[1, 2]
  
  r_out <- terra::project(r_mean, grid_raster, method = "bilinear")
  r_out <- fill_na(r_out, grid_raster, w = 3)
  writeRaster(r_out, out_fil, overwrite = TRUE)
  message("Gemt: ", out_fil)
  
  if (is.null(navn)) navn <- tools::file_path_sans_ext(basename(out_fil))
  
  list(
    navn      = navn,
    koge      = r_out,      # klippet til Køge (til delta + plot)
    fuld_mean = r_mean,     # FULD udstrækning (til deltaens range)
    min_abs   = r_min,
    max_abs   = r_max
  )
}

# res <- list()
# 
# ## ---- kør for depth 5 temperatur ---- for ssp126
# res$baseline <-  periode_gennemsnit(temp5, 1993, 2015, grid_raster, file.path(out_dir,"ssp126", "ssp126_thetao_d5_baseline_1993-2015.tif"))
# res$fut2035  <- periode_gennemsnit(temp5, 2020, 2050, grid_raster, file.path(out_dir,"ssp126", "ssp126_thetao_d5_2035_2020-2050.tif"))
# res$fut2050  <- periode_gennemsnit(temp5, 2035, 2065, grid_raster, file.path(out_dir, "ssp126","ssp126_thetao_d5_2050_2035-2065.tif"))
# 
# 
# ## ---- kør for bot depth temperatur ----
# res$baseline <- periode_gennemsnit(tempbot, 1993, 2015, grid_raster, file.path(out_dir,"ssp126", "ssp126_thetao_bottom_baseline_1993-2015.tif"))
# res$fut2035  <- periode_gennemsnit(tempbot, 2020, 2050, grid_raster, file.path(out_dir, "ssp126","ssp126_thetao_bottom_2035_2020-2050.tif"))
# res$fut2050  <- periode_gennemsnit(tempbot, 2035, 2065, grid_raster, file.path(out_dir, "ssp126","ssp126_thetao_bottom_2050_2035-2065.tif"))
# 
# 
# 
# ## ---- kør for depth 5 pH ----
# res$baseline <- periode_gennemsnit(ph5, 1993, 2015, grid_raster, file.path(out_dir,"ssp126", "ssp126_pH_d5_baseline_1993-2015.tif"))
# res$fut2035  <- periode_gennemsnit(ph5, 2020, 2050, grid_raster, file.path(out_dir,"ssp126", "ssp126_pH_d5_2035_2020-2050.tif"))
# res$fut2050  <- periode_gennemsnit(ph5, 2035, 2065, grid_raster, file.path(out_dir,"ssp126", "ssp126_pH_d5_2050_2035-2065.tif"))
# 
# plot(baseline)
# ## ---- kør for bot depth pH ----
# res$baseline <- periode_gennemsnit(phbot, 1993, 2015, grid_raster, file.path(out_dir,"ssp126", "ssp126_pH_bottom_baseline_1993-2015.tif"))
# res$fut2035  <- periode_gennemsnit(phbot, 2020, 2050, grid_raster, file.path(out_dir,"ssp126", "ssp126_pH_bottom_2035_2020-2050.tif"))
# res$fut2050  <- periode_gennemsnit(phbot, 2035, 2065, grid_raster, file.path(out_dir, "ssp126","ssp126_pH_bottom_2050_2035-2065.tif"))
# 

## lille hjælper: PRÆCIS de 4 manuelle trin, pakket én gang
##  sign =  1  -> delta = fremtid - baseline   (opvarmning / rå pH-fald)
##  sign = -1  -> delta = baseline - fremtid   (forsuring som positiv påvirkning)
lav_delta_norm <- function(baseline, fremtid, sign = 1) {
  delta_full <- sign * (fremtid$fuld_mean - baseline$fuld_mean)   # FULD udstrækning
  rng <- terra::global(delta_full, "range", na.rm = TRUE)
  mn  <- rng[1, 1]; mx <- rng[1, 2]
  
  delta_koge <- sign * (fremtid$koge - baseline$koge)             # Køge
  norm <- terra::clamp((delta_koge - mn) / (mx - mn), 0, 1)
  
  list(norm = norm, min = mn, max = mx)
}

deltas <- list()   # her samler vi alle delta-resultater (til tabellen til sidst)
scen   <- "ssp245"


## ================= TEMP depth 5 =================
res <- list()
res$baseline <- periode_gennemsnit(temp5, 1993, 2015, grid_raster, file.path(out_dir, scen, "ssp245_thetao_d5_baseline_1993-2015.tif"))
res$fut2035  <- periode_gennemsnit(temp5, 2020, 2050, grid_raster, file.path(out_dir, scen, "ssp245_thetao_d5_2035_2020-2050.tif"))
res$fut2050  <- periode_gennemsnit(temp5, 2035, 2065, grid_raster, file.path(out_dir, scen, "ssp245_thetao_d5_2050_2035-2065.tif"))

deltas$thetao_d5_2035 <- lav_delta_norm(res$baseline, res$fut2035, sign = 1)   # opvarmning
deltas$thetao_d5_2050 <- lav_delta_norm(res$baseline, res$fut2050, sign = 1)
plot(deltas$thetao_d5_2035$norm)
plot(deltas$thetao_d5_2050$norm)

writeRaster(deltas$thetao_d5_2035$norm, file.path(out_dir, scen, "ssp245_thetao_d5_2035_delta_norm.tif"), overwrite = TRUE)
writeRaster(deltas$thetao_d5_2050$norm, file.path(out_dir, scen, "ssp245_thetao_d5_2050_delta_norm.tif"), overwrite = TRUE)


## ================= TEMP bottom =================
res <- list()
res$baseline <- periode_gennemsnit(tempbot, 1993, 2015, grid_raster, file.path(out_dir, scen, "ssp245_thetao_bottom_baseline_1993-2015.tif"))
res$fut2035  <- periode_gennemsnit(tempbot, 2020, 2050, grid_raster, file.path(out_dir, scen, "ssp245_thetao_bottom_2035_2020-2050.tif"))
res$fut2050  <- periode_gennemsnit(tempbot, 2035, 2065, grid_raster, file.path(out_dir, scen, "ssp245_thetao_bottom_2050_2035-2065.tif"))

deltas$thetao_bottom_2035 <- lav_delta_norm(res$baseline, res$fut2035, sign = 1)
deltas$thetao_bottom_2050 <- lav_delta_norm(res$baseline, res$fut2050, sign = 1)

writeRaster(deltas$thetao_bottom_2035$norm, file.path(out_dir, scen, "ssp245_thetao_bottom_2035_delta_norm.tif"), overwrite = TRUE)
writeRaster(deltas$thetao_bottom_2050$norm, file.path(out_dir, scen, "ssp245_thetao_bottom_2050_delta_norm.tif"), overwrite = TRUE)


## ================= pH depth 5  (både + og −) =================
res <- list()
res$baseline <- periode_gennemsnit(ph5, 1993, 2015, grid_raster, file.path(out_dir, scen, "ssp245_pH_d5_baseline_1993-2015.tif"))
res$fut2035  <- periode_gennemsnit(ph5, 2020, 2050, grid_raster, file.path(out_dir, scen, "ssp245_pH_d5_2035_2020-2050.tif"))
res$fut2050  <- periode_gennemsnit(ph5, 2035, 2065, grid_raster, file.path(out_dir, scen, "ssp245_pH_d5_2050_2035-2065.tif"))

# "+" : forsuring som påvirkning (pH-fald -> høj værdi)
deltas$pH_d5_2035_pos <- lav_delta_norm(res$baseline, res$fut2035, sign = -1)
deltas$pH_d5_2050_pos <- lav_delta_norm(res$baseline, res$fut2050, sign = -1)
# "−" : rå signeret delta (pH-fald -> lav værdi)
deltas$pH_d5_2035_neg <- lav_delta_norm(res$baseline, res$fut2035, sign =  1)
deltas$pH_d5_2050_neg <- lav_delta_norm(res$baseline, res$fut2050, sign =  1)

writeRaster(deltas$pH_d5_2050_pos$norm, file.path(out_dir, scen, "ssp245_pH_d5_2050_delta_norm_pos.tif"), overwrite = TRUE)
writeRaster(deltas$pH_d5_2050_neg$norm, file.path(out_dir, scen, "ssp245_pH_d5_2050_delta_norm_neg.tif"), overwrite = TRUE)


writeRaster(deltas$pH_d5_2035_pos$norm, file.path(out_dir, scen, "ssp245_pH_d5_2035_delta_norm_pos.tif"), overwrite = TRUE)
writeRaster(deltas$pH_d5_2035_neg$norm, file.path(out_dir, scen, "ssp245_pH_d5_2035_delta_norm_neg.tif"), overwrite = TRUE)

# (tilsvarende for 2035 hvis du vil gemme begge)


## ================= pH bottom  (både + og −) =================
res <- list()
res$baseline <- periode_gennemsnit(phbot, 1993, 2015, grid_raster, file.path(out_dir, scen, "ssp245_pH_bottom_baseline_1993-2015.tif"))
res$fut2035  <- periode_gennemsnit(phbot, 2020, 2050, grid_raster, file.path(out_dir, scen, "ssp245_pH_bottom_2035_2020-2050.tif"))
res$fut2050  <- periode_gennemsnit(phbot, 2035, 2065, grid_raster, file.path(out_dir, scen, "ssp245_pH_bottom_2050_2035-2065.tif"))

deltas$pH_bottom_2035_pos <- lav_delta_norm(res$baseline, res$fut2035, sign = -1)
deltas$pH_bottom_2050_pos <- lav_delta_norm(res$baseline, res$fut2050, sign = -1)
deltas$pH_bottom_2035_neg <- lav_delta_norm(res$baseline, res$fut2035, sign =  1)
deltas$pH_bottom_2050_neg <- lav_delta_norm(res$baseline, res$fut2050, sign =  1)

writeRaster(deltas$pH_bottom_2050_pos$norm, file.path(out_dir, scen, "ssp245_pH_bottom_2050_delta_norm_pos.tif"), overwrite = TRUE)
writeRaster(deltas$pH_bottom_2050_neg$norm, file.path(out_dir, scen, "ssp245_pH_bottom_2050_delta_norm_neg.tif"), overwrite = TRUE)


writeRaster(deltas$pH_bottom_2035_pos$norm, file.path(out_dir, scen, "ssp245_pH_bottom_2035_delta_norm_pos.tif"), overwrite = TRUE)
writeRaster(deltas$pH_bottom_2035_neg$norm, file.path(out_dir, scen, "ssp245_pH_bottom_2035_delta_norm_neg.tif"), overwrite = TRUE)


## ================= SAML min/max i én tabel =================
minmax_tabel <- purrr::imap_dfr(deltas, ~ tibble::tibble(lag = .y, min = .x$min, max = .x$max))
print(minmax_tabel)

write.csv(minmax_tabel, file.path(out_dir, scen, "delta_minmax_fuldt_omraade.csv"), row.names = FALSE)

