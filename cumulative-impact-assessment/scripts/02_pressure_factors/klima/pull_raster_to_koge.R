#### Klimaforandringer ##########

library(terra)

## ---- parametre ----
temp5   <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\BalticSea\\ssp126\\thetao\\depth 5"
tempbot <- "D:\\PRA\\ETC BE\\Multiple pressures\\Multiple_pressures\\Helle paper\\geotiffs\\geotiffs\\BalticSea\\ssp126\\thetao\\depth_bottom"
out_dir <- "C:\\Users\\SIO\\NIVA\\Køge Bugt - General\\data\\Pressure\\klimaforandringer"

# undersøgelsesområde: enten en polygon-fil ELLER en extent
aoi <- assessment_area_vect # SpatVector (shp/gpkg)
# alternativ: aoi <- ext(xmin, xmax, ymin, ymax)

## ---- funktion: cellevist gennemsnit over en årsperiode, klippet til aoi ----
periode_gennemsnit <- function(mappe, aar_start, aar_slut, aoi, out_fil) {
  f <- list.files(mappe, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  
  # træk årstal ud af filnavnet: thetao_mean_1993-01_stats.tif -> 1993
  aar   <- as.integer(sub(".*_(\\d{4})-\\d{2}_.*", "\\1", basename(f)))
  f_sub <- f[!is.na(aar) & aar >= aar_start & aar <= aar_slut]
  
  if (length(f_sub) == 0) stop("Ingen filer i ", aar_start, "-", aar_slut)
  message(sprintf("%d-%d: %d månedsfiler", aar_start, aar_slut, length(f_sub)))
  
  # stak alle månedsfiler -> gennemsnit pr. celle
  r_mean <- mean(rast(f_sub), na.rm = TRUE)
  
  # klip til undersøgelsesområde
  if (inherits(aoi, "SpatExtent")) {
    r_out <- crop(r_mean, aoi)
  } else {
    aoi   <- project(aoi, crs(r_mean))        # samme CRS som rasteren
    r_out <- mask(crop(r_mean, aoi), aoi)
  }
  
  writeRaster(r_out, out_fil, overwrite = TRUE)
  message("Gemt: ", out_fil)
  r_out
}

## ---- kør for depth 5 ----
baseline <- periode_gennemsnit(temp5, 1993, 2015, aoi, file.path(out_dir, "thetao_d5_baseline_1993-2015.tif"))
fut2035  <- periode_gennemsnit(temp5, 2020, 2050, aoi, file.path(out_dir, "thetao_d5_2035_2020-2050.tif"))
fut2050  <- periode_gennemsnit(temp5, 2035, 2065, aoi, file.path(out_dir, "thetao_d5_2050_2035-2065.tif"))



