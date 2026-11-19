source("scripts/00_setup.R")
PATHS <- set_project_paths()
# base_raster         <- rast("C:/Users/EMD/OneDrive - NIVA/TIKTA/study area/europe10km.tif")
# output_csv_folder   <- "C:/Users/EMD/OneDrive - NIVA/TIKTA/NOISE trawling/"
# output_plots_folder <- "C:/Users/EMD/OneDrive - NIVA/TIKTA/NOISE trawling/"

# HELCOM_grid <- st_read(file.path(PATHS$input_assessment_area,"HELCOM","_ags_HELCOM_1x1km_Baltic_Sea_grid","HELCOM_1x1km_Baltic_Sea_grid.shp")) %>%
#   st_transform(,crs = target_crs) %>%
#   st_union()
# 
# 
# 
# 
# ices_SAR <- read_delim("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/lyd_energi/rumbling/EMODnet_HA_Fisheries_FishingIntensity_20260424/ICES_FO_VMS_SAR_20260424.csv", 
#                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
# 
# ices_effort <- read_delim("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/lyd_energi/rumbling/EMODnet_HA_Fisheries_FishingIntensity_20260424/ICES_FO_VMS_Effort_20260424.csv", 
#                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
# 
# sar   <- st_read(file.path(PATHS$input_pressure,"lyd_energi","rumbling","EMODnet_HA_Fisheries_FishingIntensity_20260424","EMODnet_HA_Fisheries_Csquares_20260424.shp")) %>%
#   st_transform(,crs = target_crs) %>%
#   left_join(ices_SAR)
# sar_baltic <- st_intersection(sar, HELCOM_grid) 
# 
# bathy <- rast(file.path(PATHS$input_pressure,"lyd_energi","rumbling","gebco_2026_n70.0_s26.0_w-18.0_e40.0_geotiff.tif"))
# bathy <- terra::project(bathy, terra::vect(HELCOM_grid), method = "bilinear") %>%
#   terra::mask(terra::vect(HELCOM_grid))
# 
# 
# 
# 
# 
# bbox <- st_read(file.path(PATHS$input_assessment_area, "bbox_omraade","koge_bugt_bbox.shp"))
# 
# bbox_v <- bbox %>% st_transform(crs(sar)) %>% vect()

# base_raster         <- rast("C:/Users/EMD/OneDrive - NIVA/TIKTA/study area/europe10km.tif")
# output_csv_folder   <- "C:/Users/EMD/OneDrive - NIVA/TIKTA/NOISE trawling/"
# output_plots_folder <- "C:/Users/EMD/OneDrive - NIVA/TIKTA/NOISE trawling/"

sar   <- terra::rast(file.path(PATHS$input_pressure,"lyd_energi","rumbling","demersal_mobile_laea_10km.tif"))
sar   <- project(sar,paste0("EPSG:",target_crs))

bathy <- rast(file.path(PATHS$input_pressure,"lyd_energi","rumbling","gebco_2026_n70.0_s26.0_w-18.0_e40.0_geotiff.tif"))
bathy <- project(bathy, sar, method = "bilinear")

# bathymetry loss factor
bathy_abs <- abs(bathy)
min_depth <- min(values(bathy_abs), na.rm = TRUE)
max_depth <- max(values(bathy_abs), na.rm = TRUE)


# bathymetry loss factor
# bathy[bathy >= 0] <- NA          # fjern land (behold kun hav, dvs. under havoverfladen)
# plot(bathy)
# bathy_abs <- abs(bathy) # havdybde som positiv (0 ved kysten, ~5680 i dybhavet)
# min_depth <- min(values(bathy_abs), na.rm = TRUE)
# max_depth <- max(values(bathy_abs), na.rm = TRUE)

# Shallow water = 1 (max attenuation)/ deep water = 0.2 (min attenuation)
bathy_loss_factor <- 1 - ((bathy_abs - min_depth) / (max_depth - min_depth))
bathy_loss_factor <- (bathy_loss_factor * 0.8) + 0.2
plot(bathy_loss_factor)


# source level proxy (decibels 0-60dB)
source_level   <- log10(sar + 1)
max_source_val <- max(values(source_level), na.rm = TRUE)

if(max_source_val > 0) {
  source_level <- (source_level / max_source_val) * 60
}

# propagation distance (meters) from trawling activity
trawl_mask <- sar > 0
dist_m     <- distance(trawl_mask) 
plot(dist_m)

dist_m[dist_m == 0] <- 1  # Stop log10(0) issues
# transmission loss (Spreading + Absorption)
transmission_loss <- 15 * log10(dist_m) + (0.00005 * dist_m)
plot(transmission_loss)
plot(bathy_loss_factor)
# total loss = transmission loss + bathymetry loss factor
total_loss       <- transmission_loss * bathy_loss_factor
noise            <- source_level - total_loss
plot(noise)
noise[noise < 0] <- 0
plot(total_loss)
plot(source_level)

par(mfrow = c(3,1))
# Fill any missing gaps natively in terra using adjacent values
noise_filled <- focal(noise, w = 3, fun = "mean", na.policy = "only", na.rm = TRUE)

# Project and snap the final result onto your official study area base_raster grid
noise_final  <- project(noise_filled, grid_raster, method = "bilinear") %>%
  terra::mask(grid_raster)
plot(noise_final)

# normalized scale
min_v <- min(values(noise_filled), na.rm = TRUE)
max_v <- max(values(noise_filled), na.rm = TRUE)
noise_scaled <- (noise_final - min_v) / (max_v - min_v)
names(noise_scaled) <- "Input.of.bottom.trawling.rumbling.noise"

# save all in different formats
writeRaster(noise_scaled, filename = paste0(output_plots_folder, "Continuous_sound_SAR.tif"), overwrite = TRUE)

raster_output <- as.data.frame(noise_scaled, xy = TRUE, na.rm = TRUE)
write.csv(raster_output, paste0(output_csv_folder, "Continuous_sound_SAR.csv"), row.names = FALSE)

turbo_palette <- viridisLite::turbo(100)
png(filename = paste0(output_plots_folder, "Continuous_sound_SAR.png"), width = 700, height = 500)
plot(noise_scaled, col = turbo_palette, main = "SAR sound 1km resampled")
dev.off()

#=================================
# distribution plot
distribution_plot <- ggplot(raster_output, aes(x = Input.of.bottom.trawling.rumbling.noise)) +
  # Histogram to show counts across 50 bins
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "skyblue", color = "white", alpha = 0.7) +
  # Overlay density line to see the shape clearly
  geom_density(color = "darkblue", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Bottom Trawling Rumbling Noise",
    subtitle = "Normalized Scale (0 to 1)",
    x = "Scaled Noise Value",
    y = "Density"
  )

print(distribution_plot)
ggsave(
  filename = paste0(output_plots_folder, "Noise_Data_Distribution.png"), 
  plot = distribution_plot, 
  width = 7, 
  height = 5, 
  dpi = 300
)