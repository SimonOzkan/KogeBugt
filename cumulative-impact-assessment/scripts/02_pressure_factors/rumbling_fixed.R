


HELCOM_grid <- st_read(file.path(PATHS$input_assessment_area,"HELCOM","_ags_HELCOM_1x1km_Baltic_Sea_grid","HELCOM_1x1km_Baltic_Sea_grid.shp")) %>%
  st_transform(,crs = target_crs) %>%
  st_union()

ices_SAR <- read_delim("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/lyd_energi/rumbling/EMODnet_HA_Fisheries_FishingIntensity_20260424/ICES_FO_VMS_SAR_20260424.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

# 1) komma->punktum, til numerisk, og ÉN værdi pr. c-square
sar_1 <- ices_SAR %>%
  mutate(srfc_sar = as.numeric(gsub(",", ".", srfc_sr))) %>%
  group_by(c_squar) %>%
  summarise(srfc_sar = mean(srfc_sar, na.rm = TRUE), .groups = "drop")


# ices_effort <- read_delim("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/lyd_energi/rumbling/EMODnet_HA_Fisheries_FishingIntensity_20260424/ICES_FO_VMS_Effort_20260424.csv", 
#                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

sar   <- st_read(file.path(PATHS$input_pressure,"lyd_energi","rumbling","EMODnet_HA_Fisheries_FishingIntensity_20260424","EMODnet_HA_Fisheries_Csquares_20260424.shp")) %>%
  st_transform(,crs = target_crs) %>%
  left_join(sar_1, by = "c_squar")
sar_baltic <- st_intersection(sar, HELCOM_grid) 

bathy <- rast(file.path(PATHS$input_pressure,"lyd_energi","rumbling","gebco_2026_n70.0_s26.0_w-18.0_e40.0_geotiff.tif"))
bathy <- terra::project(bathy, terra::vect(HELCOM_grid), method = "bilinear") %>%
  terra::mask(terra::vect(HELCOM_grid))




## ---- 0. Baltic-maske + arbejds-raster ---------------------------------------
helcom_v <- terra::vect(HELCOM_grid)
res_work <- 500                                              # 1 km (sæt 2000 hvis for langsomt)
r0 <- terra::rast(helcom_v, resolution = res_work)

## ---- 1. SAR (bundtrawl) -> raster -------------------------------------------
sar_field <- "srfc_sar"
sar_r <- terra::rasterize(terra::vect(sar_baltic), r0, field = sar_field,
                          fun = "max", background = NA) %>% terra::mask(helcom_v)
plot(sar_r)

## ---- 2. Kildeniveau (dB-proxy) ved trawl-celler -----------------------------
sl       <- log10(sar_r + 1)                                  # SAR er skævt -> log
sl_max   <- terra::global(sl, "max", na.rm = TRUE)[[1]]
sl_min   <- terra::global(sl, "min", na.rm = TRUE)[[1]]
source_r <- (sl / sl_max) * 60                                # 0-60 dB over Østersøen
src_level <- terra::ifel(sar_r > 0, source_r, NA)             # kun hvor der trawles
names(src_level) <- "level"
plot(src_level)
## ---- 3. Afstand + NÆRMESTE kildes niveau (ægte propagation) ------------------
dist_m <- terra::distance(src_level)                          # m til nærmeste trawl-celle
dist_m <- terra::ifel(dist_m == 0, res_work/2, dist_m)        # undgå log10(0)

src_pts    <- terra::as.points(src_level)                     # kilder som punkter m. niveau

#3) voronoi(src_pts, bnd=helcom_v) + rasterize(..., field="level") — Voronoi (Thiessen-polygoner) deler rummet op i felter,
#hvor hvert felt er det område der er tættest på netop det kildepunkt. bnd=helcom_v klipper felterne til Østersø-grænsen. rasterize(..., field="level")
#maler så hvert felt med dets kildes niveau. Resultatet: hver celle får niveauet af sin nærmeste trawl-kilde.

near_level <- terra::rasterize(terra::voronoi(src_pts, bnd = helcom_v), r0,
                               field = "level") %>%            # nærmeste kildes niveau overalt 
  terra::mask(helcom_v)

## ---- 4. Dybde-afhængigt transmissionstab (land fjernet, Baltic-normaliseret) -
bathy <- terra::ifel(bathy >= 0, NA, -bathy)              # kun hav; dybde positiv
depth <- terra::project(bathy, r0, method = "bilinear") %>%
  terra::mask(helcom_v)
plot(depth)
dmin <- terra::global(depth, "min", na.rm = TRUE)[[1]]
dmax <- terra::global(depth, "max", na.rm = TRUE)[[1]]
bathy_loss <- (1 - (depth - dmin) / (dmax - dmin)) * 0.8 + 0.2   # lavt=1 (mest tab), dybt=0.2

TL <- (15 * log10(dist_m) + 0.00005 * dist_m) * bathy_loss

## ---- 5. Modtaget støj -------------------------------------------------------
noise <- near_level - TL
noise <- terra::clamp(noise, lower = 0, values = TRUE) %>%    # gulv 0
  terra::mask(helcom_v)

plot(noise)
## ---- 6. Snap til Køge-grid + maske ------------------------------------------
noise_koge <- terra::project(noise, grid_raster, method = "bilinear") %>%
  terra::mask(grid_raster)
plot(noise_koge)


max_noise_baltic <- terra::global(noise, "max", na.rm = TRUE)[1,1]

noise_scaled_koge <- (noise_koge/max_noise_baltic) ### min = 0

noise_scaled_koge_fill <- fill_na(noise_scaled_koge, grid_raster, w = 3)

######## save raster

terra::writeRaster(
  noise_scaled_koge_fill,
  filename  = file.path(PATHS$output_pressure_tif, "lyd_energi","rumbling.tif"),
  overwrite = TRUE
)


# ### Plotting bilag
noise_sf <- noise_scaled_koge_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_noise_norm <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = noise_sf, aes(fill = level), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_noise_norm

gem_trim(map_noise_norm,
         file.path(PATHS$output_pressure_png, "lyd_energi", "rumbling_norm.png"),
         dpi = 360, width_cm = 18)



# Claude tjek

#Check 1 (afstand): 250 til 632 km, og kortet viser nu rigtig struktur — lav afstand (mørk) i SØ hvor der trawles, voksende afstand (lyst bånd op til ~30 km) ud mod den inderste bugt/Øresund-siden i NV. Det er præcis det afstandsfelt der manglede før. min = 250 = din res_work/2 (res_work=500), så kilde-cellerne er korrekt sat til halv-celle-afstand. ✓
# Check 2 (de tre paneler) — det afgørende bevis: kig på “effektivt tab” (højre panel). Det er ~0 (mørkt) i SØ tæt på trawlingen og højt (gult, op til ~25) ude i NV langt fra trawling. Tabet vokser med afstanden fra kilderne — det er netop dét, en propagation skal gøre, og det du ikke havde før (hvor tabet var ~0 overalt). Og “modtaget støj” (midten) viser at NV-bugten går mørk, mens SØ forbliver lys. Propagationen virker nu. ✓
# Check 4 (tal): 0 til 52,8, median 16,2 — fornuftigt. Og bemærk: max er nu 52,8, ikke ~22 som før. Det er en kilde i dybt vand: ved kilden er tabet 15·log10(250)·bathy ≈ 36·0,2 ≈ 7 dB, så 60−7 ≈ 53. At max’en er høj netop i dybt vand betyder at bathymetri-faktoren nu faktisk bider — dybe kilder beholder mere, lave kilder taber mere. Endnu et tegn på at maskineriet er aktivt. ✓
# Check 3 (transekt): ja, find et hotspot automatisk og træk linjen derfra ud i et mørkt område:
#1)
plot(crop(dist_m, bbox_v))      # skal være 0 ved trawl og vokse udad — IKKE ≈0 overalt
global(dist_m, c("min","max"), na.rm = TRUE)

#2)
par(mfrow = c(1,3))
plot(crop(near_level, bbox_v), main = "kildeniveau")
plot(crop(noise, bbox_v),      main = "modtaget støj")
plot(crop(near_level - noise, bbox_v), main = "effektivt tab")   # ~0 ved kilder, vokser udad

#4)
global(noise, c("min","max","mean"), na.rm = TRUE)
quantile(values(noise), c(0,.5,.9,.99,1), na.rm = TRUE)
hist(values(noise))


#3)
# start = højeste støj-celle (hotspot) inden for Køge
nk   <- crop(noise, bbox_v)
hot  <- xyFromCell(nk, which.max(values(nk)))     # hotspot-koordinat
x1 <- hot[1]; y1 <- hot[2]
x2 <- 700000; y2 <- 6165000                       # et mørkt NV-punkt (aflæs fra kortet)

line <- vect(rbind(c(x1,y1), c(x2,y2)), type = "lines", crs = crs(noise))
pts  <- spatSample(line, size = 100, method = "regular")   # 100 punkter langs linjen
val  <- terra::extract(noise, pts)[,2]
plot(val, type = "l", xlab = "langs transekt (hotspot → NV)", ylab = "støj")

global(noise, c("min","max","mean"), na.rm = TRUE)
quantile(values(noise), c(0,.5,.9,.99,1), na.rm = TRUE)
hist(values(noise))

