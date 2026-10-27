###### Iltsvind ##################


### Dette dokument udregner antal dage med iltsvind (under 4mg/l) i Køge Bugt. 
# Det starter med at Copernicus Oxygen koncentration hentes ved CLI commands (tutorials online)
# Der hentes daglige værdier fra august til oktober for 2023, 2024 og 2025 for hele Østersøen for normalisering
# Eftersom der er en kæmpe hypoxia zone i Østersøen vil der i løbet af scriptet laves et dybde lag filtrering, så det kun er dybder til 40m der kigges på
# I alt er der 276 dage (august (31*3)+ sept(30 * 3) + okt(31*3)) 
# Så i princippet vil det maksimale antal dage med hypoxia være 276, men eftersom dette ikke er sammenligningligt med Køge Bugt foretages der også en percentil (0.99) filtrering
# Dette blev gjort da selv efter dybde filtrering var der områder med 276 dage i Hypoxia
# Det giver en maks værdi på 98 dage med iltsvind som max sættes efter i normaliseringen


# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

path_cm <- "C:\\Users\\SIO\\Downloads\\copernicusmarine-extra.exe"

system2(path_cm, "--version")

system2(path_cm, c("describe", "--contains", "bal_bgc"))


out_dir <- file.path(PATHS$input_ecosystem, "iltsvind","daily")

# --- download whole-Baltic o2b (dry-run FØRST for at se størrelsen!) ---
# brug bb_baltic["xmin"] osv. i -x/-X/-y/-Y, Aug-Okt, 2023-2025 -> o2b_baltic_YYYY.nc
baltic_mask <- depth                                   # alle værdier = 1, EPSG:3035
bb_baltic   <- sf::st_bbox(terra::project(baltic_mask, "EPSG:4326"))   # -> download-bbox


####### ---------- Dette henter de 276 raster lag for bund ilt koncentration i mmol/m3 ############# Filerne er gemt i "out_dir" som .nc
# for (y in 2023:2025) {
#   status <- system2(path_cm, c(
#     "subset",
#     "-i", "cmems_mod_bal_bgc_my_P1D-m",
#     "-v", "o2b",
#     "-x", "9.0", "-X", "30.5", "-y", "53.0", "-Y", "66.0",
#     "-t", sprintf("%d-08-01", y), "-T", sprintf("%d-10-31", y),
#     "-o", shQuote(out_dir), "-f", sprintf("o2b_baltic_%d.nc", y)
#   ))
#   if (status != 0) warning("Download fejlede for ", y)
# }

# Dybde for hele Østersøen, der anvendes til maskering
depth_baltic <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_depth_250m\\depth_250m.tif")) 

#


# --- iltsvindsdage pr. celle, hele Baltic (native grid) ---
files_bs <- list.files(out_dir, "o2b_baltic.*\\.nc$", full.names = TRUE)
o2b_bs   <- rast(lapply(files_bs, \(f) rast(f, subds = "o2b")))
o2b_bs   <- classify(o2b_bs, cbind(-999, NA)) * 0.032      # mmol/m3 -> mg/L

plot(o2b_bs[[1]])     # kortet for dag 1 (hele Østersøen)

crs(o2b_bs) <- "EPSG:4326"
dage_bs  <- sum(o2b_bs < 4, na.rm = TRUE)                  # antal dage < 4 mg/L



# dybde over HELE Baltic, projiceret til dage_bs' grid (lon/lat)
depth_bs <- terra::project(depth_baltic, dage_bs, method = "bilinear")
plot(depth_bs)   # tjek at den dækker hele Baltic (ikke bare NA)

# lavvandet maske: kun celler <= 40 m (Køges egen maksdybde ~47 m)
dage_shallow <- terra::ifel(depth_bs <= 40, dage_bs, NA)


## Information omkring oxygen lagene 
plot(dage_shallow)

plot(dage_shallow == 276)

terra::global(dage_shallow == 276, "sum", na.rm = TRUE)[1,1]

terra::global(!is.na(dage_shallow), "sum", na.rm = TRUE)[1,1]

terra::global(dage_shallow, "range", na.rm = TRUE)


quantile(values(x = dage_shallow), c(0.9, 0.95, 0.99,1), na.rm = TRUE)
###########

####   Maskering af koncentration lagene til Køge Bugt og dererefter fill_na der laver 3x3 window gennemsnit for at udfylde huller

koge_raster_df <- terra::project(o2b_bs, grid_raster, method = "bilinear") 

koge_raster_df_fill_na <- fill_na(koge_raster_df, grid_raster, w = 3)


### Viser oxygen niveauernes percentiler
terra::global(koge_raster_df_fill_na, "range", na.rm = TRUE)
quantile(values(koge_raster_df_fill_na), c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE)
## dage under grænseværdien for ilt i Køge Bugt
dage_bs_koge  <- sum(koge_raster_df_fill_na < 4, na.rm = TRUE)                  # antal dage < 4 mg/L

plot(dage_bs_koge)

# Viser antallet af dage hvor grænseværdien overskrides
terra::global(dage_bs_koge, "range", na.rm = TRUE)
quantile(values(dage_bs_koge), c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE)

Dmin <- 0
Dmax <- quantile(values(dage_shallow), 0.99, na.rm = TRUE)[[1]]   # anvender 98 dage som er 99% for hele dybde maskeret oxygen lag


# A) presfaktor-retning (flest dage = 1) - standard for en presfaktor i CIA
iltsvind <- terra::clamp((dage_bs_koge - Dmin) / (Dmax - Dmin), 0, 1)
names(iltsvind) <- "value"

plot(iltsvind)

terra::writeRaster(iltsvind,
                   file.path(PATHS$output_ecosystem_tif,"iltsvind", "iltsvind.tif"), overwrite = TRUE)
# B) Økosystem retning (flest dage = 0, færrest 1)

iltsvind_B <- terra::clamp(1- (dage_bs_koge - Dmin) / (Dmax - Dmin), 0, 1)
plot(iltsvind_B)

names(iltsvind_B) <- "value"



terra::writeRaster(iltsvind_B,
                   file.path(PATHS$output_ecosystem_tif,"iltsvind", "iltsvind.tif"), overwrite = TRUE)

iltsvind_sf <- iltsvind_B %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()


map_iltsvind <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = iltsvind_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_iltsvind

ggsave(plot = map_iltsvind,
       filename = file.path(PATHS$output_ecosystem_png,"iltsvind","klfa.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)





