############################
# Byg assessment-grid for Køge Bugt
# -----------------------------------------------------------------------------
# Formål:
#   1. Indlæs vandområder, dissolve og buffer
#   2. Lav et 250 x 250 m grid der er "snappet" til et rundt 250 m-net
#   3. Behold kun fulde celler (250 x 250 m = 62500 m2)
#   4. Tildel unikt celle-id (bro mellem raster og sf)
#   5. Konverter til raster der matcher grid'et PRÆCIST (samme cellenet)
#   6. Gem både .shp og .tif
###############################################################################
target_crs <- 25832
# Install packages hvis de mangler
required_packages <- c("terra","dplyr","sf","ggplot2", "tidyr", "purrr", "readr","readxl","patchwork", "raster","tidyterra",
                       "lubridate", "stringr", "forcats", "scales","gridExtra","grid","lattice","ggpubr","gt","writexl",
                       "ggspatial","httr","ows4R","viridis", "viridisLite","mgcv","gdistance","magick","terrainr")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
# Load packages
lapply(required_packages, library, character.only = TRUE)


source(here::here("config/paths.R"))
PATHS <- set_project_paths()

res_m      <- 250          # celleopløsning (m)
cell_area  <- res_m * res_m  # fuldt celleareal = 62500 m2



# ── 1. Indlæs, dissolve og buffer ────────────────────────────────────────────
omrade_comb_split <- st_read(file.path(PATHS$input_assessment_area,"shp","koge_split.shp")) %>%
  st_transform(crs = target_crs)



omrade_comb_dissolved <- omrade_comb_split %>%
  st_union() %>%
  st_sf(geometry = .)
# ── 2. Definér et afrundet 250 m-skelet ──────────────────────────────────────
# Snap bounding box UD til nærmeste 250 m, så cellegrænser ligger på "pæne"
# koordinater (multipla af 250). Dette er det fælles fundament for BÅDE
# grid-polygonerne og rasteret, så de matcher præcist.
bb <- st_bbox(omrade_comb_dissolved)

xmin <- floor(bb["xmin"]   / res_m) * res_m
ymin <- floor(bb["ymin"]   / res_m) * res_m
xmax <- ceiling(bb["xmax"] / res_m) * res_m
ymax <- ceiling(bb["ymax"] / res_m) * res_m

# ── 3. Lav 250 x 250 m grid med snappet oprindelse ───────────────────────────
grid_250 <- st_make_grid(
  omrade_comb_dissolved,
  cellsize = c(res_m, res_m),
  offset   = c(xmin, ymin),   # <- afrundet oprindelse => aligned net
  what     = "polygons",
  square   = TRUE
) %>%
  st_sf() %>%
  mutate(id = row_number())   # unikt id FØR klipning (bevares gennem alt)

# ── 4. Klip til området og behold kun fulde celler ───────────────────────────
# st_intersection deler celler i kanten; vi beholder kun dem der er hele.
grid_clip <- st_intersection(grid_250, omrade_comb_dissolved)

grid_full <- grid_clip %>%
  mutate(area = as.numeric(st_area(.))) %>%
  filter(area > cell_area * 0.1)   # Laver celler hvor der er op til 10% af arealet der er hav og sætter det til hav.

# Vigtigt: brug de ORIGINALE (uklippede) firkantede celler for de id'er der
# overlevede areal-filteret. Så er hver celle en perfekt 250 x 250 m firkant,
# ikke en afklippet rest.
grid_sf <- grid_250 %>%
  filter(id %in% grid_full$id)

message("Antal fulde celler i grid: ", nrow(grid_sf))



# ── 5. Konverter til raster der matcher grid'et præcist ───────────────────────
# Byg et tomt raster-skelet med SAMME extent, opløsning og CRS som grid'et.
grid_raster <- terra::rast(
  xmin = xmin, xmax = xmax,
  ymin = ymin, ymax = ymax,
  resolution = res_m,
  crs = paste0("EPSG:", target_crs)
)

# Brænd celle-id ind i rasteret. Fordi grid_sf's celler ligger på nøjagtig
# samme net som rasteret, matcher hver rastercelle én grid-polygon 1:1.
id_raster <- terra::rasterize(terra::vect(grid_sf), grid_raster, field = "id")
names(id_raster) <- "id"

# Lav også et simpelt maske-raster (1 = inden for området, NA = udenfor)
mask_raster <- terra::rasterize(terra::vect(grid_sf), grid_raster, field = 1)
names(mask_raster) <- "assessment_area"

# ── 6. (Valgfrit) Verificér at raster og shp matcher ─────────────────────────
# Antal ikke-NA celler i rasteret skal svare til antal polygoner i grid_sf.
n_raster <- terra::global(!is.na(id_raster), "sum")[1, 1]
message("Antal ikke-NA celler i raster: ", n_raster,
        "  (grid_sf har ", nrow(grid_sf), " celler)")

# ── 7. Gem output ────────────────────────────────────────────────────────────
st_write(grid_sf,
         file.path(PATHS$input_assessment_area,"shp", "250_grid.shp"),
         append = FALSE, quiet = TRUE)

st_write(grid_sf,
         file.path(PATHS$input_assessment_area,"shp", "250_grid.gpkg"),
         append = FALSE, quiet = TRUE)

terra::writeRaster(id_raster,
                   file.path(PATHS$input_assessment_area,"geotif","assessment_area_raster_id.tif"),
                   overwrite = TRUE)

terra::writeRaster(mask_raster,
                   file.path(PATHS$input_assessment_area,"geotif","assessment_area_raster.tif"),
                   overwrite = TRUE)


# ── Kontrolplot ──────────────────────────────────────────────────────────────
plot(mask_raster, main = "Assessment area raster (250 m)")
plot(st_geometry(omrade_comb_dissolved), add = TRUE)
plot(st_geometry(grid_sf), add = TRUE, border = "grey40", lwd = 0.2)



# Konverter til dataframe med celle-centroide koordinater.
# xy = TRUE  -> tilføjer x og y (cellens centrum)
# na.rm = TRUE -> udelader celler uden for området (NA)
df <- terra::as.data.frame(id_raster, xy = TRUE, na.rm = TRUE)

# Omdøb id-kolonnen til area_id og sortér kolonnerne
df <- df %>%
  rename(area_id = id) %>%
  select(x, y, area_id) %>%
  arrange(area_id)

# Kig på de første rækker
head(df)

# Skriv til Excel
write_xlsx(df, file.path(PATHS$input_assessment_area,"geotif", "assessment_area_id.xlsx"))

message("Gemte assessment_area_id.xlsx med ", nrow(df), " celler.")
