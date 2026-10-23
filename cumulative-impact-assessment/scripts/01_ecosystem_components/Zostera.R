#-------------- Ålegræs (Zostera Marina) --------------------#

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# For den danske del anvendes Sanjina-model, for den svenske del af Øresund suppleres der
# med HELCOM HOLAS 3, da Sanjina-modellen ikke dækker svensk territorium. Begge kilder er i
# praksis binære presence-lag: Sanjina-laget er allerede tærskelværdi-sat af forfatteren
# (probability > 0.5 -> 1), og HELCOM er present/absent. Kriterierne bag hver kilde er
# forskellige (Sanjina: artsdistributionsmodel; HELCOM: filtreret med lys/dybde og substrat)
# og dokumenteres separat i metadata_log.md (ét afsnit per kilde), men da begge i sidste ende
# er binære presence-indikatorer, kan de kombineres direkte til én samlet arealfraktion.

# Indlæs grid og undersøgelsesområde fra de forskellige project paths

assessment_area <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\koge_split.shp"))



# Svensk del af undersøgelsesområdet (bruges til at afgrænse hvor HELCOM-data anvendes)
svensk <- assessment_area %>%
  filter(id == 2)


## ------------------------------------------------------------------
## 1. HELCOM Zostera (present/absent) - kun relevant for svensk område
## ------------------------------------------------------------------

HELCOM_zostera <- terra::rast(file.path(PATHS$input_ecosystem, "\\Zostera\\_ags_EC_22\\EC_22.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

HELCOM_zostera_cropped <- terra::crop(HELCOM_zostera, assessment_area_vect) %>%
  terra::mask(assessment_area_vect)

# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
HELCOM_zostera_sf <- HELCOM_zostera_cropped %>%
  terra::classify(cbind(0, NA)) %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# Afgræns til svensk del af undersøgelsesområdet
HELCOM_zostera_sverige <- st_intersection(HELCOM_zostera_sf, svensk)

## ------------------------------------------------------------------
## 2. Zostera fra Sanjina (kontinuert suitability model, dansk del)
## ------------------------------------------------------------------

zostera_dk_tif <- terra::rast(file.path(PATHS$input_ecosystem, "/Zostera/SDM_DK_Kattegat_extended_SUS_July26.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

zostera_dk_tif_new <-terra::rast(file.path(PATHS$input_ecosystem, "/Zostera/SDM_probab_DK_Kattegat_extended_SUS_aug26.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

zostera_cropped <- terra::crop(zostera_dk_tif, assessment_area_vect) %>%
  terra::mask(assessment_area_vect)

zostera_cropped_new <- terra::crop(zostera_dk_tif_new, assessment_area_vect) %>%
  terra::mask(assessment_area_vect)



# Konverter til sf polygon (kun celler med værdi > 0)
zostera_sf <- zostera_cropped %>%
  terra::classify(cbind(0, NA)) %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# Intersection med assessment area sikrer præcis klipning
zostera_dk <- st_intersection(zostera_sf,assessment_area_dissolved) 



# TODO: Filtrer til >= 70% confidence når 0-1 sandsynlighed er modtaget fra Sanjina
# zostera_final <- zostera_final %>% filter(value > 0.70)

## ------------------------------------------------------------------
## 3. Lysforhold (Kd/dybde) - bruges til at filtrere HELCOM-data
## ------------------------------------------------------------------

kdPar <- terra::rast(file.path(PATHS$input_phys_chem_geo, "/kdpar_all_europe/kdpar_all_europe.tif"))
z <- terra::rast(file.path(PATHS$input_phys_chem_geo, "/_ags_depth_250m/depth_250m.tif"))

# De to lag er meget store, så assessment area konverteres til deres respektive projektion til crop/mask
assessment_area_vect_4326 <- assessment_area_vect %>%
  terra::project(., "EPSG:4326")
assessment_area_vect_3035 <- assessment_area_vect %>%
  terra::project(., "EPSG:3035")

kdPar_masked <- terra::crop(kdPar, assessment_area_vect_4326) %>%
  terra::mask(assessment_area_vect_4326) %>%
  terra::project("EPSG:25832", method = "bilinear")

z_masked <- terra::crop(z, assessment_area_vect_3035) %>%
  terra::mask(assessment_area_vect_3035) %>%
  terra::project("EPSG:25832", method = "bilinear")

# Resample kdPar til z's 250m grid (z er grovere opløsning)
kdPar_resampled <- terra::resample(kdPar_masked, z_masked, method = "bilinear")

# Beregn lystransmission til bund: Iz = exp(-Kd * z) * 100
Iz <- exp(-kdPar_resampled * z_masked) * 100
names(Iz) <- "Iz_pct"

plot(Iz, main = "Lystransmission til bund (%)")

# Filtrer til optimalt ålegræs-range (Iz 20-100%) og max 10 meter dybde
seagrass_optimal <- Iz %>%
  terra::clamp(lower = 20, upper = 100, values = FALSE)

depth_filter <- z_masked <= 10
lz_optimal_shallow <- terra::mask(seagrass_optimal, depth_filter, maskvalue = FALSE)

lz_optimal_shallow_sf <- lz_optimal_shallow %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()


## ------------------------------------------------------------------
## 4. Substratfiltrering (EUSeaMap) - bruges til at filtrere HELCOM-data
## ------------------------------------------------------------------

# Genindlæser habitatlag produceret af habitat_processing.R (se scripts/01_ecosystem_components/)
euseamap_koge <- st_read(file.path(PATHS$input_phys_chem_geo, "/euSeamap/Koge_bugt_seaMap.shp"))

# Fjerner substrattyper som Zostera ikke kan gro på
euseamap_koge_zostera_soft <- euseamap_koge %>%
  filter(!MSFD_BBHT %in% c("Na", "Infralittoral rock and biogenic reef"))


## ------------------------------------------------------------------
## 5. Filtrer HELCOM (svensk) med lysforhold + substrat
## ------------------------------------------------------------------

HELCOM_zostera_sf_lz <- st_intersection(HELCOM_zostera_sverige, lz_optimal_shallow_sf)

HELCOM_zostera_final <- st_intersection(HELCOM_zostera_sf_lz, euseamap_koge_zostera_soft) 


## ------------------------------------------------------------------
## 6. Kombiner Sanjina (dansk) og filtreret HELCOM (svensk) og beregn fraktion
## ------------------------------------------------------------------

# Begge kilder er binære presence-lag (se note øverst i scriptet), så de kan samles og
# behandles som ét presence-lag. "kilde" bevares som attribut til QA/dokumentation, men
# indgår ikke i selve arealberegningen.
zostera_combined <- bind_rows(
  zostera_dk,
  HELCOM_zostera_final) %>%
  st_make_valid()

zostera_union <- zostera_combined %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf()


zostera_intersected <- st_intersection(zostera_union, grid_sf) %>%
  mutate(area_zostera = as.numeric(st_area(.)))



zostera_area <- zostera_intersected %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(area_zost_sum = sum(area_zostera, na.rm = TRUE), .groups = "drop")
  
zostera_frac <- left_join(grid_sf, zostera_area) %>%
  mutate(value = area_zost_sum / area_grid,
         value= ifelse(is.na(value), 0, value)) 
  

## ------------------------------------------------------------------
## 8. Rasterize og gem .tif
## ------------------------------------------------------------------


zostera_complete_raster <- terra::rasterize(
  terra::vect(zostera_frac),
  grid_raster,
  field      = "value",
  fun        = "max",      # der bør ikke være overlap efter union, derfor tages max
  background = 0
)

# Masked
zostera_complete_raster<- terra::mask(zostera_complete_raster, grid_raster)
plot(zostera_complete_raster, main = "Zostera arealfraktion (kombineret Sanjina + HELCOM)")

terra::writeRaster(
  zostera_complete_raster,
  filename  = file.path(PATHS$output_ecosystem_tif, "vigtige_habs","Zostera.tif"),
  overwrite = TRUE
)


############### Plotting for bilag ################

zostera_complete_raster_sf <- zostera_complete_raster %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

map_zostera <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = zostera_complete_raster_sf,
          aes(fill = value),color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_zostera

ggsave(plot = map_zostera,
       filename = file.path(PATHS$output_ecosystem_png, "vigtige_habs","Zostera.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)

