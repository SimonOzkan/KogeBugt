#----------------------------------- Kystbeskyttelse / Anlæg på havet  ----------------------- ##

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# Sæt crs
target_crs <- 25832

# Indlæs grid og undersøgelsesområde fra de forskellige project paths

grid <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\250_grid_minus_land.shp")) %>%
  st_transform(., crs = target_crs)

grid_area <- grid %>%
  mutate(area_grid = st_area(.)) %>%
  st_drop_geometry(.)

assessment_area_dissolved <- st_read(file.path(PATHS$input_assessment_area, "\\shp\\assessment_area_dissolved.shp")) %>%
  st_transform(., crs = target_crs)

assessment_area_vect <- terra::vect(assessment_area_dissolved)


## Kystbeskyttelsesanlæg 
wfs_url_kystbeskyt <- "https://gisportal.mst.dk/server/services/ekstern/KDI_Kystbeskyttelsesanlaeg/MapServer/WFSServer?SERVICE=WFS&REQUEST=GetCapabilities"

request_kystbeskyt <- paste0(
  wfs_url_kystbeskyt,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=esri:Kystbeskyttelsesanlaeg",
  "&outputFormat=GEOJSON"
)

kystbeskyt <- st_read(request_kystbeskyt) %>%
  st_transform(crs = target_crs)

#Filtrer for hård substrat og sedimentbarriere
kystbeskyt_koge <- st_intersection(kystbeskyt, assessment_area_dissolved) %>%
  filter(Typenr %in% c("Bølgebryder", "Ydermole", "Høfde", "T høfde", "Ledeværk",
                       "Skråningsbeskyttelse", "Skråningsbeskyttelse uindm", "Stenrække", "Dige",
                       "Diverse konstruktioner", "Bygværk"))


## Bemærk der er et lag kaldet sandfodring og Terrænændring (dækker det samme). Hør om det skal med. 

# EMODnet for Sverige
swe_kystbeskyt <- st_read(file.path(PATHS$input_pressure, "/kystbeskyt/_ags_CoastalDefence_lines_HOLAS3/CoastalDefence_lines_HOLAS3.shp")) %>%
  st_transform(crs = target_crs) %>%
  st_intersection(assessment_area_dissolved)

