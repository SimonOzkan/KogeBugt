#---------------------- Erhvervs og lysbådshavne -----------------

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


# Miljøportalen for Danmark suppleret med HELCOM for Svenske havne

wfs_havne <- "https://wfs2-miljoegis.mim.dk/vp3_2endelig2025/ows"

request <- paste0(
  "WFS:",
  wfs_havne,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=vp3_2endelig2025:vp3_2e2025_havne",
  "&outputFormat=application/json"
)

havne_dk <- st_read(request) %>%
  st_transform(crs = target_crs)

havne_sve <- st_read(file.path(PATHS$input_pressure, "/havne/Harbours_EMODnet_OSM_HOLAS3.shp")) %>%
  filter(Country == "Sweden") 
  
havne_good_traffic <- st_read(file.path(PATHS$input_pressure, "/havne/EMODnet_HA_MainPorts_Traffic_20251210/EMODnet_HA_MainPorts_Ports2025_20251210.shp")) %>%
  st_transform(crs = target_crs)


test <- st_intersection(havne_good_traffic, assessment_area_dissolved)

test_unique <- unique(test$PORT_ID)

Goods_Traffic_20251210 <- read_delim("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/havne/EMODnet_HA_MainPorts_Traffic_20251210/Goods_Traffic_20251210.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(PORT_ID %in% test_unique) %>%
  mutate(YEAR = YEAR / 1e15)



passenger_Traffic_20251210 <- read_delim("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/havne/EMODnet_HA_MainPorts_Traffic_20251210/Passengers_Traffic_20251210.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(PORT_ID %in% test_unique) %>%
  mutate(YEAR = YEAR / 1e15)

goods_filter <- Goods_Traffic_20251210 


malmo <- test %>%
  st_drop_geometry() %>%
  left_join(Goods_Traffic_20251210, by = "PORT_ID") %>%
  filter(PORT_NAME == "Malmö",
         CARGO == "TOTAL",
         !is.na(T_Thousands_of_tonnes)) %>%
  mutate(Ton = as.numeric(sub(",", ".", T_Thousands_of_tonnes, fixed = TRUE)))

ggplot(data = malmo)+
  geom_point(aes(x = YEAR, y = Ton, color =DIRECT ))+
  theme_minimal()




