#---------------------- kommercielle fisk -----------------

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


# BITS (Baltic Sea trawl survey)

BITS <- read_csv("C:/Users/SIO/NIVA/Køge Bugt - General/data/Ecosystem/Fisk/ICES_BITS/Unaggregated trawl and biological information_2026-08-11 10_32_50/Unaggregated trawl and biological information_2026-08-11 10_32_50.csv")

CPUE_per_hr<- read_csv("C:/Users/SIO/NIVA/Køge Bugt - General/data/Ecosystem/Fisk/ICES_BITS/CPUE per length per haul per hour_2026-08-11 13_05_50/CPUE per length per haul per hour_2026-08-11 13_05_50.csv")

CPUE_per_sub_a <-  read_csv("C:/Users/SIO/NIVA/Køge Bugt - General/data/Ecosystem/Fisk/ICES_BITS/CPUE per length per subarea_2026-08-11 13_15_11/CPUE per length per subarea_2026-08-11 13_15_11.csv")

CPUE_Clupea_l_pr_hr <-   read_csv("C:/Users/SIO/NIVA/Køge Bugt - General/data/Ecosystem/Fisk/ICES_BITS/CPUE per length per haul per hour_2026-08-11 14_04_17/CPUE per length per haul per hour_2026-08-11 14_04_17.csv")


ICES_rect <- st_read("C:/Users/SIO/NIVA/Køge Bugt - General/data/Ecosystem/Fisk/ICES_BITS/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")

ICES_area <- st_read()

library(dplyr)

df <- read.csv(
  "C:/Users/SIO/Downloads/1786452738.csv",
  stringsAsFactors = FALSE
) %>%
  select(
    Genus,
    Species,
    Center.Lat,
    Center.Long,
    C.Square.Code,
    Overall.Probability
  )

map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = 4326)

CPUE_Clupea_l_pr_hr_filt <- CPUE_Clupea_l_pr_hr %>%
  mutate(Long = as.numeric(ShootLong),
         Lat = as.numeric(ShootLat)) %>%
  filter(!is.na(Lat),
         !is.na(Long),
         !Lat == -9,
         !Long == -9,
         !Lat >= 59,
         !Lat <= 52,
         !Long <=10,
         !Long >= 15) %>%
  st_as_sf(., coords = c("Long","Lat"), crs = 4326, remove = FALSE) %>%
  st_transform(., crs = target_crs)

Clupea_Oresund <- st_intersection(CPUE_Clupea_l_pr_hr_filt,assessment_area_dissolved) 

# Aggreger til ét punkt per haul per år
Clupea_agg <- Clupea_Oresund %>%
  group_by(Year, HaulNo, Long, Lat) %>%
  summarise(
    total_CPUE = sum(CPUE_number_per_hour, na.rm = TRUE),
    geometry   = first(geometry),
    .groups    = "drop"
  ) %>%
  st_as_sf()

# Plot
ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = "grey60", linewidth = 0.3) +
  geom_sf(data = map_baltic_sea, fill = "lightblue", color = NA) +
  geom_sf(data = Clupea_agg,
          aes(size = total_CPUE, geometry = geometry),
          color = "darkblue", alpha = 0.5) +
  scale_size_continuous(
    name   = "CPUE\n(antal/time)",
    range  = c(1, 10),
    breaks = c(100, 500, 1000, 5000)
  ) +
  facet_wrap(~ Year) +
  coord_sf(
    crs  = target_crs,
    xlim = c(696427, 775958),
    ylim = c(6096053, 6179593)
  ) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank()
  ) +
  labs(title    = "Clupea CPUE per haul",
       subtitle = "Størrelse angiver antal fisk per time")


ggplot()+
  geom_sf(data = map_baltic_sea, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = Clupea_Oresund, aes(geometry = geometry),  color = "black")+
  coord_sf(
    crs  = 4326,
    xlim = c(10, 14),
    ylim = c(54,56),
    clip = "off"                         
  ) 


Clupea_swept <- CPUE_Clupea_l_pr_hr_filt %>%
  select(Long, Lat, HaulLong, HaulLat)
library(sf)
library(ggplot2)
library(dplyr)

# Lav linjer fra ShootLat/ShootLong til HaulLat/HaulLong
trawl_lines <- tst_BITS %>%
  mutate(
    ShootLat  = as.numeric(ShootLat),
    ShootLong = as.numeric(ShootLong)
  ) %>%
  filter(!is.na(ShootLat), !is.na(ShootLong),
         !is.na(HaulLat),  !is.na(HaulLong)) %>%
  rowwise() %>%
  mutate(geometry = st_sfc(
    st_linestring(matrix(
      c(ShootLong, ShootLat, HaulLong, HaulLat),
      ncol = 2, byrow = TRUE
    )),
    crs = 4326
  )) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

# Plot
ggplot() +
  geom_sf(data = map_baltic_sea, fill = "#c3fbb1", color = NA, alpha = 0.5) +
  geom_sf(data = trawl_lines, 
          aes(color = factor(Year)), 
          linewidth = 0.5, alpha = 0.7) +
  scale_color_viridis_d(name = "År") +
  theme_minimal() +
  labs(title = "BITS trawl survey ruter — Østersøen")+ coord_sf(
    crs  = 4326,
    xlim = c(10, 14),
    ylim = c(54,56),
    clip = "off"                         
  ) 


