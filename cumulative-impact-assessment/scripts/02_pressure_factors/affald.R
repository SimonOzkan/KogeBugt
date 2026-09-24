#-------------------------------- Marint affald --------------------------------#
source("scripts/00_setup.R")
PATHS <- set_project_paths()

BITS_affald <- read_csv(file.path(PATHS$input_pressure, "affald","Litter Assessment Output_2026-09-23 11_13_03","Litter Assessment Output_2026-09-23 11_13_03.csv"))

BITS_affald <-BITS_affald %>%
  filter(MSFDArea == "Baltic Sea")

BITS_sf <- BITS_affald %>%
  rename("lat" = "ShootLat",
         "lon" = "ShootLong") %>%
  st_as_sf(., coords = c("lon","lat"), crs = 4326, remove  = FALSE) %>%
  st_transform(, crs = target_crs)

BITS_koge <- st_intersection(BITS_sf, grid) %>%
  filter(!LT_Weight < 0)

map_tst <- ggplot(BITS_koge, aes(geometry = geometry)) +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(aes(size = LT_Weight), alpha = 0.5) +
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar
map_tst


