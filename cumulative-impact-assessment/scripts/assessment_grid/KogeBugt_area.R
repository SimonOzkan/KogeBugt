# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")
PATHS <- set_project_paths()

syd_koge <- st_read(file.path(PATHS$input_assessment_area,
                              "maps/assessment_area/Denmark/syd_koge_afgransning_union.shp"))

nord_koge <- st_read(file.path(PATHS$input_assessment_area,
                               "maps/assessment_area/Denmark/nord_koge_afgransning_union.shp")) %>%
  rename("area" = "Area")

koge_bugt <- st_read(file.path(PATHS$input_assessment_area,
                               "maps/assessment_area/Denmark/kogebugt_vandomrade_union.shp"))

oeresund <- st_read(file.path(PATHS$input_assessment_area,
                              "maps/Europe/Oersund.shp"))   # Øresundsforbindelsen

maritime_boundaries <- st_read(file.path(PATHS$input_assessment_area,
                                         "maps/world_maritime_boundaries/World_Maritime_Boundaries.shp")) %>%
  st_transform(crs = target_crs)

map_eu <- st_read(file.path(PATHS$input_assessment_area,
                            "maps/Europe/Europe_merged3035.shp"))

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area,
                                    "maps/BalticSeaMap/iho.shp")) %>%
  st_transform(crs = target_crs)

swe_koge_bugt <- st_read(file.path(PATHS$input_assessment_area, "Maps/Assessment_area/Sverige/swe_union_east.shp")) %>%
  mutate(id = 2, area = "Sverige, øst") 

nord_dk_koge <- st_read(file.path(PATHS$input_assessment_area, "Maps/Assessment_area/Denmark/nord_koge_dk.shp"))%>%
  mutate(id = 4, area = "nord køge DK")



praestoe_url <- "https://wfs2-miljoegis.mim.dk/vp3endelig2022/ows"

req_praestoe <- paste0(
  praestoe_url,
  "?service=WFS",
  "&version=2.0.0",
  "&request=GetFeature",
  "&typeNames=vp3endelig2022:vp3e2022_marin_samlet",
  "&outputFormat=application/json"
)

praestoe <-st_read(req_præstø) %>%
  filter(mst_id ==47)

# Opret en dummy variabel til linjetypen i maritime_boundaries
maritime_boundaries$dummy_linetype <- "Maritime grænser"


# Sørg for at alle lag har samme CRS
syd_koge       <- st_transform(syd_koge, target_crs)
nord_koge      <- st_transform(nord_koge, target_crs)
koge_bugt      <- st_transform(koge_bugt, target_crs)
oeresund       <- st_transform(oeresund, target_crs)
map_eu         <- st_transform(map_eu, target_crs)
map_baltic_sea <- st_transform(map_baltic_sea, target_crs)
praestoe <- st_transform(praestoe, target_crs)%>%
  transmute(id = 5, area = "Præstø Fjord")   # transmute beholder geometrien,
swe_koge_bugt <- st_transform(swe_koge_bugt, target_crs)
nord_dk_koge <- st_transform(nord_dk_koge, target_crs)
# Definer bbox ud fra koge_bugt så kortet zoomer til området
bbox <- st_bbox(koge_bugt)
expand <- 50000  # 50 km margin
xlim <- c(bbox["xmin"] - expand, bbox["xmax"] + expand)
ylim <- c(bbox["ymin"] - expand, bbox["ymax"] + expand)

ggplot() +
  # Baggrundslag
  geom_sf(data = map_eu,         fill = "grey85",      color = "grey60",    linewidth = 0.3) +
  geom_sf(data = map_baltic_sea, fill = "lightblue",   color = "steelblue", linewidth = 0.3, alpha = 0.5) +
  
  # Køge Bugt vandområde
  geom_sf(data = koge_bugt,      fill = "lightcyan",   color = "darkcyan",  linewidth = 0.5, alpha = 0.6) +
  
  # Vurderingsområder
  geom_sf(data = syd_koge,       fill = "tomato",      color = "red3",      linewidth = 0.7, alpha = 0.5) +
  geom_sf(data = nord_koge,      fill = "gold",        color = "goldenrod", linewidth = 0.7, alpha = 0.5) +
  
  # Præstø 
  geom_sf(data = præstø,      fill = "lightgreen",        color = "lightgreen", linewidth = 0.7, alpha = 0.5) +
  
  # Øresundsbroen
  geom_sf(data = oeresund,       color = "purple",     linewidth = 1.2) +
  
  # Maritime grænser
  geom_sf(data = maritime_boundaries, color = "navy", linewidth = 0.8, linetype = "dashed") +
  
  # Zoom til området
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  
  # Manuel legende via annotate (da lagene ikke deler én aes)
  labs(
    title    = "Køge Bugt – Oversigt over lag",
    subtitle = "Vurderingsområder, vandområde og grænser",
    caption  = "CRS: EPSG 3035"
  ) +
  
  # Falsk legende med fill + color
  scale_fill_identity() +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid    = element_line(color = "grey90"),
    axis.text     = element_text(size = 8)
  ) +
  annotate("rect", xmin = -Inf, xmax = -Inf, ymin = -Inf, ymax = -Inf, fill = NA) + # dummy
  guides() +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering())

# ── 1. Beregn maritime grænser inden for hvert område ─────────────────────────
boundary_syd  <- st_intersection(maritime_boundaries, syd_koge)
boundary_nord <- st_intersection(maritime_boundaries, nord_koge)

# ── 2. Split områderne langs de maritime grænser ──────────────────────────────
syd_koge_split  <- st_difference(syd_koge,  st_union(boundary_syd))
nord_koge_split <- st_difference(nord_koge, st_union(boundary_nord))

# ── 3. Udtræk polygoner og tildel id ──────────────────────────────────────────
syd_koge_split <- syd_koge_split %>%
  st_collection_extract("POLYGON") %>%
  mutate(id = row_number())

nord_koge_split <- nord_koge_split %>%
  st_collection_extract("POLYGON") %>%
  mutate(id = row_number())

# Tjek hvilke id'er der findes (kør disse og inspicer før du fortsætter)
ggplot() + geom_sf(data = syd_koge_split,  aes(fill = as.factor(id))) + theme_minimal()
ggplot() + geom_sf(data = nord_koge_split, aes(fill = as.factor(id))) + theme_minimal()

# ── 4. Filtrer og navngiv de relevante dele ───────────────────────────────────
# NB: Tjek id-værdierne fra plot ovenfor og ret filtrene hvis nødvendigt
syd_koge_dk <- syd_koge_split %>%
  filter(id == 1) %>%
  mutate(id = 3, area = "syd køge DK")

nord_koge_dk <- syd_koge_split %>%                  # Læses fra ekstern fil nedenfor
  filter(id == 23) %>%
  mutate(id = 5, area = "nord køge DK")

koge_bugt <- koge_bugt %>%
  mutate(id = 1, area = "Køge Bugt")

# ── 6. Kombinér alle områder ───────────────────────────────────────────────────
omrade_comb_split <- bind_rows(
  syd_koge_dk,
  swe_koge_bugt,
  nord_dk_koge,
  koge_bugt,
  praestoe_area          # <- Præstø med
) %>%
  mutate(id = as.factor(id)) %>%
  dplyr::select(id, area, geometry)

### omrade_comb_split er det der anvendes til assessment area / undersøgelsesområde
# Denne "opløses" og gemmes og fra denne laves raster og shp laget med 250x250 m


st_write(omrade_comb_split,file.path(PATHS$input_assessment_area, "shp","koge_split.shp" ),
         append = FALSE)

map_vandomrader <- ggplot() +
  geom_sf(data = map_baltic_sea, fill = "lightblue") +
  geom_sf(data = omrade_comb_split, aes(fill = id)) +
  geom_sf(data = maritime_boundaries, aes(linetype = dummy_linetype), color = "black", linewidth = 1.4) + 
  geom_sf(data = oeresund, color = "black") +
  scale_fill_manual(
    values = c("#4A90C4", "#E07B6A", "#D4B96A", "#E8924A", "#6AB06A"),
    labels = c("Køge Bugt",
               "Tilstødende vandområde, øst (Sverige)",
               "Tilstødende vandområde, syd (Danmark)",
               "Tilstødende vandområde, nord (Danmark)",
               "Præstø Fjord"),
    name = "Vandområder",
    guide = guide_legend(order = 1)
  ) + 
  scale_linetype_manual(
    values = c("Maritime grænser" = "dashed"),
    name = "",
    guide = guide_legend(order = 2)  # Placerer linjetypen under farvelegenden
  ) +
  geom_sf(data = map_eu, fill = 	"#c3fbb1") +
  #coord_sf(xlim = c(4450000, 4545210), ylim = c(3539000, 3635709)) + #hele undersøgelsesområdet
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.81, 0.90),
    legend.justification = "center",
    legend.title = element_text(size = 20),
    legend.text=element_text(size=18)
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(3.5, "cm"),
    pad_y = unit(1, "cm"),
    height = unit(1.8, "cm"),
    width = unit(1.8,"cm")
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.05,
    height = unit(0.4, "cm"),
    bar_cols = c("black", "white"),
    pad_x = unit(0.2, "cm"),
    pad_y = unit(1.5, "cm"),
    text_cex = 1.2 # Juster tekststørrelse 
  )
print(map_vandomrader)


