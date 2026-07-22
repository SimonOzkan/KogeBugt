#---------------------- Bentiske habitater (EUSeaMap) -----------------

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

# WGS84-udgave bruges kun til wkt_filter ved indlæsning af EUSeaMap (kræver lon/lat)
assessment_wgs84 <- st_transform(assessment_area_dissolved, crs = 4326)


## Indlæs EUSeaMap bentiske habitater (se data folder / metadata_log)

gpkg_path <- file.path(PATHS$input_ecosystem, "\\broad_scale_benthic\\EUSeaMap_2025\\EUSeaMap_2025\\EUSeaMap_2025.gpkg")

st_layers(gpkg_path)

# Hent alle unikke MSFD_BBHT værdier uden at indlæse geometri (hurtig oversigt)
unikke_habitater <- st_read(
  gpkg_path,
  layer = "EUSeaMap_2025",
  query = "SELECT DISTINCT MSFD_BBHT FROM EUSeaMap_2025"
) %>%
  st_drop_geometry()

print(unikke_habitater)

# Indlæs kun habitater indenfor undersøgelsesområdet (wkt_filter begrænser hvad der læses ind - hurtigere end at hente alt)
euseamap_koge <- st_read(
  gpkg_path,
  layer = "EUSeaMap_2025",
  wkt_filter = st_as_text(st_geometry(assessment_wgs84))
) %>%
  st_transform(crs = target_crs) %>%
  st_make_valid() %>%
  st_intersection(assessment_area_dissolved)

# Tjek hvilke habitattyper der findes i undersøgelsesområdet
koge_unikke_hab <- unique(euseamap_koge$MSFD_BBHT)

euseamap_koge <- euseamap_koge %>%
  mutate(MSFD_BBHT_NA = case_when(MSFD_BBHT == "Na" ~ NA,
                                  .default = MSFD_BBHT))

ggplot() +
  geom_sf(data = euseamap_koge,
          aes(fill = MSFD_BBHT_NA),
          alpha = 0.5, color = NA) +
  scale_fill_viridis_d(name = "Habitattype", na.value = "red") +
  theme_minimal() +
  labs(title = "EUSeaMap habitater — Køge Bugt",
       subtitle = "Overlap synligt som farveblanding")

euseamap_koge <- euseamap_koge %>%
  dplyr::select(MSFD_BBHT, geom)

# Gem shapefil til senere brug/QA
st_write(euseamap_koge,
         file.path(PATHS$output_ecosystem_shp, "\\Koge_bugt_seaMap.shp"),
         append = FALSE)


## Intersection per habitattype med grid

# Tom liste til at gemme resultater
hab_intersection_list <- list()

for (i in seq_along(koge_unikke_hab)) {
  
  hab_navn <- koge_unikke_hab[i]
  message("Behandler (", i, "/", length(koge_unikke_hab), "): ", hab_navn)
  
  # Filtrer ét habitatlag
  hab_layer <- euseamap_koge %>%
    filter(MSFD_BBHT == hab_navn)
  
  # Spring over hvis tomt
  if (nrow(hab_layer) == 0) {
    message("  → Ingen features, springer over")
    next
  }
  
  # Lav intersection
  tryCatch({
    intersected <- st_intersection(grid, hab_layer) %>%
      dplyr::select(id, MSFD_BBHT) %>%
      mutate(habitat = hab_navn)
    
    if (nrow(intersected) > 0) {
      hab_intersection_list[[hab_navn]] <- intersected
      message("  → ", nrow(intersected), " grid-celler med dette habitat")
    }
    
  }, error = function(e) {
    message("  → Fejl: ", e$message)
  })
  
}

# Saml alle lag til én sf dataframe
koge_habs_gridded <- bind_rows(hab_intersection_list)


## Beregn areal-fraktion per celle per habitat

koge_habs_frac <- koge_habs_gridded %>%
  mutate(intersect_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  left_join(grid_area, by = "id") %>%
  mutate(area_frac = intersect_area / as.numeric(area_grid),
         # Klamp til 0-1 (afrundingsfejl kan give værdier lidt over 1)
         area_frac = pmin(area_frac, 1))


## Raster template baseret på assessment area

r_template <- terra::rast(
  extent     = terra::ext(assessment_area_vect),
  resolution = 250,
  crs        = "EPSG:25832"
)


habitat_typer <- unique(koge_habs_frac$habitat)


## Lav ét raster per habitattype

for (hab in habitat_typer) {
  
  message("Laver raster for: ", hab)
  
  # Filtrer til ét habitat
  hab_frac <- koge_habs_frac %>%
    filter(habitat == hab) %>%
    select(id, area_frac)
  
  # Join fraktion til grid geometri
  grid_hab <- grid %>%
    left_join(hab_frac, by = "id") %>%
    mutate(area_frac = ifelse(is.na(area_frac), 0, area_frac))
  
  # Rasterize — brug area_frac som værdi
  r_hab <- terra::rasterize(
    terra::vect(grid_hab),
    r_template,
    field      = "area_frac",
    fun        = "max",
    background = NA
  )
  
  # Sæt 0-celler til NA (ingen habitat)
  r_hab[r_hab == 0] <- NA
  
  # Rens habitatnavn til filnavn
  hab_filename <- hab %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]", "") %>%
    stringr::str_trunc(50, ellipsis = "")
  
  terra::writeRaster(
    r_hab,
    filename  = file.path(PATHS$output_ecosystem_tif, paste0(hab_filename, ".tif")),
    overwrite = TRUE
  )
  
  message("  → Gemt: ", hab_filename, ".tif")
}


############### Plotting for bilag ################

map_baltic_sea <- st_read(file.path(PATHS$input_assessment_area, "/maps/BalticSeaMap/iho.shp")) %>%
  st_transform(., crs = target_crs)
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(., crs = target_crs)

for (hab in habitat_typer) {
  
  message("Laver png for: ", hab)
  
  # Filtrer til ét habitat
  hab_frac <- koge_habs_frac %>%
    filter(habitat == hab) %>%
    select(id, area_frac)
  
  # Join fraktion til grid geometri, sæt 0-værdier til NA så kun celler med reelt overlap farvelægges
  grid_hab_plot <- grid %>%
    left_join(hab_frac, by = "id") %>%
    mutate(area_frac = ifelse(is.na(area_frac) | area_frac == 0, NA, area_frac))
  
  # Rens habitatnavn til filnavn (samme logik som i raster-loopet)
  hab_filename <- hab %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]", "") %>%
    stringr::str_trunc(50, ellipsis = "")
  
  map_hab <- ggplot() +
    geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
    geom_sf(data = map_baltic_sea, fill = "white", color = NA, alpha = 1) +
    geom_sf(data = grid_hab_plot,
            aes(fill = area_frac, color = after_scale(fill)),
            linewidth = 0.1) +
    scale_fill_viridis_c(name = hab, limits = c(0, 1), na.value = "transparent") +
    coord_sf(
      crs  = 25832,
      xlim = c(696427, 775958),
      ylim = c(6096053, 6179593)
    ) +
    theme_minimal() +
    theme(
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      axis.text.x      = element_blank(),
      axis.text.y      = element_blank(),
      legend.position  = c(0.81, 0.90),
      legend.justification = "center",
      legend.title     = element_text(size = 20),
      legend.text      = element_text(size = 18),
      axis.ticks = element_blank(),
      plot.margin = grid::unit(c(0, 0, 0, 0), units = "mm"),
      axis.ticks.length = unit(0, "pt")
    ) +
    annotation_north_arrow(
      location    = "br",
      which_north = "true",
      style       = north_arrow_fancy_orienteering,
      pad_x       = unit(3.5, "cm"),
      pad_y       = unit(1.0, "cm"),
      height      = unit(1.8, "cm"),
      width       = unit(1.8, "cm")
    ) +
    annotation_scale(
      location    = "br",
      width_hint  = 0.05,
      height      = unit(0.4, "cm"),
      bar_cols    = c("black", "white"),
      pad_x       = unit(0.2, "cm"),
      pad_y       = unit(1.5, "cm"),
      text_cex    = 1.2
    )
  
  ggsave(plot = map_hab,
         filename = file.path(PATHS$output_ecosystem_png, paste0(hab_filename, ".png")),
         bg = NULL,
         height = 18,
         width = 18,
         dpi = 300)
  
  message("  → Gemt: ", hab_filename, ".png")
}


## Tjek et enkelt lag

test_hab <- terra::rast(file.path(PATHS$output_ecosystem_tif, "Circalittoral_sand.tif"))
plot(test_hab, main = "Fraktion Circalittoral sand per 250m celle")

