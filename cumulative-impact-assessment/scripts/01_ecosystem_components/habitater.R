#---------------------- Bentiske habitater (EUSeaMap) -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

# WGS84-udgave bruges kun til wkt_filter ved indlæsning af EUSeaMap (kræver lon/lat)
assessment_wgs84 <- st_transform(assessment_area_dissolved, crs = 4326)


## Indlæs EUSeaMap bentiske habitater (se data folder / metadata_log)

gpkg_path <- file.path(PATHS$input_ecosystem, "\\broad_scale_benthic\\EUSeaMap_2025\\EUSeaMap_2025\\EUSeaMap_2025.gpkg")

st_layers(gpkg_path)

# Hent alle unikke All2019DL2 værdier uden at indlæse geometri (hurtig oversigt)
unikke_habitater <- st_read(
  gpkg_path,
  layer = "EUSeaMap_2025",
  query = "SELECT DISTINCT All2019DL2 FROM EUSeaMap_2025" ## Anvender All2019DL2, eftersom All2019DL2 ikke inkluderer kysten 
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

euseamap_koge_hab <- euseamap_koge %>%
  mutate(All2019DL2 = stringr::str_remove(All2019DL2, "^M[A-D]\\d+:\\s*"))


koge_unikke_hab <- unique(euseamap_koge_hab$All2019DL2)

benthic_plot <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = euseamap_koge_hab,
          aes(fill = All2019DL2),
          alpha = 1, color = NA) +
  scale_fill_viridis_d(name = "Habitattype", na.value = "red") +
  theme_minimal() +
  labs(title = "Storskala havbundshabitater") +
  boundary+
  theme(plot.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22),
        legend.position = "inside",
        legend.position.inside = c(0.9,0.9))+
  north_arrow+
  scale_bar
ggsave(plot = benthic_plot,
       filename = file.path(PATHS$output_ecosystem_png,"/broad_scale_hab/benhtic_habs_samlet.png"),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)


euseamap_koge <- euseamap_koge_hab %>%
  dplyr::select(All2019DL2, geom)

# Gem shapefil til senere brug/QA
st_write(euseamap_koge,
         file.path(PATHS$input_ecosystem, "/broad_scale_benthic/EuSeamap_all/Koge_bugt_seaMap.shp"),
         append = FALSE)


## Intersection per habitattype med grid

# Tom liste til at gemme resultater
hab_intersection_list <- list()

for (i in seq_along(koge_unikke_hab)) {
  
  hab_navn <- koge_unikke_hab[i]
  message("Behandler (", i, "/", length(koge_unikke_hab), "): ", hab_navn)
  
  # Filtrer ét habitatlag
  hab_layer <- euseamap_koge %>%
    filter(All2019DL2 == hab_navn)
  
  # Spring over hvis tomt
  if (nrow(hab_layer) == 0) {
    message("  → Ingen features, springer over")
    next
  }
  
  # Lav intersection
  tryCatch({
    intersected <- st_intersection(grid_sf, hab_layer) %>%
      dplyr::select(id, All2019DL2) %>%
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

koge_habs_A <- koge_habs_gridded %>%
  mutate(intersect_area = as.numeric(st_area(geom))) %>%
  st_drop_geometry()



## --- Enkelte celler intersecter ikke med EUseaMap grundet opløsning, de tildeles nærmeste substrat type ---

# dominant habitat pr. celle med overlap (til nabo-opslag)
dom <- koge_habs_A %>%
  dplyr::group_by(id) %>%
  dplyr::slice_max(intersect_area, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

known <- grid_sf %>% dplyr::inner_join(dplyr::select(dom, id, All2019DL2), by = "id")
gaps  <- grid_sf %>% dplyr::filter(!id %in% dom$id)      # ~30 celler uden overlap

message(nrow(gaps), " gap-celler udfyldes med nærmeste nabo")

# nærmeste nabocelle med kendt habitat -> overtag dens substrattype
nn <- st_nearest_feature(gaps, known)
gap_fill <- gaps %>% st_drop_geometry() %>%
  dplyr::transmute(
    id,
    All2019DL2     = known$All2019DL2[nn],
    habitat        = known$All2019DL2[nn],
    intersect_area = as.numeric(area_grid)   # hele cellen -> area_frac = 1
  )

# tilføj gap-cellerne til areal-tabellen
koge_habs_A <- dplyr::bind_rows(koge_habs_A, gap_fill)

# din frac-beregning som før (uden "Na"-linjen)
koge_habs_frac <- left_join(grid_sf, koge_habs_A) %>%
  mutate(area_frac = intersect_area / as.numeric(area_grid)) %>%
  st_make_valid()


koge_habs_frac<- left_join(grid_sf, koge_habs_A) %>%
  mutate(area_frac = intersect_area / as.numeric(area_grid),
         All2019DL2 = ifelse(is.na(All2019DL2), "Na", All2019DL2)) %>%
  st_make_valid()



habitat_typer <- unique(koge_habs_frac$habitat)



## Lav ét raster per habitattype
for (hab in habitat_typer) {
  
  message("Laver raster for: ", hab)
  
  # Filtrer til ét habitat
  hab_frac <- koge_habs_frac %>%
    filter(habitat == hab) %>%
    select(id, area_frac)

  
  # Rasterize — brug area_frac som værdi
  r_hab <- terra::rasterize(
    terra::vect(hab_frac),
    grid_raster,
    field      = "area_frac",
    fun        = "max",
    background = 0
  )

  
  # fill_na og masker
  
  r_hab_filled <- fill_na(r_hab,grid_raster, w = 3) 
  
  # Rens habitatnavn til filnavn
  hab_filename <- hab %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]", "") %>%
    stringr::str_trunc(50, ellipsis = "")
  
  terra::writeRaster(
    r_hab,
    filename  = file.path(PATHS$output_ecosystem_tif,"/broad_scale_hab/", paste0(hab_filename, ".tif")),
    overwrite = TRUE
  )
  
  
  message("  → Gemt: ", hab_filename, ".tif")
  
}


############### Plotting for bilag ################

for (hab in habitat_typer) {
  
  message("Laver png for: ", hab)
  
  
  # Filtrer til ét habitat
  hab_frac <- koge_habs_frac %>%
    filter(habitat == hab) %>%
    select(id, area_frac) %>%
    st_drop_geometry()
  
  hab_gridded <- left_join(grid_sf, hab_frac, by = "id") %>%
    mutate(area_frac = ifelse(is.na(area_frac),0, area_frac)) %>%
    st_make_valid()
  
  
  # Rens habitatnavn til filnavn (samme logik som i raster-loopet)
  hab_filename <- hab %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[^A-Za-z0-9_]", "") %>%
    stringr::str_trunc(50, ellipsis = "")
  
  map_hab <- ggplot() +
    geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
    geom_sf(data = hab_gridded,
            aes(fill = area_frac, color = after_scale(fill)),
            linewidth = 0.1) +
    labs(subtitle = hab)+
    color_viridis+
    boundary+
    theme_minimal()+
    my_theme+
    theme(plot.subtitle = element_text(size = 26))+
    north_arrow+
    scale_bar
  
  ggsave(plot = map_hab,
         filename = file.path(PATHS$output_ecosystem_png,"/broad_scale_hab/" ,paste0(hab_filename, ".png")),
         bg = NULL,
         height = 18,
         width = 18,
         dpi = 300)
  
  message("  → Gemt: ", hab_filename, ".png")
}


## Tjek et enkelt lag

test_hab <- terra::rast(file.path(PATHS$output_ecosystem_tif, "Circalittoral_sand.tif"))
plot(test_hab, main = "Fraktion Circalittoral sand per 250m celle")

