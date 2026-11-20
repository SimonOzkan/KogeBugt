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
  st_drop_geometry() %>%
  group_by(id,habitat) %>%
  summarise(a_habs_sum = sum(intersect_area, na.rm =TRUE), .groups = "drop") %>%
  mutate(frac = a_habs_sum / 62500) # area of grid cell


habitat_typer <- unique(koge_habs_A$habitat)



## Lav ét raster per habitattype
for (hab in habitat_typer) {
  
  message("Laver raster for: ", hab)
  
  hab_join <- koge_habs_A %>%
    filter(habitat == hab) 
  
  # Filtrer til ét habitat
  hab_frac <- left_join(grid_sf,hab_join) %>%
    mutate(frac = tidyr::replace_na(frac,0))

  
  # Rasterize — brug area_frac som værdi
  r_hab <- terra::rasterize(
    terra::vect(hab_frac),
    grid_raster,
    field      = "frac",
    fun        = "max",
    background = NA
  )


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
  
  
  hab_sf <- r_hab %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    st_make_valid()
  
  map_hab <- ggplot() +
    geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
    geom_sf(data = hab_sf, 
            aes(fill = frac), color = NA) +
    color_viridis+
    boundary+
    theme_minimal()+
    my_theme+
    north_arrow+
    scale_bar
  
  
  gem_trim(map_hab,
           file.path(PATHS$output_ecosystem_png, "broad_scale_hab", paste0(hab_filename, ".png")),
           dpi = 360, width_cm = 18)
  
  
  message("  → Gemt: ", hab_filename, ".png")
  
}



## Tjek et enkelt lag

test_hab <- terra::rast(file.path(PATHS$output_ecosystem_tif, "Circalittoral_sand.tif"))
plot(test_hab, main = "Fraktion Circalittoral sand per 250m celle")

