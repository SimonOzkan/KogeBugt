### Aquamaps downscaling ###


source("scripts/00_setup.R")
PATHS <- set_project_paths()


#install.packages("devtools")
#devtools::install_github("raquamaps/aquamapsdata", dependencies = TRUE)

library(aquamapsdata)
#download_db(force = TRUE)   # KØR ÉN GANG - downloader ~2 GB, pakker ud til ~10 GB

# Hent miljøparametre


depth <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_depth_250m\\depth_250m.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")
temp_surf <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_temperature_surface_mean_resample_250m\\temperature_surface_mean_resample_250m.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

salinity <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_salinity_surface_mean_resample_250m\\salinity_surface_mean_resample_250m.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")
primprod <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_primary_productivity_surface_mean_resample_250m\\primary_productivity_surface_mean_resample_250m.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")
mxld <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_mixed_layer_thickness_mean_resample_250m\\mixed_layer_thickness_mean_resample_250m.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")

icecon <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_sea_ice_fraction_mean_resample_250m\\sea_ice_fraction_mean_resample_250m.tif")) %>%
  terra::project(., "EPSG:25832", method = "near")



arter <- tibble::tribble(
  ~navn,              ~genus,          ~species,
  "Skrubbe",          "Platichthys",   "flesus",
  "Rødspætte",        "Pleuronectes",  "platessa",
  "Torsk",            "Gadus",         "morhua",
  "Marsvin",          "Phocoena",      "phocoena",
  "Ål",               "Anguilla",       "anguilla",
  "Sild",             "Clupea",         "harengus" 
)

art <- arter %>% filter(navn == "Sild")      # eller: slice(arter, 1)

key <- am_search_exact(Genus = art$genus, Species = art$species)$SpeciesID
hsp <- am_hspen() %>% filter(SpeciesID == key) %>% head(1) %>% collect()


hspen_to_env <- function(hsp) {
  map <- tibble::tribble(
    ~par,       ~prefix,     ~yn,
    "depth",    "Depth",     "DepthYN",
    "temp",     "Temp",      "TempYN",
    "salinity", "Salinity",  "SalinityYN",
    "primprod", "PrimProd",  "PrimProdYN",
    "icecon",   "IceCon",    "IceConYN",
    "oxygen",   "Oxy",       "OxyYN",
    "landdist", "LandDist",  "LandDistYN"
  )
  purrr::pmap_dfr(map, function(par, prefix, yn) {
    tibble::tibble(
      par  = par,
      min  = hsp[[paste0(prefix, "Min")]],
      pmin = hsp[[paste0(prefix, "PrefMin")]],
      pmax = hsp[[paste0(prefix, "PrefMax")]],
      max  = hsp[[paste0(prefix, "Max")]],
      brug = isTRUE(hsp[[yn]] == 1)
    )
  })
}

env <- hspen_to_env(hsp)

### Sætter parametre til FALSE manuelt
env <- env %>%
  mutate(brug = case_when(par == "primprod" ~ FALSE,
                          .default = brug))


# 2) Parameter input data lag — ét SpatRaster pr. envelope-variabel, i de RETTE enheder
##    (navnene skal matche env$par)
lag <- list(
  depth    = depth,
  temp     = temp_surf,
  salinity = salinity,
  primprod = primprod,
  icecon = icecon
  # oxygen = oxygen,
  # landdist = landdist
)

brug <- env %>% filter(brug)

mangler <- setdiff(brug$par, names(lag))
if (length(mangler)) message("Springer over (intet raster): ", paste(mangler, collapse = ", "))


## 3) Trapez-suitability for ét lag (terra-native, med nul-division-værn)
suit <- function(r, mn, pmin, pmax, mx) {
  rising  <- if (pmin > mn) (r - mn) / (pmin - mn) else r * 0 + 1
  falling <- if (mx > pmax) (mx - r) / (mx - pmax) else r * 0 + 1
  s <- ifel(r >= pmin & r <= pmax, 1,
            ifel(r <= mn   | r >= mx,   0,
                 ifel(r <  pmin, rising, falling)))
  clamp(s, 0, 1)
}
## 4) Fælles grid: resample alle lag til et referencelag (fx finest/dit AOI)
ref <- grid_raster
lag <- lapply(lag, function(r) {
  if (!compareGeom(r, ref, stopOnError = FALSE)) r <- resample(r, ref, method = "bilinear")
  r
})

prob <- ref * 0 + 1
suit_stack <- list()
for (p in intersect(brug$par, names(lag))) {
  e <- brug[brug$par == p, ]
  s <- suit(lag[[p]], e$min, e$pmin, e$pmax, e$max)
  suit_stack[[p]] <- s
  prob <- prob * s
}
names(prob) <- "value"
plot(rast(suit_stack))   # ét suitability-kort pr. variabel
plot(prob)               # samlet sandsynlighed 0-1


terra::writeRaster(
  prob,
  filename = file.path(PATHS$output_ecosystem_tif,"fisk/",paste(art$genus,art$species,"aquamaps_downscaled.tif", sep = "_" )),
  overwrite = TRUE
)


# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
prob_sf <- prob %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()

# plot
map_eu <- st_read(file.path(PATHS$input_assessment_area,"/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(.,crs = target_crs)


# Sætter baggrundskortet i.e. hvor "value/fraction" = 0 til samme farve 
viridis_start_color <- viridis_pal()(1)  

map_prob <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.5) +
 # geom_sf(data = assessment_area_dissolved, fill = viridis_start_color, color = "white", alpha = 1) +
  geom_sf(data = prob_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_prob


ggsave(plot = map_prob,
       filename = file.path(PATHS$output_ecosystem_png,"fisk/",paste(art$genus,art$species,"aquamaps_downscaled.png", sep = "_" )),
       bg = NULL,
       height = 18,
       width = 18,
       dpi = 300)



##
## skalar trapez functions plot
suit_curve <- function(x, mn, pmn, pmx, mx) {
  rising  <- if (pmn > mn) (x - mn) / (pmn - mn) else rep(1, length(x))
  falling <- if (mx > pmx) (mx - x) / (mx - pmx) else rep(1, length(x))
  pmax(0, pmin(1, rising, falling))      # = klemt min af de to ramper
}

## kurve-data pr. parameter (lidt margin ude i siderne)
kurver <- brug %>% pmap_dfr(function(par, min, pmin, pmax, max, ...) {
  pad <- (max - min) * 0.15
  x   <- seq(min - pad, max + pad, length.out = 400)
  tibble(par = par, x = x, suitability = suit_curve(x, min, pmin, pmax, max))
})

## de fire knækpunkter (Min/Max = 0, PrefMin/PrefMax = 1)
punkter <- brug %>%
  transmute(par, Min = min, PrefMin = pmin, PrefMax = pmax, Max = max) %>%
  pivot_longer(-par, names_to = "knæk", values_to = "x") %>%
  mutate(suitability = if_else(knæk %in% c("PrefMin", "PrefMax"), 1, 0))

ggplot(kurver, aes(x, suitability)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(data = punkter, color = "red", size = 2) +
  facet_wrap(~ par, scales = "free_x") +
  labs(x = "Parameterværdi", y = "Suitability (0–1)") +
  theme_bw()


