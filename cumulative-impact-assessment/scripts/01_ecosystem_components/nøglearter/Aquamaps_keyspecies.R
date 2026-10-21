### Aquamaps downscaling Key species #### 

#For nogle nøglearter er environmental envelope på AquaMaps hjemmesiden opdateret med ekspert viden
#DIsse tal er ikke opdateret i R-pakken, derfor kan dette dokument anvendes til at lave kort for nøglearter


source("scripts/00_setup.R")
PATHS <- set_project_paths()


#install.packages("devtools")
#devtools::install_github("raquamaps/aquamapsdata", dependencies = TRUE)

library(aquamapsdata)
default_db("sqlite")      # genetabler forbindelsen til den downloadede db
am_search_fuzzy("cod")
#download_db(force = TRUE)   # KØR ÉN GANG - downloader ~2 GB, pakker ud til ~10 GB

# Hent miljøparametre


depth <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_depth_250m\\depth_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
temp_surf <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_temperature_surface_mean_resample_250m\\temperature_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
temp_bot <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_temperature_bottom_mean_resample_250m\\temperature_bottom_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
salinity <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_salinity_surface_mean_resample_250m\\salinity_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
primprod <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_primary_productivity_surface_mean_resample_250m\\primary_productivity_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
mxld <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_mixed_layer_thickness_mean_resample_250m\\mixed_layer_thickness_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")

icecon <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_sea_ice_fraction_mean_resample_250m\\sea_ice_fraction_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")


arter <- tibble::tribble(
  ~navn,              ~genus,          ~species,
  "Skrubbe",          "Platichthys",   "flesus",
  "Rødspætte",        "Pleuronectes",  "platessa",
  "Torsk",            "Gadus",         "morhua",
  "Marsvin",          "Phocoena",      "phocoena",
  "Ål",               "Anguilla",       "anguilla",
  "Sild",             "Clupea",         "harengus",
  "Gråsæl",           "Halichoerus",     "grypus"
)

art <- arter %>% filter(navn == "Gråsæl")      # eller: slice(arter, 1)

key <- am_search_exact(Genus = art$genus, Species = art$species)$SpeciesID
hsp <- am_hspen() %>% filter(SpeciesID == key) %>% head(1) %>% collect()


if(hsp$Layer == "s"){
  temp = temp_surf
}else{
  temp = temp_bot
}

# ##### Environmental envelope for Marsvin #### ##########################
# env <- tibble::tribble(
#   ~par,        ~min,   ~pmin,  ~pmax,   ~max,   ~brug,
#   "depth",       0,      1,     60,     500,    TRUE,
#   "temp",       -0.6,   5.44,  14.87,   24.5, TRUE,
#   "salinity",    3.58,   24.18,  35.23,   40.44, TRUE,
#   "primprod",    0.47,   2.64,  20.14,   48.18, FALSE,
#   "icecon",     -0.99,   0,      0.01,    0.96,  TRUE,
#   "oxygen",    118.57, 246.63, 366.92,  413.49, FALSE,
#   "landdist",    0,      3,    119,     637,    FALSE
# )
##########################################################

##### Environmental envelope for Gråsæl ###################### 
env <- tibble::tribble(
  ~par,        ~min,   ~pmin,  ~pmax,   ~max,   ~brug,
  "depth",       0,      1,     50,     500,    TRUE,
  "temp",       -1.71,   6.85,  12.85,   17.32, TRUE,
  "salinity",    3.36,   6.35,  35.22,   35.72, TRUE,
  "primprod",    0.47,   2.64,  20.14,   48.18, FALSE,
  "icecon",     -0.99,   0,      0.02,    0.5,  TRUE,
  "oxygen",    118.57, 246.63, 366.92,  413.49, FALSE,
  "landdist",    0,      3,    119,     637,    FALSE
)

####################################
# 2) Parameter input data lag — ét SpatRaster pr. envelope-variabel, i de RETTE enheder
##    (navnene skal matche env$par)


lag <- list(
  depth    = depth,
  temp     = temp,
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


fill_na <- function(r, template, w = 3, max_iter = 200) {
  out <- r
  for (i in seq_len(max_iter)) {
    # tæl NA-celler, der ligger INDEN for template-området
    na_tilbage <- global(is.na(out) & !is.na(template), "sum")[1, 1]
    if (na_tilbage == 0) break
    filled <- terra::focal(out, w = w, fun = "mean", na.rm = TRUE)  # nabo-gennemsnit
    out    <- terra::cover(out, filled)                             # behold eksisterende, fyld kun NA
  }
  mask(out, template)   
}

prob_filled <- fill_na(prob, grid_raster, w = 3)

plot(prob_filled)

terra::writeRaster(
  prob_filled,
  filename = file.path(PATHS$output_ecosystem_tif,"pattedyr/",paste(art$genus,art$species,"aquamaps_downscaled.tif", sep = "_" )),
  overwrite = TRUE
)


# Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
prob_sf <- prob_filled %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid()


map_prob <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = prob_sf, 
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar

map_prob



gem_trim(map_prob,
         file.path(PATHS$output_ecosystem_png,"fisk/",paste(art$genus,art$species,"aquamaps_downscaled.png", sep = "_" )),
         dpi = 360, width_cm = 18)



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
