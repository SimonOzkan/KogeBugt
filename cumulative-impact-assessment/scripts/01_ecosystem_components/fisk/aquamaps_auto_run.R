


source("scripts/00_setup.R")
PATHS <- set_project_paths()


#install.packages("devtools")
#devtools::install_github("raquamaps/aquamapsdata", dependencies = TRUE)

library(aquamapsdata)
default_db("sqlite")      # genetabler forbindelsen til den downloadede db
#download_db(force = TRUE)   # KØR ÉN GANG - downloader ~2 GB, pakker ud til ~10 GB

# Hent miljøparametre



depth <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_depth_250m\\depth_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
temp <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_temperature_surface_mean_resample_250m\\temperature_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
# temp_bot <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_temperature_bottom_mean_resample_250m\\temperature_bottom_mean_resample_250m.tif")) %>%
#   terra::project(., grid_raster, method = "near")
salinity <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_salinity_surface_mean_resample_250m\\salinity_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
primprod <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_primary_productivity_surface_mean_resample_250m\\primary_productivity_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
mxld <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_mixed_layer_thickness_mean_resample_250m\\mixed_layer_thickness_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")

icecon <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_sea_ice_fraction_mean_resample_250m\\sea_ice_fraction_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")


arter <- tibble::tribble(
  ~navn,          ~genus,            ~species,     ~kategori,
  "Ål",           "Anguilla",        "anguilla",   "Ål",
  "Sild",         "Clupea",          "harengus",   "Erhvervsfisk",
  "Makrel",       "Scomber",         "scombrus",   "Erhvervsfisk",
  "Sej",          "Pollachius",      "virens",     "Erhvervsfisk",
  "Brisling",     "Sprattus",        "sprattus",   "Erhvervsfisk",
  "Hornfisk",     "Belone",          "belone",     "Erhvervsfisk",
  "Laks",         "Salmo",           "salar",      "Erhvervsfisk",
  "Skrubbe",      "Platichthys",     "flesus",     "Erhvervsfisk",
  "Pighvar",      "Scophthalmus",    "maximus",    "Erhvervsfisk",
  "Rødspætte",    "Pleuronectes",    "platessa",   "Erhvervsfisk",
  "Sandkutling",  "Pomatoschistus",  "minutus",    "Ikke-kommerciel",
  "Torsk",        "Gadus",           "morhua",     "Ikke-kommerciel",
  "Ålekvabbe",    "Zoarces",         "viviparus",  "Ikke-kommerciel",
  "Ulk",          "Myoxocephalus",   "scorpius",   "Ikke-kommerciel",
  "Ising",        "Limanda",         "limanda",    "Ikke-kommerciel"
)


## Tjekker om det er de samme parametre der skal bruges for alle fisk eller om oxygen eller distance to land er relevant for nogle

# Lav tom dataframe til append
df_empty <- data.frame(matrix(ncol = 56, nrow = 0))
x <- names(hsp)
colnames(df_empty) <- x


for(i in seq_len(nrow(arter))){
  
  art <- slice(arter, i)    
  
  key_i <- am_search_exact(Genus = art$genus, Species = art$species)$SpeciesID[1]
  hsp_i <- am_hspen() %>% filter(SpeciesID == key_i) %>% head(1) %>% collect()
  
  df_empty <- rbind(df_empty, hsp_i)
  
}
# Dette viser at alle fisk kan modelleres baseret på de samme parametre
art_YN_tjek <- df_empty %>% 
  dplyr::select(dplyr::ends_with("YN"))

### 

# art <- arter %>% filter(navn == "Ising")      # eller: slice(arter, 1)
# 
# key <- am_search_exact(Genus = art$genus, Species = art$species)$SpeciesID
# hsp <- am_hspen() %>% filter(SpeciesID == key) %>% head(1) %>% collect()

### Kører alle fisk igennem og gemmer plots ##########
## ---- defineres ÉN gang før løkken ----
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
      brug = isTRUE(hsp[[yn]] == 1),
      layer = hsp$Layer
    )
  })
}
suit <- function(r, mn, pmin, pmax, mx) {
  rising  <- if (pmin > mn) (r - mn)/(pmin - mn) else r*0 + 1
  falling <- if (mx > pmax) (mx - r)/(mx - pmax) else r*0 + 1
  clamp(ifel(r >= pmin & r <= pmax, 1,
             ifel(r <= mn | r >= mx, 0,
                  ifel(r < pmin, rising, falling))), 0, 1)
}

fill_na <- function(r, template, w = 3, max_iter = 200) {
  out <- r
  for (k in seq_len(max_iter)) {
    if (global(is.na(out) & !is.na(template), "sum")[1,1] == 0) break
    out <- cover(out, focal(out, w = w, fun = "mean", na.rm = TRUE))
  }
  mask(out, template)
}

temp <- temp_surf                         # alt er surface
ref  <- grid_raster

# resample alle miljølag til gridet ÉN gang (ens for alle arter)
lag_base <- lapply(list(depth = depth, temp = temp, salinity = salinity,
                        primprod = primprod, icecon = icecon),
                   function(r) if (!compareGeom(r, ref, stopOnError = FALSE))
                     resample(r, ref, method = "bilinear") else r)



## ---- løkken ----
for (i in seq_len(nrow(arter))) {
  art   <- dplyr::slice(arter, i)
  key_i <- am_search_exact(Genus = art$genus, Species = art$species)$SpeciesID[1]
  
  if (length(key_i) == 0 || is.na(key_i)) {
    message("Springer over (ingen key): ", art$navn); next
  }
  
  hsp_i  <- am_hspen() %>% filter(SpeciesID == key_i) %>% head(1) %>% collect()
  env_i  <- hspen_to_env(hsp_i) %>%
    mutate(brug = case_when(par == "primprod" ~ FALSE, .default = brug))
  brug_i <- env_i %>% filter(brug)
  
  prob <- ref * 0 + 1
  for (p in intersect(brug_i$par, names(lag_base))) {     # <- brug_i, ikke brug
    e <- brug_i[brug_i$par == p, ]
    prob <- prob * suit(lag_base[[p]], e$min, e$pmin, e$pmax, e$max)
  }
  names(prob) <- "value"
  
  prob_filled <- fill_na(prob, grid_raster, w = 3)
  
  writeRaster(prob_filled,
              file.path(PATHS$output_ecosystem_tif, "fisk",
                        paste(art$genus, art$species, "aquamaps_downscaled.tif", sep = "_")),
              overwrite = TRUE)
  
  message("Gemt: ", art$navn, "  (", i, "/", nrow(arter), ")")
}
