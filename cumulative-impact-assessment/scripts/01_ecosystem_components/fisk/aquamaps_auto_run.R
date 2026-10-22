# Aquamaps downscaling hvor der køres loop over alle arter og gemmer som raster og png #


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
depth <- fill_na(depth, grid_raster, w = 3)

temp <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_temperature_surface_mean_resample_250m\\temperature_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
temp <- fill_na(temp, grid_raster, w = 3)

## Alle arter er modelleret fra overflade lag 
# temp_bot <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_temperature_bottom_mean_resample_250m\\temperature_bottom_mean_resample_250m.tif")) %>%
#   terra::project(., grid_raster, method = "near")
salinity <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_salinity_surface_mean_resample_250m\\salinity_surface_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
salinity <- fill_na(salinity, grid_raster, w = 3)

icecon <- terra::rast(file.path(PATHS$input_phys_chem_geo, "\\_ags_sea_ice_fraction_mean_resample_250m\\sea_ice_fraction_mean_resample_250m.tif")) %>%
  terra::project(., grid_raster, method = "near")
icecon <- fill_na(icecon, grid_raster, w = 3)
# Primary production fra Copernicus 
#https://data.marine.copernicus.eu/viewer/expert?view=viewer&crs=epsg:4326&t=1767139200000&z=-0.5016462206840515&center=12.448322445580079,55.35093677732264&zoom=16.8&layers=H4sIAM.j2WoAAz2MywqCQBhG3_VfG01jq9mZSAgFkbaQiJ.RmXRgLuKNTHz3JoJ233cOnPsCbmhkFzvXiR7Ysgag_Sy7VACDQ3TK0zhLIjzf.CqS6IqHY4yEhEh2dFsZaXo0TmDJNZZ1hWbGy67YGKSEhiTc2radIIB3aoV8AaMkAPUto6Yea1dnFdcS2JPrXnrXJ0INytZ5M5rScqX.auJ6lGdlgfnG73Bf3JP18QHVNcWMxQAAAA--&basemap=dark&objv2=H4sIAM.j2WoAA3WP0QrCIBSG3_Vci_Tm0nkZ1HX3Q4Y1W0Kb4hw0wndPC0ZB3X7f_c85f.MA04GA1lJAcNEqzF5PIBIPi9PJHN4s2V7bQQe.gFjl0d6W3o5Jnq31nRlVeKWbhpSYlsWWE1RVmFUbXhYSfVKKGSsYp5kSXDNSE.6P.t6wUikjAuet0z6YfP_r1KiG.Gq7hxhlauGVu_aZlDHT7mTvIIKfdZRPFgS_QQwBAAA-
primprod_path <- file.path(PATHS$input_phys_chem_geo, "\\_ags_primary_productivity_surface_mean_resample_250m\\BAL-MYP-ERGOM_BGC-AnnualMeans-2024.nc")
npp <- terra::rast(primprod_path, subds = "nppv")
# overflade = øverste dybdelag (depth er "positive down", så lag 1 = overflade)
npp_surf <- npp[[1]]
# sikr NA for fill-value (hvis terra ikke selv har sat det)
npp_surf <- terra::classify(npp_surf, cbind(-999, NA))
# nc'en er lon/lat -> sæt CRS hvis den mangler
if (is.na(crs(npp_surf)) || crs(npp_surf) == "") crs(npp_surf) <- "EPSG:4326"
# reprojicér + align til dit 250 m-grid i ét skridt
primprod_250 <- terra::project(npp_surf, grid_raster, method = "bilinear")
primprod<-fill_na(primprod_250, grid_raster, w =3)

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
# df_empty <- data.frame(matrix(ncol = 56, nrow = 0))
# x <- names(hsp)
# colnames(df_empty) <- x
# 
# 
# for(i in seq_len(nrow(arter))){
#   
#   art <- slice(arter, i)    
#   
#   key_i <- am_search_exact(Genus = art$genus, Species = art$species)$SpeciesID[1]
#   hsp_i <- am_hspen() %>% filter(SpeciesID == key_i) %>% head(1) %>% collect()
#   
#   df_empty <- rbind(df_empty, hsp_i)
#   
# }
# # Dette viser at alle fisk kan modelleres baseret på de samme parametre
# art_YN_tjek <- df_empty %>% 
#   dplyr::select(dplyr::ends_with("YN"))
# 
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
  env_i  <- hspen_to_env(hsp_i) 
  
  brug_i <- env_i %>% filter(brug)
  
  prob <- ref * 0 + 1
  for (p in intersect(brug_i$par, names(lag_base))) {     # <- brug_i, ikke brug
    e <- brug_i[brug_i$par == p, ]
    prob <- prob * suit(lag_base[[p]], e$min, e$pmin, e$pmax, e$max)
  }
  names(prob) <- "value"
  
  
  writeRaster(prob,
              file.path(PATHS$output_ecosystem_tif, "fisk",
                        paste(art$genus, art$species, "aquamaps_downscaled.tif", sep = "_")),
              overwrite = TRUE)
  
  # Konverter til sf polygon (kun present beholdes, 0 -> NA fjerner "absent"-celler)
  prob_sf <- prob %>%
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
  
  
  gem_trim(map_prob,
           file.path(PATHS$output_ecosystem_png,"fisk/",paste(art$genus,art$species,"aquamaps_downscaled.png", sep = "_" )),
           dpi = 360, width_cm = 18)
  
  
  message("Gemt: ", art$navn, "  (", i, "/", nrow(arter), ")")
}
