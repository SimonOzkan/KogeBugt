##### Fugle fra OBIS SDM ####
# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()


library(aws.s3)

output_folder <- file.path(PATHS$input_ecosystem,"fugle")
fs::dir_create(output_folder)

bucket    <- "obis-maps"      # var "mpaeu-dist"
s3_folder <- "sdm/species"    # var "results/species"

# Create a function to download
download_objs <- function(s3_objects, local_folder) {
  i <- 0
  total <- length(s3_objects)
  for (obj in s3_objects) {
    i <- i + 1
    cat("Downloading", i, "out of", total, "\n")
    s3_key <- obj$Key
    local_file <- file.path(local_folder, s3_key)
    
    if (!endsWith(s3_key, "/")) {
      save_object(
        object = s3_key,
        bucket = bucket,
        file = local_file,
        region = "",
        use_https = TRUE 
      )
      message(paste("Downloaded:", s3_key, "to", local_file))
    }
  }
  return(invisible(NULL))
}

# CODE 1: faster, look only at the target species
species <- c(137073,   # Sortand (Melanitta nigra)
             137128,   # Alk  (Alcida Torda)
             137074,   #Ederfugl (Somateria mollissima)
             159098,   #Toppet skallesluger (Mergus serrator)
             137186,   #Sort-strubet lom (Gavia arctica)
             137071)   #Havlit (Clangula hyemalis)

for (sp in species) {
  
  s3_objects <- get_bucket(
    bucket = bucket,
    prefix = paste0(s3_folder, "/taxonid=", sp, "/model=mpaeu/predictions"),
    use_https = TRUE, max = Inf
  )
  
  # behold kun ensemble, nutid (dropper bootcv og alle ssp-scenarier)
  s3_objects <- Filter(function(o) grepl("ensemble_scen=current_cog\\.tif$", o$Key),
                       s3_objects)
  
  download_objs(s3_objects, file.path(output_folder, paste0("taxonid=", sp)))
  
}


## Hent tif filerne der er gemt lokalt

folder_path <- file.path(PATHS$input_ecosystem,"fugle")

rastlist <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)  

length(rastlist)

# læs alle som en liste af SpatRasters
fugle_list <- lapply(rastlist, terra::rast)
names(fugle_list) <- tools::file_path_sans_ext(basename(rastlist))


## --- artsnavne fra taxonid (autoritativt via WoRMS) ---
## --- artsnavne fra taxonid ---
art_navne <- tribble(
  ~taxid,  ~sci,                   ~dansk,
  137073,  "Melanitta nigra",      "sortand",
  137128,  "Alca torda",           "alk",
  137074,  "Somateria mollissima", "ederfugl",
  159098,  "Mergus serrator",      "toppet_skallesluger",
  137186,  "Gavia arctica",        "sortstrubet_lom",
  137071,  "Clangula hyemalis",    "havlit"
)

taxid <- as.integer(str_extract(basename(rastlist), "(?<=taxonid=)\\d+"))

art_navn <- tibble(taxid = taxid) %>%
  left_join(art_navne, by = "taxid") %>%
  mutate(slug = tolower(gsub(" ", "_", sci)))

stopifnot(!any(is.na(art_navn$slug)),
          !any(duplicated(art_navn$slug)))

slug <- art_navn$slug
names(fugle_list) <- slug
print(art_navn)   # tjek at mapping passer



####### tilføjer kyst buffer til at fill na langs kysten. Nogle fugle er ikke tilstede længere ude i øresund/køge Bugt
# Den oprindelige fill_na fyldte dog disse punkter og skabte forvrædning i data

bbox <- st_read(file.path(PATHS$input_assessment_area,"bbox_omraade","koge_bugt_bbox.shp")) %>%
  st_transform(crs = target_crs)
  
eu_kyst <- st_read(file.path(PATHS$input_assessment_area,"Europe_coastline_poly","Europe_coastline_poly.shp")) %>%
  st_transform(crs = target_crs)



kyst_buffer <- 5000   # meter, ca. én OBIS-celle


land_sf <- eu_kyst %>%
  st_make_valid() %>%
  st_crop(st_bbox(st_buffer(bbox, kyst_buffer)))


land_r <- land_sf %>%
  terra::vect() %>%
  terra::rasterize(grid_raster, field = 1)   # 1 på land, NA i havet

afstand_land <- terra::distance(land_r)       # afstand fra havceller til nærmeste land

kystzone <- terra::ifel(afstand_land <= kyst_buffer & !is.na(grid_raster), 1, NA)

plot(kystzone)
plot(st_geometry(land_sf), add = TRUE)


fill_kyst <- function(r, template, kystzone, w = 3, max_iter = 20) {
  out <- r
  for (i in seq_len(max_iter)) {
    mangler <- global(is.na(out) & !is.na(kystzone), "sum", na.rm = TRUE)[1, 1]
    if (mangler == 0) break
    filled <- terra::focal(out, w = w, fun = "mean", na.rm = TRUE)  # nabo-gennemsnit
    filled <- terra::mask(filled, kystzone)                          # kun inden for kystzonen
    out    <- terra::cover(out, filled)                              # fyld kun NA
  }
  out <- terra::cover(out, template * 0)   # resterende NA i området -> 0
  terra::mask(out, template)
}
## --- pipeline pr. art (præcis som sortand) ---
til_grid <- function(r) {
  r <- r[["current"]] / 100                          # prædiktion -> 0-1
  r <- project(r, grid_raster, method = "bilinear")  # reprojicér + resample
  fill_kyst(r, grid_raster, kystzone,
            max_iter = kyst_buffer / res(grid_raster)[1])  # fyld kun ved kysten, resten = 0
}
fugle_grid <- rast(lapply(fugle_list, til_grid))
names(fugle_grid) <- slug

plot(fugle_grid$mergus_serrator)
plot(kystzone, add = TRUE, col = "red")

## --- gem individuelle raster pr. art ---
out_dir <- file.path(PATHS$output_ecosystem_tif, "fugle")   # din output-mappe
walk(slug, ~ writeRaster(fugle_grid[[.x]],
                         file.path(out_dir, paste0("fugl_", .x, ".tif")), overwrite = TRUE))

## --- samlet lag: gennemsnit på tværs af alle arter ---
fugle_mean <- mean(fugle_grid, na.rm = TRUE)   # celle-vist middel over arterne
names(fugle_mean) <- "fugle_gennemsnit"
writeRaster(fugle_mean, file.path(out_dir, "fugle_samlet_gennemsnit_norm.tif"),
            overwrite = TRUE)



## Lav plot til bilag for hver art
for (art in fugle_arter) {
  
  message("Laver kort for: ", art)
  
  art_sf <- fugle_grid[[art]] %>%
    terra::as.polygons(dissolve = FALSE) %>%
    st_as_sf() %>%
    rename(value = 1) %>%          # første kolonne = artens værdier
    st_transform(target_crs) %>%
    st_make_valid()
  
  map_art <- ggplot() +
    geom_spatraster_rgb(data = tiles, maxcell = 5e6) +  # baggrund: relief
    geom_sf(data = art_sf, aes(fill = value), color = NA) +
    color_viridis +
    boundary +
    theme_minimal() +
    my_theme +
    north_arrow +
    scale_bar
  
  fil_navn <- paste0("fugl_", art)
  
  gem_trim(map_art,
           file.path(file.path(PATHS$output_ecosystem_png, "fugle", paste0(fil_navn, ".png"))),
           dpi = 360, width_cm = 18)
  
  message("  → Gemt: ", fil_navn, ".png")
}
