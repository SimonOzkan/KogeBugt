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
taxid <- as.integer(str_extract(names(fugle_list), "(?<=taxonid=)\\d+"))
sci   <- map_chr(taxid, ~ tryCatch(wm_id2name(.x), error = \(e) paste0("taxon_", .x)))
slug  <- tolower(gsub(" ", "_", sci))
names(fugle_list) <- slug
print(data.frame(taxid, sci))     # tjek at mapping passer

## --- pipeline pr. art (præcis som sortand) ---
til_grid <- function(r) {
  r <- r[["current"]] / 100                          # prædiktion -> 0-1
  r <- project(r, grid_raster, method = "bilinear")  # reprojicér + resample
  r <- fill_na(r, grid_raster)                       # fyld kystnære huller
}
fugle_grid <- rast(lapply(fugle_list, til_grid))
names(fugle_grid) <- slug

## --- gem individuelle raster pr. art ---
out_dir <- file.path(PATHS$output_ecosystem_tif, "fugle")   # din output-mappe
dir_create(out_dir)
walk(slug, ~ writeRaster(fugle_grid[[.x]],
                         file.path(out_dir, paste0("fugl_", .x, "_norm.tif")), overwrite = TRUE))

## --- samlet lag: gennemsnit på tværs af alle arter ---
fugle_mean <- mean(fugle_grid, na.rm = TRUE)   # celle-vist middel over arterne
names(fugle_mean) <- "fugle_gennemsnit"
writeRaster(fugle_mean, file.path(out_dir, "fugle_samlet_gennemsnit_norm.tif"),
            overwrite = TRUE)

plot(fugle_grid); plot(fugle_mean)


