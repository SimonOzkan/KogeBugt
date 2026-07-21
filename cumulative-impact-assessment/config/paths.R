# Definerer stier til Teams-mappen og lokale mapper
# Brug: paths <- set_project_paths()

set_project_paths <- function() {
 
  # Windows team base:
  TEAMS_BASE <- "C:/Users/SIO/NIVA/Køge Bugt - General"
  
  # Mac eksempel:
  # TEAMS_BASE <- "/Users/[username]/Library/.../.../Teams/Køge Bugt - General"
  
  # Opret liste med alle relevante stier
  paths <- list(
    # Input data fra Teams
    input_ecosystem = file.path(TEAMS_BASE, "Data/Ecosystem"),
    input_pressure = file.path(TEAMS_BASE, "Data/Pressure"),
    input_assessment_area = file.path(TEAMS_BASE, "Data/Assessment_Area"),
    
    # Output data til Teams 
    output_ecosystem_tif = file.path(TEAMS_BASE, "Data/Outputs/EC_tif"),
    output_pressure_tif = file.path(TEAMS_BASE, "Data/Outputs/Pressure_tif"),
    
    
    output_ecosystem_png = file.path(TEAMS_BASE, "Data/Outputs/EC_png"),
    output_pressure_png = file.path(TEAMS_BASE, "Data/Outputs/Pressure_png"),
    
    # Lokale temp-files 
    local_temp = here::here("outputs")
  )
  
  # Tjek at Teams-mapperne findes
  if (!dir.exists(TEAMS_BASE)) {
    warning(paste("TEAMS_BASE eksisterer ikke:", TEAMS_BASE))
    warning("Opdater stien i config/paths.R")
  }
  
  # Opret mapper hvis de ikke eksisterer
  invisible(lapply(paths, function(p) {
    if (!dir.exists(p)) {
      dir.create(p, recursive = TRUE, showWarnings = FALSE)
      cat("Oprettet mappe:", p, "\n")
    }
  }))
  
  # Opret en .gitkeep i outputs så den bliver tracket
  gitkeep <- here::here("outputs/.gitkeep")
  if (!file.exists(gitkeep)) {
    file.create(gitkeep)
  }
  
  return(paths)
}

# ====================================================================
# EKSEMPEL PÅ BRUG I DINE SCRIPTS:
# ====================================================================
# 
# # I toppen af hvert script:
# source(here::here("config/paths.R"))
# PATHS <- set_project_paths()
#
# # Læs data fra Teams-mappe:
# ecosystem_data <- read.csv(file.path(PATHS$input_ecosystem, "natura2000.csv"))
#
# # Gem output til Teams:
# write.csv(result, file.path(PATHS$output, "cumulative_impact_2024.csv"))
#
# # Gem midlertidigt til lokal temp (ignoreres af Git):
# saveRDS(large_object, file.path(PATHS$local_temp, "temp_cache.rds"))
# ====================================================================
