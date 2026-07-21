# Definerer stier til Teams-mappen og lokale mapper
# Brug: paths <- set_project_paths()

set_project_paths <- function() {
  # ====================================================================
  # TILPAS DENNE STI TIL DIN LOKALE TEAMS-MAPPE
  # ====================================================================
  # Windows eksempel:
  TEAMS_BASE <- "C:/Users/SIO/NIVA/Køge Bugt - General"
  
  # Mac eksempel (fjern #):
  # TEAMS_BASE <- "/Users/[username]/Library/CloudStorage/OneDrive-AarhusUniversitet/Teams/Køge Bugt - General"
  
  # Opret liste med alle relevante stier
  paths <- list(
    # Input data fra Teams
    input_ecosystem = file.path(TEAMS_BASE, "Data/Ecosystem"),
    input_pressure = file.path(TEAMS_BASE, "Data/Pressure"),
    
    # Output data til Teams (delt med kollegaer)
    output = file.path(TEAMS_BASE, "Data/Outputs"),
    
    # Lokale temp-files (IKKE delt, ignoreres af Git)
    local_temp = here::here("outputs")
  )
  
  # Tjek at Teams-mappen findes
  if (!dir.exists(TEAMS_BASE)) {
    warning(paste("TEAMS_BASE eksisterer ikke:", TEAMS_BASE))
    warning("Opdater stien i config/paths.R")
  }
  
  # Opret mapper hvis de ikke eksisterer
  invisible(lapply(paths, function(p) {
    if (!dir.exists(p)) {
      dir.create(p, recursive = TRUE, showWarnings = FALSE)
      cat("✓ Oprettet mappe:", p, "\n")
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
