
**`cumulative-impact-assessment/config/paths.R`**
```r name=cumulative-impact-assessment/config/paths.R
# Definerer stier til Teams-mappen
set_project_paths <- function() {
  # Tilpas stien til din lokale Teams-mappe
  TEAMS_BASE <- "C:/Users/.../.../Køge Bugt - General"  
  
  paths <- list(
    input_ecosystem = file.path(TEAMS_BASE, "Data/Ecosystem"),
    input_pressure = file.path(TEAMS_BASE, "Data/Pressure"),
    output = file.path(TEAMS_BASE, "Data/Outputs"),
    local_temp = here::here("outputs")
  )
  
  # Opret mapper hvis de ikke eksisterer
  invisible(lapply(paths, function(p) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }))
  
  return(paths)
}

# Brug: paths <- set_project_paths()
