# Definerer stier til Teams-mappen og lokale mapper
# Brug: paths <- set_project_paths()

set_project_paths <- function() {
  
  # Teams/SharePoint-roden. Portabel: bygges fra den enkelte brugers USERPROFILE,
  # Kan overskrives med miljøvariablen KOGEBUGT_TEAMS_BASE, hvis dit Teams-
  # bibliotek ligger et andet sted (fx under "OneDrive - NIVA" eller på Mac).
  TEAMS_BASE <- Sys.getenv(
    "KOGEBUGT_TEAMS_BASE",
    unset = file.path(Sys.getenv("USERPROFILE"), "NIVA", "Køge Bugt - General")
  )
  # Mac: sæt i ~/.Renviron, fx
  #   KOGEBUGT_TEAMS_BASE=/Users/<navn>/Library/CloudStorage/.../Køge Bugt - General
  
  paths <- list(
    # Input data fra Teams
    input_ecosystem        = file.path(TEAMS_BASE, "Data/Ecosystem"),
    input_pressure         = file.path(TEAMS_BASE, "Data/Pressure"),
    input_assessment_area  = file.path(TEAMS_BASE, "Data/Assessment_Area"),
    input_phys_chem_geo    = file.path(TEAMS_BASE, "Data/phys_chem_geo data"),
    
    # Output data til Teams
    output_ecosystem_tif   = file.path(TEAMS_BASE, "Data/Outputs/EC_tif"),
    output_pressure_tif    = file.path(TEAMS_BASE, "Data/Outputs/Pressure_tif"),
    output_ecosystem_png   = file.path(TEAMS_BASE, "Data/Outputs/EC_png"),
    output_pressure_png    = file.path(TEAMS_BASE, "Data/Outputs/Pressure_png"),
    
    # Lokale temp-files (i det lokale repo, ikke Teams)
    local_temp             = here::here("outputs")
  )
  
  # Teams skal være synkroniseret lokalt, ellers kan intet resolve
  if (!dir.exists(TEAMS_BASE)) {
    warning("TEAMS_BASE findes ikke: ", TEAMS_BASE,
            "\nEr Teams-mappen synkroniseret til denne pc? ",
            "Sæt evt. KOGEBUGT_TEAMS_BASE i ~/.Renviron.")
  }
  
  # Opret KUN output- og temp-mapper. Input-mapper oprettes BEVIDST ikke:
  # mangler en input-mappe, vil vi have en ærlig fejl - ikke en tom, falsk mappe.
  create_dirs <- c(
    paths$output_ecosystem_tif, paths$output_pressure_tif,
    paths$output_ecosystem_png, paths$output_pressure_png,
    paths$local_temp
  )
  invisible(lapply(create_dirs, function(p) {
    if (!dir.exists(p)) {
      dir.create(p, recursive = TRUE, showWarnings = FALSE)
      cat("Oprettet mappe:", p, "\n")
    }
  }))
  
  # .gitkeep så den lokale outputs/-mappe bevares i Git
  gitkeep <- here::here("outputs/.gitkeep")
  if (!file.exists(gitkeep)) file.create(gitkeep)
  
  return(paths)
}
