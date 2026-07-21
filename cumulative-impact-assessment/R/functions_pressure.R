# ====================================================================
# PRESSURE FACTORS - Genbrugelige funktioner
# ====================================================================
# Funktioner til behandling af påvirkningsfaktorer
# (Fiskeri, skibstrafik, forurening osv.)

#' Process fishing pressure data
#'
#' @param fishing_data Data frame med fiskeridata
#' @param gear_type Type af redskab (f.eks. "trawl", "line")
#'
#' @return Data frame med processeret fiskeridata
#'
#' @examples
#' # process_fishing_pressure(fishing_df, gear_type = "trawl")
#'
process_fishing_pressure <- function(fishing_data, gear_type = NULL) {
  # Placeholder for fiskeridata processing
  cat("Processing fishing pressure data", 
      if (!is.null(gear_type)) paste("for gear type:", gear_type), "\n")
  return(fishing_data)
}


#' Process shipping traffic data
#'
#' @param shipping_data Data frame med skibstrafik information
#' @param buffer_distance Buffer afstand omkring skibsruter (meters)
#'
#' @return Data frame med processeret skibstrafik
#'
process_shipping_traffic <- function(shipping_data, buffer_distance = 1000) {
  # Placeholder for skibstrafik processing
  cat("Processing shipping traffic with buffer:", buffer_distance, "m\n")
  return(shipping_data)
}


#' Calculate pressure intensity
#'
#' @param pressure_data Data frame med påvirkningsdata
#'
#' @return Data frame med beregnede intensitetsværdier
#'
calculate_pressure_intensity <- function(pressure_data) {
  # Placeholder for intensitetsberegning
  cat("Calculating pressure intensity\n")
  return(pressure_data)
}


#' Normalize pressure values to 0-1 scale
#'
#' @param values Numerisk vektor
#'
#' @return Normaliseret vektor (0-1)
#'
normalize_pressure <- function(values) {
  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)
  normalized <- (values - min_val) / (max_val - min_val)
  return(normalized)
}


# ====================================================================
# NOTES:
# ====================================================================
# - Tilføj din egen logik til disse funktioner
# - Kald disse funktioner fra scripts/02_pressure_factors/*.R
# - Normalisering bruges senere i cumulative analysis
# ====================================================================
