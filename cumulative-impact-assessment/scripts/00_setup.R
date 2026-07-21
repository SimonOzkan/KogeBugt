# Master setup script 
# source("scripts/00_setup.R")

# Install packages hvis de mangler
required_packages <- c("terra","dplyr","sf","ggplot2", "tidyr", "purrr", "readr","readxl","patchwork", "raster","tidyterra",
                       "lubridate", "stringr", "forcats", "scales","sf","gridExtra","grid","lattice","ggpubr","gt","writexl",
                       "ggspatial","httr")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
# Load packages
lapply(required_packages, library, character.only = TRUE)
# Load paths configuration
source(here::here("config/paths.R"))
PATHS <- set_project_paths()

cat("Setup complete. Use PATHS object for file paths.\n")



