##### sensitivity scores ##########
#---------------------- Bentiske habitater (EUSeaMap) -----------------

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()
list_ecocomp <- read_csv("C:/Users/SIO/NIVA/Køge Bugt - General/data/sensitivity/list_ecocomp.csv")

list_press <- read_csv("C:/Users/SIO/NIVA/Køge Bugt - General/data/sensitivity/list_stressors.csv")

SS_all_090120 <- read_csv("C:/Users/SIO/NIVA/Køge Bugt - General/data/sensitivity/SS_all_090120.csv")


##### Renaming af alle Økosystemkomponenter, så navnene matcher sensitivity scores ##########



#---------------------------- BENTISKE HABITATER ------------------------------ #
ecocomb_name_hab <- list_ecocomp %>%
  filter(str_detect(ResName,"hab"),
         !ResName %in% c("hab_eelgras_rescaled","hab_prod_surf_waters_rescaled","hab_oxygen_merged_rescaled","hab_stonereefs_rescaled")) 


benth_habs <- list.files(file.path(PATHS$output_ecosystem_tif, "broad_scale_hab")) 


# 1) Tilføj new_names til de eksisterende rækker
ecocomb_name_hab <- ecocomb_name_hab %>%
  mutate(new_names = case_when(
    ValueField == "InfraCoarseSediment" ~ "Infralittoral_coarse_sediment",
    ValueField == "InfraRxBiogenReef"   ~ "Infralittoral_rock",
    ValueField == "InfraMixedSediment"  ~ "Infralittoral_mixed_sediment",
    ValueField == "InfraMud"            ~ "Infralittoral_mud",
    ValueField == "InfraSand"           ~ "Infralittoral_sand",
    ValueField == "CircaCoarseSediment" ~ "Circalittoral_coarse_sediment",
    ValueField == "CircaMixedSediment"  ~ "Circalittoral_mixed_sediment",
    ValueField == "CircaMud"            ~ "Circalittoral_mud",
    ValueField == "CircaSand"           ~ "Circalittoral_sand",
    TRUE                                ~ NA_character_
  ))

# 2) Tilføj nye rækker for filer uden match (File, ResName, ValueField = NA)
ecocomb_name_hab <- ecocomb_name_hab %>%
  bind_rows(
    tibble(new_names = str_remove(benth_habs, ".tif")) %>%
      anti_join(ecocomb_name_hab, by = "new_names")
  )

# 3) Udfyld ResName og ValueField for de nye rækker
new_meta <- tribble(
  ~new_names,                               ~ResName_new,                          ~ValueField_new,
  "Circalittoral_seabed",                   "hab_circaseabed_rescaled",            "CircaSeabed",
  "Infralittoral_biogenic_habitat",         "hab_infrabiogenhab_rescaled",         "InfraBiogenicHabitat",
  "Infralittoral_seabed",                   "hab_infraseabed_rescaled",            "InfraSeabed",
  "Offshore_circalittoral_coarse_sediment", "hab_offcircacoarsesediment_rescaled", "OffCircaCoarseSediment",
  "Offshore_circalittoral_mixed_sediment",  "hab_offcircamixedsediment_rescaled",  "OffCircaMixedSediment",
  "Offshore_circalittoral_mud",             "hab_offcircamud_rescaled",            "OffCircaMud",
  "Offshore_circalittoral_sand",            "hab_offcircasand_rescaled",           "OffCircaSand"
)

ecocomb_name_hab <- ecocomb_name_hab %>%
  left_join(new_meta, by = "new_names") %>%
  mutate(ResName    = coalesce(ResName, ResName_new),
         ValueField = coalesce(ValueField, ValueField_new)) %>%
  select(-ResName_new, -ValueField_new)




#---------------------------- PELAGISKE HABITATER ------------------------------ #





