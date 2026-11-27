##### sensitivity scores ##########

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

list_ecocomp <- read_csv(file.path(PATHS$input_sensitivity,"list_ecocomp.csv"))

list_press <- read_csv(file.path(PATHS$input_sensitivity,"list_stressors.csv"))

SS_all_090120 <-read_csv(file.path(PATHS$input_sensitivity,"SS_all_090120.csv"))








SS_all_090120_T <- as.data.frame(t(SS_all_090120[,-1]))
colnames(SS_all_090120_T) <- n
 
SS_all_090120_T$myfactor <- factor(row.names(SS_all_090120_T)) 
 
SS_T <- SS_all_090120_T %>%
  relocate("myfactor", .before = polnut_din_rescaled )

names_eco <- SS_T$myfactor
names_pressures <- SS_T[0,]
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

ecocomb_name_pelagisk <- list_ecocomp %>%
  filter(ValueField %in%  c("chla","oxygen_merge"))

pelagisk_hab <- list.files(file.path(PATHS$output_ecosystem_tif, "pelagisk_hab")) 

ecocomb_name_pelagic <- ecocomb_name_pelagisk %>%
  mutate(new_names = case_when(
    ValueField == "chla"            ~ "klfa_log_norm",
    ValueField == "oxygen_merge"           ~ "iltsvind",
    TRUE                                ~ NA_character_
  ))

#-------------------------- VIGTIGE HABITATER -----------------------------------# 

list_ecocomp_check <- list_ecocomp %>%
  filter(!ResName %in% ecocomb_name_hab$ResName,
         !ValueField %in%  c("chla","oxygen_merge"))
ecocomb_name_vigtige_hab <- list_ecocomp %>%
  filter(ValueField %in% c("StenRev", "eelgras_ECOMAR", "recreational_use"))

vigtige_hab <- list.files(file.path(PATHS$output_ecosystem_tif, "vigtige_habs"),
                          pattern = "\\.tif$")

# 1) Tilføj new_names til de eksisterende rækker
ecocomb_name_vigtige_hab <- ecocomb_name_vigtige_hab %>%
  mutate(new_names = case_when(
    ValueField == "StenRev"          ~ "rev",
    ValueField == "eelgras_ECOMAR"   ~ "Zostera",
    ValueField == "recreational_use" ~ "vigtig_habs_rast",
    TRUE                             ~ NA_character_
  ))

# 2) Tilføj nye rækker for filer uden match
ecocomb_name_vigtige_hab <- ecocomb_name_vigtige_hab %>%
  bind_rows(
    tibble(new_names = str_remove(vigtige_hab, "\\.tif$")) %>%
      anti_join(ecocomb_name_vigtige_hab, by = "new_names")
  )

# 3) Udfyld ResName og ValueField for de nye rækker
new_meta <- tribble(
  ~new_names,                           ~ResName_new, ~ValueField_new,
  "Mytilus_edulis_aquamaps_downscaled", "mytilus",    "mytilus",
  "saltmarskes",                        "saltmarsk",  "saltmarsk"
)

ecocomb_name_vigtige_hab <- ecocomb_name_vigtige_hab %>%
  left_join(new_meta, by = "new_names") %>%
  mutate(ResName    = coalesce(ResName, ResName_new),
         ValueField = coalesce(ValueField, ValueField_new)) %>%
  select(-ResName_new, -ValueField_new)

#------------- HAVPATTEDYR -------------------------------#

pattedyr <- list.files(file.path(PATHS$output_ecosystem_tif, "pattedyr"),
                          pattern = "\\.tif$")

ecocomb_name_pattedyr <- list_ecocomp %>%
  filter(ValueField %in% c("ECOMAR_GreySeal_final", "ECOMAR_HarbourSeal_final", "ECOMAR_Porpoise_final"))
ecocomb_name_pattedyr <- ecocomb_name_pattedyr %>%
  mutate(new_names = case_when(
    ValueField == "ECOMAR_GreySeal_final"          ~ "Halichoerus_grypus_aquamaps_downscaled",
    ValueField == "ECOMAR_HarbourSeal_final"   ~ "spaettet_sael",
    ValueField == "ECOMAR_Porpoise_final" ~ "marsvin",
    TRUE                             ~ NA_character_
  ))



#------------ FISK --------------------------------- #
fisk <- list.files(file.path(PATHS$output_ecosystem_tif, "fisk"),
                   pattern = "\\.tif$")


ecocomb_name_fisk <- list_ecocomp %>%
  filter(!str_detect(ValueField, "crayfish"),
         str_detect(ValueField,"fish_") |
         ValueField %in% c("haddok_CPUE_tot","hake_CPUE_tot", "herring_CPUE_tot","plaice_CPUE_tot",
                           "saithe_CPUE_tot","sole_CPUE_tot","sprat_CPUE_tot", 
                           "turbot_CPUE_tot",  "cod_CPUE_tot")) 


# 1) Tilføj new_names til de eksisterende rækker
ecocomb_name_fisk <- ecocomb_name_fisk %>%
  mutate(new_names = case_when(
    ValueField == "herring_CPUE_tot"      ~ "Clupea_harengus_aquamaps_downscaled",
    ValueField == "plaice_CPUE_tot"       ~ "Pleuronectes_platessa_aquamaps_downscaled",
    ValueField == "saithe_CPUE_tot"       ~ "Pollachius_virens_aquamaps_downscaled",
    ValueField == "sprat_CPUE_tot"        ~ "Sprattus_sprattus_aquamaps_downscaled",
    ValueField == "turbot_CPUE_tot"       ~ "Scophthalmus_maximus_aquamaps_downscaled",
    ValueField == "cod_CPUE_tot"          ~ "Gadus_morhua_aquamaps_downscaled",
    ValueField == "cfs_pel_fish_mackerel" ~ "Scomber_scombrus_aquamaps_downscaled",
    TRUE                                  ~ NA_character_
  ))

# 2) Tilføj nye rækker for filer uden match (dropper _fine-versionen af skrubbe)
ecocomb_name_fisk <- ecocomb_name_fisk %>%
  bind_rows(
    tibble(new_names = str_remove(fisk, "\\.tif$")) %>%
      filter(!str_detect(new_names, "_fine$")) %>%
      anti_join(ecocomb_name_fisk, by = "new_names")
  )

# 3) Udfyld ResName og ValueField for de nye rækker
new_meta <- tribble(
  ~new_names,                                   ~ResName_new,                        ~ValueField_new,
  "Anguilla_anguilla_aquamaps_downscaled",      "fish_eel_rescaled",                 "fish_eel",
  "Belone_belone_aquamaps_downscaled",          "fish_garfish_rescaled",             "fish_garfish",
  "Limanda_limanda_aquamaps_downscaled",        "fish_dab_rescaled",                 "fish_dab",
  "Myoxocephalus_scorpius_aquamaps_downscaled", "fish_shorthornsculpin_rescaled",    "fish_shorthornsculpin",
  "Phocoena_phocoena_aquamaps_downscaled",      "mammal_harbourporpoise_rescaled",   "mammal_harbourporpoise",
  "Platichthys_flesus_aquamaps_downscaled",     "fish_flounder_rescaled",            "fish_flounder",
  "Pomatoschistus_minutus_aquamaps_downscaled", "fish_sandgoby_rescaled",            "fish_sandgoby",
  "Salmo_salar_aquamaps_downscaled",            "fish_salmon_rescaled",              "fish_salmon",
  "Zoarces_viviparus_aquamaps_downscaled",      "fish_eelpout_rescaled",             "fish_eelpout"
)

ecocomb_name_fisk <- ecocomb_name_fisk %>%
  left_join(new_meta, by = "new_names") %>%
  mutate(ResName    = coalesce(ResName, ResName_new),
         ValueField = coalesce(ValueField, ValueField_new)) %>%
  select(-ResName_new, -ValueField_new)

#----------- FUGLE -----------------------------#

fugle <- list.files(file.path(PATHS$output_ecosystem_tif, "fugle"),
                    pattern = "\\.tif$")

ecocomb_name_fugle <- list_ecocomp %>%
  filter(str_detect(ValueField,"bird") |
           ValueField %in% c("eider_avg","alcid_avg", "scoter_avg")) 

# 1) Tilføj new_names til de eksisterende rækker
ecocomb_name_fugle <- ecocomb_name_fugle %>%
  mutate(new_names = case_when(
    ValueField == "birds_diver2006"      ~ "fugl_gavia_arctica",
    ValueField == "birds_merganeser2008" ~ "fugl_mergus_serrator",
    ValueField == "birds_ltd"            ~ "fugl_clangula_hyemalis",
    ValueField == "eider_avg"            ~ "fugl_somateria_mollissima",
    ValueField == "alcid_avg"            ~ "fugl_alca_torda",
    ValueField == "scoter_avg"           ~ "fugl_melanitta_nigra",
    TRUE                                 ~ NA_character_
  ))

# 2) Tilføj nye rækker for filer uden match
ecocomb_name_fugle <- ecocomb_name_fugle %>%
  bind_rows(
    tibble(new_names = str_remove(fugle, "\\.tif$")) %>%
      anti_join(ecocomb_name_fugle, by = "new_names")
  )

# 3) Udfyld ResName og ValueField for de nye rækker
new_meta <- tribble(
  ~new_names,                     ~ResName_new,          ~ValueField_new,
  "fugle_samlet_gennemsnit_norm", "birds_mean_rescaled", "birds_mean"
)

ecocomb_name_fugle <- ecocomb_name_fugle %>%
  left_join(new_meta, by = "new_names") %>%
  mutate(ResName    = coalesce(ResName, ResName_new),
         ValueField = coalesce(ValueField, ValueField_new)) %>%
  select(-ResName_new, -ValueField_new)

#---------- FORTID OG ARKÆOLOGI -----------------#
for_arch <- list.files(file.path(PATHS$output_ecosystem_tif, "fortid"),
                       pattern = "\\.tif$")

ecocomb_name_for_arch <- list_ecocomp %>%
  filter(ValueField %in% c("Archaelogical_sites  ","VragNyTid")) 


# 1) Tilføj new_names til de eksisterende rækker
ecocomb_name_for_arch <- ecocomb_name_fugle %>%
  mutate(new_names = case_when(
    ValueField == "Archaelogical_sites"      ~ "fortids",
    ValueField == "VragNyTid" ~ "skibsvrag",
    TRUE                                 ~ NA_character_
  ))

#----------- BADEVAND ------------------------------------#
kyst_badevand <- list.files(file.path(PATHS$output_ecosystem_tif, "kyst_badevand"),
                            pattern = "\\.tif$")

ecocomb_name_bade <- list_ecocomp %>%
  filter(ValueField == "bathing_sites") 

ecocomb_name_bade<- ecocomb_name_bade %>%
  mutate(new_names = case_when(
    ValueField == "bathing_sites" ~ "badevand",
    TRUE                                 ~ NA_character_
  ))



##
tst <- list_ecocomp %>%
  filter(!ValueField %in% ecocomb_name_hab$ValueField,
         !ValueField %in% ecocomb_name_pelagic$ValueField,
         !ValueField %in% ecocomb_name_vigtige_hab$ValueField,
         !ValueField %in% ecocomb_name_pattedyr$ValueField,
         !ValueField %in% ecocomb_name_fisk$ValueField,
         !ValueField %in% ecocomb_name_fugle$ValueField
         )



