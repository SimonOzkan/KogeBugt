##### sensitivity scores ##########

#################### Sensitivitetsmatrix: ØKOMAR -> eget projekt ####################
source("scripts/00_setup.R")
PATHS <- set_project_paths()
library(openxlsx)

## 1. ØKOMAR i langt format (rækker = pres, kolonner = økosystem) ----------------
ss_long <- read_csv(file.path(PATHS$input_sensitivity, "SS_all_090120.csv"),
                    show_col_types = FALSE) %>%
  rename(okomar_p = 1) %>%
  pivot_longer(-okomar_p, names_to = "okomar_e", values_to = "score")

range(ss_long$score, na.rm = TRUE)  

ss_long <- ss_long %>%
  mutate(score = score /2) # skaleret til 0-1, så værdier fra Andys CEA kan anvendes
## 2. Kobling af presfaktorer ----------------------------------------------------
p_map <- tribble(
  ~P_ID,    ~P_navn,                                          ~okomar_p,                              ~p_type,
  "P01_01", "Marint affald",                                  "polchem_marine_litter_merge_rescaled", "direkte",
  "P02_01", "Fuglejagt",                                      "selex_bird_hunting_rescaled",          "direkte",
  "P03_01", "Råstofindvinding",                               "iei_excavation_prod_1000_rescaled",    "direkte",
  "P03_02", "Lysforurening",                                  NA,                                     NA,
  "P04_01", "Broer",                                          "iei_bridges_1000_rescaled",            "direkte",
  "P04_02", "Havvindmølleparker",                             "iei_wp_plants_1000_rescaled",          "direkte",
  "P04_03", "Bypass",                                         "iei_dredging_5000_rescaled",           "proxy",
  "P04_04", "Kabler",                                         "iei_cables_rescaled",                  "direkte",
  "P04_05", "Gasrør",                                         "iei_pipelines_rescaled",               "direkte",
  "P04_06", "Fyrtårne",                                       "iei_lighthouses_rescaled",             "direkte",
  "P04_07", "Holmene",                                        "iei_coastal_defence_1000_rescaled",    "proxy",
  "P04_08", "Klapning",                                       "iei_dumpsites_5000_rescaled",          "direkte",
  "P04_09", "Kystbeskyttelse",                                "iei_coastal_defence_1000_rescaled",    "direkte",
  "P04_10", "Militære øvelsesområder",                        "iei_military_areas_7500_rescaled",     "direkte",
  "P06_01", "Kemisk forurening",                              "polchem_contaminants_rescaled",        "direkte",
  "P06_02", "Kontek elektrodeanlæg",                          "polchem_contaminants_rescaled",        "proxy",
  "P06_03", "Oliespild",                                      "polchem_oilspills_rescaled",           "direkte",
  "P07_01", "Kontinuerlig støj",                              "noise_continuous_rescaled",            "direkte",
  "P07_02", "Impulsiv støj",                                  "noise_bang_days_rescaled",             "direkte",
  "P07_03", "Støj fra havvindmølleparker",                    "noise_continuous_rescaled",            "proxy",
  "P07_04", "Tilførelse af varme",                            "iei_powerplants_1000_rescaled",        "proxy",
  "P08_01", "Ikke-hjemmehørende arter",                       "nis_ralaha_rescaled",                  "direkte",
  "P09_01", "Vinter DIN",                                     "polnut_din_rescaled",                  "direkte",
  "P09_02", "Vinter DIP",                                     "polnut_dip_rescaled",                  "direkte",
  "P10_01", "Rekreativ skibstrafik (AIS)",                    "recrea_boating_rescaled",              "direkte",
  "P10_02", "Områder vigtige for fritidsaktiviteter og turisme", "recrea_coastal_rescaled",           "direkte",
  "P10_03", "Fritidsdykning",                                 "recrea_diving_rescaled",               "direkte",
  "P12_01", "Skibstrafik industri og fiskeri (AIS)",          "sat_shipping_rescaled",                "direkte",
  "P12_02", "Havne - industri",                               "sat_ports_5000_rescaled",              "direkte",
  "P12_03", "Havne - rekreativ sejlads",                      "sat_harbours_3000_rescaled",           "direkte",
  "P13_01", "Temperatur - overflade",                                NA,                                 NA,
  "P13_02", "Temperatur - bund",                                     NA,                                 NA,
  "P13_03", "Forsuring - overflade",                                 NA,                                 NA,
  "P13_04", "Forsuring - bund",                                      NA,                                 NA
)


## 3. Kobling af økosystemkomponenter --------------------------------------------
e_map <- tribble(
  ~E_ID,    ~E_navn,                                   ~okomar_e,                                ~e_type,
  "P01_1",  "Klorofyl a",                              "hab_prod_surf_waters_rescaled",          "direkte",
  "P01_2",  "Iltsvind",                                "hab_oxygen_merged_rescaled",             "direkte",
  "P02_1",  "Infralittoral sand",                      "hab_infrasand_rescaled",                 "direkte",
  "P02_2",  "Infralittoral groft sediment",            "hab_infracoarsesed_rescaled",            "direkte",
  "P02_3",  "Infralittoral blandet sediment",          "hab_inframixedsed_rescaled",             "direkte",
  "P02_4",  "Infralittoral mudder",                    "hab_inframud_rescaled",                  "direkte",
  "P02_5",  "Infralittoral sten og biogene rev",       "hab_infrarxbiogenreef_rescaled",         "direkte",
  "P02_6",  "Circalittoral sand",                      "hab_circasand_merge_rescaled",           "direkte",
  "P02_7",  "Circalittoral groft sediment",            "hab_circacoarsesediment_merge_rescaled", "direkte",
  "P02_8",  "Circalittoral blandet sediment",          "hab_circamixedsediment_merge_rescaled",  "direkte",
  "P02_9",  "Circalittoral mudder",                    "hab_circamud_merge_rescaled",            "direkte",
  "P02_10", "Offshore circalittoral sand",             "hab_circasand_merge_rescaled",           "proxy",
  "P02_11", "Offshore circalittoral groft sediment",   "hab_circacoarsesediment_merge_rescaled", "proxy",
  "P02_12", "Offshore circalittoral blandet sediment", "hab_circamixedsediment_merge_rescaled",  "proxy",
  "P02_13", "Offshore circalittoral mud",              "hab_circamud_merge_rescaled",            "proxy",
  "P03_1",  "Rev",                                     "hab_stonereefs_rescaled",                "direkte",
  "P03_2",  "Blåmuslinger (Mytilus edulis)",           NA,                                       NA,
  "P03_3",  "Ålegræs (Zostera marina)",                "hab_eelgras_rescaled",                   "direkte",
  "P03_4",  "Saltmarsk",                                NA,                                       NA,
  "P04_1",  "Habitater egnet for marsvin",             "mammal_porpoise_rescaled",               "direkte",
  "P04_2",  "Gråsæl",                                  "mammal_greyseal_rescaled",               "direkte",
  "P04_3",  "Spættet sæl",                             "mammal_harbourseal_rescaled",            "direkte",
  "P05_1",  "Ål",                                      NA,                                       NA,
  "P05_2",  "Sild",                                    "fish_Herring_CPUE_n_rescaled",           "direkte",
  "P05_3",  "Makrel",                                  "cfs_pel_fish_mackerel_rescaled",         "direkte",
  "P05_4",  "Sej",                                     "fish_Saithe_presence25_rescaled",        "direkte",
  "P05_5",  "Brisling",                                "fish_Sprat_CPUE14_n_rescaled",           "direkte",
  "P05_6",  "Hornfisk",                                NA,                                       NA,
  "P05_7",  "Laks",                                    NA,                                       NA,
  "P05_8",  "Skrubbe",                                 "fish_plaice_CPUE_n_rescaled",            "proxy",
  "P05_9",  "Pighvar",                                 "fish_turbot_precence_rescaled",          "direkte",
  "P05_10", "Rødspætte",                               "fish_plaice_CPUE_n_rescaled",            "direkte",
  "P05_11", "Sandkutling",                             NA,                                       NA,
  "P05_12", "Torsk",                                   "fish_cod_CPUE_n_rescaled",               "direkte",
  "P05_13", "Ålekvabbe",                               NA,                                       NA,
  "P05_14", "Ulk",                                     NA,                                       NA,
  "P05_15", "Ising",                                   "fish_plaice_CPUE_n_rescaled",            "proxy",
  "P06_1",  "Alkefugle",                               "birds_alcid_rescaled",                   "direkte",
  "P06_2",  "Sortand",                                 "birds_scoter_rescaled",                  "direkte",
  "P06_3",  "Ederfugl",                                "birds_eider2008_rescaled",               "direkte",
  "P06_4",  "Mallemuk",                                "birds_fulmar_rescaled",                  "direkte",
  "P06_5",  "Toppet skallesluger",                     "birds_merganeser2008_rescaled",          "direkte",
  "P06_6",  "Rødstrubet/Sortstrubet lom",              "birds_diver2006_rescaled",               "direkte",
  "P06_7",  "Havlit",                                  "birds_ltd_rescaled",                     "direkte",
  "P07_1",  "Badepladser",                             "bathing_sites",                          "direkte",
  "P07_2",  "Arkæologiske steder, fund og vrag",       "archaelogical_sites",                    "direkte",
  "P07_3",  "Skibsvrag",                               "ship_wreck",                             "direkte"
)


## 4. Byg matricen ---------------------------------------------------------------
matrix_long <- expand_grid(p_map, e_map) %>%            # bevarer rækkefølgen
  left_join(ss_long, by = c("okomar_p", "okomar_e")) %>%
  mutate(kilde = case_when(
    is.na(score)                             ~ "Mangler",
    p_type == "direkte" & e_type == "direkte" ~ "ØKOMAR",
    TRUE                                     ~ "ØKOMAR (proxy)"
  ))

score_wide <- matrix_long %>%
  select(P_ID, P_navn, E_navn, score) %>%
  pivot_wider(names_from = E_navn, values_from = score)

kilde_wide <- matrix_long %>%
  select(P_ID, P_navn, E_navn, kilde) %>%
  pivot_wider(names_from = E_navn, values_from = kilde)

count(matrix_long, kilde)   # overblik: hvor mange celler mangler?


## 5. Skriv Excel med farvekoder -------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Sensitivitet")
writeData(wb, "Sensitivitet", score_wide)

stil_mangler <- createStyle(fgFill = "#FFF2CC", border = "TopBottomLeftRight", borderColour = "#BFBFBF")
stil_proxy   <- createStyle(fgFill = "#F8CBAD", border = "TopBottomLeftRight", borderColour = "#BFBFBF")

k <- as.matrix(kilde_wide[, -(1:2)])
for (s in list(list("Mangler", stil_mangler), list("ØKOMAR (proxy)", stil_proxy))) {
  idx <- which(k == s[[1]], arr.ind = TRUE)
  if (nrow(idx) > 0) {
    addStyle(wb, "Sensitivitet", s[[2]], rows = idx[, 1] + 1, cols = idx[, 2] + 2)
  }
}

addStyle(wb, "Sensitivitet", createStyle(textRotation = 90, textDecoration = "bold", valign = "bottom"),
         rows = 1, cols = 3:ncol(score_wide), gridExpand = TRUE)
setColWidths(wb, "Sensitivitet", cols = 2, widths = 45)
setColWidths(wb, "Sensitivitet", cols = 3:ncol(score_wide), widths = 5)
freezePane(wb, "Sensitivitet", firstRow = TRUE, firstCol = FALSE, firstActiveCol = 3)

addWorksheet(wb, "Kilde");          writeData(wb, "Kilde", kilde_wide)
addWorksheet(wb, "Kobling_pres");   writeData(wb, "Kobling_pres", p_map)
addWorksheet(wb, "Kobling_økosystem"); writeData(wb, "Kobling_økosystem", e_map)

addWorksheet(wb, "Forklaring")
writeData(wb, "Forklaring", c(
  "Sensitivitetsmatrix: rækker = presfaktorer, kolonner = økosystemkomponenter.",
  "Ingen farve = værdi direkte fra ØKOMAR (SS_all_090120).",
  "Orange = proxy: værdi lånt fra en lignende ØKOMAR-komponent eller -pres (se Kobling-arkene). Gennemgå og ret ved behov.",
  "Gul = mangler i ØKOMAR og skal udfyldes. Brug samme skala som ØKOMAR.",
  "Arket 'Kilde' viser status for hver celle."
))

saveWorkbook(wb, file.path(PATHS$input_sensitivity, "sensitivitet_koge_bugt_not_filled.xlsx"), overwrite = TRUE)

