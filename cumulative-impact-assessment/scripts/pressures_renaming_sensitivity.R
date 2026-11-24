##### sensitivity scores ##########

# Indlæs pakker og set path fra source setup fil
source("scripts/00_setup.R")

# Hent tilgængelige project paths
PATHS <- set_project_paths()

list_ecocomp <- read_csv(file.path(PATHS$input_sensitivity,"list_ecocomp.csv"))

list_press <- read_csv(file.path(PATHS$input_sensitivity,"list_stressors.csv"))

SS_all_090120 <-read_csv(file.path(PATHS$input_sensitivity,"SS_all_090120.csv"))


##### Renaming af alle presfaktorer, så navnene matcher sensitivity scores ##########

# nutrient 

nutrients <- list.files(file.path(PATHS$output_pressure_tif, "nutrients")) 

list_press_nut <- list_press %>%
  filter(ValueField %in% c("DIN","DIP"))


ecocomb_name_hab <- ecocomb_name_hab %>%
  mutate(new_names = case_when(
    ValueField == "DIN" ~ "vinter_DIN_kernel_norm",
    ValueField == "DIP"   ~ "vinter_DIP_kernel_norm",
    TRUE                                ~ NA_character_
  ))


### 

iei <- list.files(file.path(PATHS$output_pressure_tif, "anlaeg")) 
kabler <- list.files(file.path(PATHS$output_pressure_tif, "kabler"))


list_press_iei <- list_press %>%
  filter(grepl("iei",File ))


# 1) Tilføj new_names til de eksisterende rækker
list_press_iei <- list_press_iei %>%
  mutate(new_names = case_when(
    ResName == "iei_cables_rescaled"        ~   "kabler",
    ResName == "iei_pipelines_rescaled"  ~   "gasrør",
    
    ResName == "iei_bridges_1000_rescaled"   ~ "broer_buffer",
    
    ResName == "iei_bridges_1000_rescaled"   ~ "broer_buffer",
    ResName == "iei_lighthouses_rescaled"    ~ "fyrtaarne_pa",
    ResName == "iei_dumpsites_5000_rescaled" ~ "klapning_norm",
    ResName == "iei_wp_plants_1000_rescaled" ~ "renseanlaeg_buffer",
    
    TRUE                                     ~ NA_character_
  ))

# 2) Tilføj nye rækker for filer uden match
list_press_nut <- list_press_nut %>%
  bind_rows(
    tibble(new_names = str_remove(iei, "\\.tif$")) %>%
      anti_join(list_press_nut, by = "new_names")
  )

# 3) Udfyld ResName og ValueField for de nye rækker
new_meta <- tribble(
  ~new_names,                            ~ResName_new,                         ~ValueField_new,
  "bypass",                              "iei_bypass_rescaled",                "bypass",
  "bypass_frac",                         "iei_bypass_frac_rescaled",           "bypass_frac",
  "fyrtaarne_buffer",                    "iei_lighthouses_buffer_rescaled",    "lighthouses_buffer",
  "havne_industri_norm",                 "iei_harbours_industry_rescaled",     "harbours_industry",
  "havne_lystbaad_frak",                 "iei_harbours_marina_rescaled",       "harbours_marina",
  "Holmene_frac",                        "iei_holmene_rescaled",               "holmene",
  "klapning",                            "iei_dumpsites_raw_rescaled",         "dumpsites_raw",
  "klapning_log_norm",                   "iei_dumpsites_log_rescaled",         "dumpsites_log",
  "renseanlaeg_pa",                      "iei_wp_plants_pa_rescaled",          "wp_plants_pa",
  "vindmoelleparker_approved_frac",      "iei_windfarms_approved_rescaled",    "windfarms_approved",
  "vindmoelleparker_planned_frac",       "iei_windfarms_planned_rescaled",     "windfarms_planned",
  "vindmoelleparker_production_frac",    "iei_windfarms_production_rescaled",  "windfarms_production"
)

list_press_nut <- list_press_nut %>%
  left_join(new_meta, by = "new_names") %>%
  mutate(ResName    = coalesce(ResName, ResName_new),
         ValueField = coalesce(ValueField, ValueField_new)) %>%
  select(-ResName_new, -ValueField_new)

print(list_press_nut, n = Inf)