
## ============================================================
## ICES + EMODnet -> fælles CHASE-skema (vand, sediment, biota)
## Output: ices_vand, ices_sed, ices_bio
## Kræver fra motor: til_ugl(), til_ugkg(), klass_biota()
## ============================================================
source("scripts/00_setup.R")
PATHS <- set_project_paths()


CAS_ICES<- read.table(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","RECO_Export_10-48-2026-08-48-04.csv"),
                      sep=",",header=T,comment.char="",fileEncoding="UTF-8",quote='"')

PARAM_ICES<- read.table(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","ICES_PARAM_names.csv"),
                        sep=",",header=T,comment.char="",fileEncoding="UTF-8",quote='"')

# Ingen kemi data for vand i ICES eller EMODnet data
water_cleaned <- read.csv(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","water_cleaned_koge.csv"))
sed_cleaned   <- read.csv(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","sediment_cleaned.csv"))
biota_cleaned <- read.csv(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","biota_cleaned.csv"))

## --- ICES-parameterkode -> CAS (sediment har ingen cas) -------------
## CAS for gruppemedlemmer matcher Group_table (PCB/PBDE/organotin)
ices_param_cas <- tribble(
  ~param,  ~cas,
  # metaller
  "as","7440-38-2","cd","7440-43-9","cr","7440-47-3","cu","7440-50-8",
  "hg","7439-97-6","ni","7440-02-0","pb","7439-92-1","zn","7440-66-6","li","7439-93-2",
  # PAH
  "ant","120-12-7","pa","85-01-8","flu","206-44-0","pyr","129-00-0",
  "baa","56-55-3","chr","218-01-9","bbf","205-99-2","bkf","207-08-9",
  "bap","50-32-8","bep","192-97-2","bghip","191-24-2","icdp","193-39-5","dbaha","53-70-3",
  # PCB (cb-koder -> kongener-CAS)
  "cb18","37680-65-2","cb28","7012-37-5","cb31","16606-02-3","cb44","41464-39-5",
  "cb52","35693-99-3","cb101","37680-73-2","cb105","32598-14-4","cb118","31508-00-6",
  "cb138","35065-28-2","cb153","35065-27-1","cb170","35065-30-6","cb180","35065-29-3",
  "cb187","52663-68-0",
  # PBDE (bd/bde-koder -> Group_table sbde6)
  "bde28","41318-75-6","bde47","5436-43-1","bde66","189084-61-5","bde85","182346-21-0",
  "bde99","32534-81-9","bd100","189084-64-8","bd153","68631-49-2","bd154","207122-15-4",
  "bd183","207122-16-5","bd209","1163-19-5",
  # organotin + HCB
  "tbsn+","36643-28-4","dbsn+","14488-53-0","mbsn+","78763-54-9","tpsn+","668-34-8",
  "hcb","118-74-1")

## --- hjælp: tilføj under_loq=FALSE hvis kolonnen mangler -------------

## --- VAND ingen data-------------------------------
# ices_vand <- water_cleaned %>%
#   mutate(value_u = til_ugl(value, munit)) %>%
#   filter(!is.na(value_u), !is.na(cas)) %>%
#   transmute(source, station = as.character(station), date = as.Date(date),
#             lon = longitude, lat = latitude, cas,
#             value_u, under_loq, limit_u = NA_real_,
#             organ = NA_character_, biota_type = NA_character_)

## --- SEDIMENT (µg/kg tv; cas mappes fra param) ----------------------
ices_sed <- sed_cleaned %>%
  mutate(param = tolower(param)) %>%
  left_join(PARAM_ICES, by = c("param" = )) 



  mutate(cas = coalesce(cas, cas.map),
         value_u = til_ugkg(value, munit)) %>%
  filter(!is.na(value_u), !is.na(cas)) %>%
  transmute(source, station = as.character(station), date = as.Date(date),
            lon = longitude, lat = latitude, cas,
            value_u, under_loq, limit_u = NA_real_,
            organ = NA_character_, biota_type = NA_character_)

## --- BIOTA (µg/kg vv; cas + species findes) -------------------------
ices_bio <- biota_cleaned %>% sikr_uloq() %>%
  filter(basis == "W") %>%                                   # vådvægt (EQS-basis)
  mutate(value_u = til_ugkg(value, munit),
         biota_type = klass_biota(coalesce(species, type))) %>%
  filter(!is.na(value_u), !is.na(cas)) %>%
  transmute(source, station = as.character(station), date = as.Date(date),
            lon = longitude, lat = latitude, cas,
            value_u, under_loq, limit_u = NA_real_,
            organ = matrx, biota_type)

## --- tjek: sediment-koder uden CAS-match (bør kun være fysiske) -----
sed_cleaned %>% mutate(param = tolower(param)) %>%
  anti_join(ices_param_cas, by = "param") %>%
  filter(is.na(cas)) %>% distinct(pargroup, param) %>% print(n = Inf)