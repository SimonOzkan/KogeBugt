## ============================================================
## SGU -> fælles CHASE-skema (vand, sediment, biota)
## Output: sgu_vand, sgu_sed, sgu_bio  (klar til bind_rows med ODA)
## Kræver: til_ugl(), til_ugkg(), add_lonlat(), klass_biota()
## ============================================================
sgu_dir <- "C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/kemisk_forurening/SGU/Utsök_DV_Miljögifter_2026-11-09 08_50"

vatten_raw    <- read_delim(file.path(sgu_dir,"Vatten.csv"),   delim=";", escape_double=FALSE, trim_ws=TRUE)
sgu_sed_raw   <- read_delim(file.path(sgu_dir,"Sediment.csv"), delim=";", escape_double=FALSE, trim_ws=TRUE)
sgu_biota_raw <- read_delim(file.path(sgu_dir,"Biota.csv"),    delim=";", escape_double=FALSE, trim_ws=TRUE)

tst_biota <- sgu_biota_raw %>%
  left_join(sgu_cas_lookup, by = "PARAMETERNAMN") %>%
  mutate(
    value_num = suppressWarnings(as.numeric(gsub("[<>]", "", MATVARDE))),
    under_loq = grepl("<", MATVARDE),
    value_u   = conv(value_num, ENHET),
    limit_u   = conv(coalesce(DETEKTIONSGRANS_LOD, RAPPORTERINGSGRANS_LOQ, value_num), ENHET),
    date      = as.Date(PROVTAG_DAT),
    ORGAN,
    station   = as.character(coalesce(as.character(STATION_ID), as.character(PROVPLATS_ID))),
    basis = basis_from_unit(ENHET)
  ) %>%
  filter(!is.na(value_u), !is.na(CAS)) %>%
  filter(!is.na(basis)) %>%
  add_lonlat("NATST_POSITION_E","NATST_POSITION_N", 3006) %>%
  transmute(source = "SGU", station, date, lon, lat, cas = CAS,param = PARAMETERNAMN,
            unit = paste0("ug/kg ", basis), ENHET,
            value_u, under_loq, limit_u, ORGAN,ART)
  


sgu_cas_lookup <- read.csv(file.path(PATHS$input_pressure,"kemisk_forurening","sgu","cas_lookup.csv"))

## fælles SGU-harmonizer (conv = til_ugl for vand, til_ugkg for sed/biota)
sgu_std <- function(df, conv, matrix = c("water","sed","biota")) {
  matrix <- match.arg(matrix)
  out <- df %>%
    filter(!is.na(MATVARDE)) %>%
    left_join(sgu_cas_lookup, by = "PARAMETERNAMN") %>%
    mutate(
      value_num = suppressWarnings(as.numeric(gsub("[<>]", "", MATVARDE))),
      under_loq = grepl("<", MATVARDE),
      value_u   = conv(value_num, ENHET),
      limit_u   = conv(coalesce(DETEKTIONSGRANS_LOD, RAPPORTERINGSGRANS_LOQ, value_num), ENHET),
      date      = as.Date(PROVTAG_DAT),
      station   = as.character(coalesce(as.character(STATION_ID), as.character(PROVPLATS_ID))),
      organ      = if (matrix == "biota") ORGAN          else NA_character_,
      biota_type = if (matrix == "biota") klass_sgu(ART) else NA_character_,
      basis = basis_from_unit(ENHET)
    ) %>%
    filter(!is.na(value_u), !is.na(CAS))
  if (matrix == "biota") out <- out %>% filter(!is.na(biota_type), !is.na(basis))   # kun fisk/skaldyr
  out %>%  
    add_lonlat("NATST_POSITION_E","NATST_POSITION_N", 3006) %>%
    transmute(source = "SGU", station, date, lon, lat, cas = CAS, param = PARAMETERNAMN,
              unit = dplyr::case_when(matrix == "water" ~ "ug/l",
                                      matrix == "biota" ~ paste0("ug/kg ", basis),
                                      TRUE              ~ "ug/kg"),
              value_u, under_loq, limit_u, organ, biota_type, basis)   # <- basis med
}

sgu_vand <- sgu_std(vatten_raw,    til_ugl,  "water")   # µg/L
sgu_sed  <- sgu_std(sgu_sed_raw,   til_ugkg, "sed")   %>%
  mutate(cas = stringr::str_extract(cas, "\\d{1,7}-\\d{2}-\\d"))# µg/kg tv
sgu_bio  <- sgu_std(sgu_biota_raw, til_ugkg, "biota")   # µg/kg vv



