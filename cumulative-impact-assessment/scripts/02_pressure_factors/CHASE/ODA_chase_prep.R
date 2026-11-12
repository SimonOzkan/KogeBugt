## ==================================================================
## ODA havvand-kemi: kobling til grænseværdier og CHASE-kontaminationsratio
## ------------------------------------------------------------------
## ==================================================================
source("scripts/00_setup.R")
PATHS <- set_project_paths()


oda_kemi_vand <- read_csv2(
  file.path(PATHS$input_pressure,"kemisk_forurening", "ODA","HentData_Hav_Vandkemi_20261105-091348.csv"),
  locale = locale(encoding = "latin1", decimal_mark = ",", grouping_mark = "."),
  col_types = cols(ObservationsStedNr = col_character(),
                   ParameterKode      = col_character(),
                   Startdato          = col_date(format = "%Y%m%d"),
                   Startklok          = col_character())
)


oda_kemi_sed <- read_csv2(
  file.path(PATHS$input_pressure,"kemisk_forurening", "ODA","HentData_Hav_Sedimentkemi_20261110-103310.csv"),
  locale = locale(encoding = "latin1", decimal_mark = ",", grouping_mark = "."))


oda_kemi_skalddyr <- read_csv2(
  file.path(PATHS$input_pressure,"kemisk_forurening", "ODA","HentData_Hav_MFS i biota_20261110-102636.csv"),
  locale = locale(encoding = "latin1", decimal_mark = ",", grouping_mark = ".")) %>%
  mutate(biota_type = "shellfish")



oda_kemi_fisk <- read_csv2(
  file.path(PATHS$input_pressure,"kemisk_forurening", "ODA","HentData_Hav_MFS i biota_20261110-102747.csv"),
  locale = locale(encoding = "latin1", decimal_mark = ",", grouping_mark = ".")) %>%
  mutate(biota_type = "fish")



## --- 2. Ikke-kemiske parametre (fjernes fra vand) ------------
ikke_kemi <- c(
  "Oxygen indhold","Oxygenmætning","Salinitet","pH","Alkalinitet,total TA",
  "Klorofyl a","Nitrogen,total N","Phosphor, total-P","Ammoniak+ammonium-N",
  "Nitrit+nitrat-N","Ortho-phosphat-P","Silicium",
  "Suspenderede stoffer","Glødetab,susp.stof","Dihydrogensulfid","Svovlbrinte (sulfid)",
  "Tørstof, total", "Glødetab, total", "Total organisk kulstof",
  "Sedimentfraktion <63 µm", "Sedimentfraktion <2 mm",
  "Grus & sten", "Ler+silt fraktion")

## --- 3. Dansk ODA-parameternavn -> CAS (summer/grupper = NA) --
oda_cas <- tribble(
  ~Parameter,                                          ~CAS,
  # PFAS
  "Perfluorbutansyre (lineær)",                        "375-22-4",
  "Perfluorpentansyre (lineær)",                       "2706-90-3",
  "Perfluorhexansyre (lineær)",                        "307-24-4",
  "Perfluorheptansyre (lineær)",                       "375-85-9",
  "Perfluoroctansyre",                                 "335-67-1",
  "Perfluornonansyre",                                 "375-95-1",
  "Perfluordecansyre (lineær)",                        "335-76-2",
  "Perfluorundecansyre (lineær)",                      "2058-94-8",
  "Perfluordodecansyre (lineær)",                      "307-55-1",
  "Perfluortridecansyre (lineær)",                     "72629-94-8",
  "Perfluorbutansulfonsyre (lineær)",                  "375-73-5",
  "Perfluorpentansulfonsyre (lineær)",                 "2706-91-4",
  "Perfluorhexansulfonsyre",                           "355-46-4",
  "Perfluorheptansulfonsyre (lineær)",                 "375-92-8",
  "Perfluoroctansulfonsyre (sum forgrenet og lineær)", "1763-23-1",
  "Perfluornonansulfonsyre (lineær)",                  "68259-12-1",
  "Perfluordecansulfonsyre (lineær)",                  "335-77-3",
  "Perfluorundecansulfonsyre (lineær)",                "749786-16-1",
  "Perfluordodecansulfonsyre (lineær)",                "79780-39-5",
  "Perfluortridecansulfonsyre (lineær)",               "791563-89-8",
  "Perfluoroctansulfonamid",                           "754-91-6",
  "6:2 Fluortelomersulfonsyre (lineær)",               "27619-97-2",
  "HFPO-DA (GenX)",                                     "13252-13-6",
  "PFAS (sum af PFOA, PFOS, PFNA og PFHxS)",            NA,
  "Sum af PFAS, 22 stoffer",                            NA,
  # Metaller
  "Antimon","7440-36-0", "Arsen","7440-38-2", "Barium","7440-39-3",
  "Bly","7439-92-1", "Bor","7440-42-8", "Cadmium","7440-43-9",
  "Chrom","7440-47-3", "Kobolt","7440-48-4", "Kobber","7440-50-8",
  "Kviksølv","7439-97-6", "Mangan","7439-96-5", "Molybden","7439-98-7",
  "Nikkel","7440-02-0", "Selen","7782-49-2", "Strontium","7440-24-6",
  "Sølv","7440-22-4", "Thallium","7440-28-0", "Tin","7440-31-5",
  "Vanadium","7440-62-2", "Uran","7440-61-1", "Zink","7440-66-6",
  # Flygtige organiske stoffer
  "1,1-Dichlorethylen","75-35-4", "1,1-Dichlorethan","75-34-3",
  "1,2-Dichlorethan","107-06-2", "1,1,1-trichlorethan","71-55-6",
  "Trichlormethan","67-66-3", "Tetrachlormethan","56-23-5",
  "Trichlorethylen","79-01-6", "Tetrachlorethylen","127-18-4",
  "Vinylchlorid","75-01-4", "Benzen","71-43-2",
  "Toluen","108-88-3", "Ethylbenzen","100-41-4",
  "o-Xylen","95-47-6", "m+p-Xylen",NA,
  # PAH og naphthalener
  "Naphthalen","91-20-3", "1-Methyl-napthalen","90-12-0",
  "2-Methylnaphthalen","91-57-6", "Dimethylnaphthalener",NA,
  "Trimethylnaphthalener",NA, "Acenaphthylen","208-96-8",
  "Acenaphthen","83-32-9", "Fluoren","86-73-7",
  "Phenanthren","85-01-8", "Antracen","120-12-7",
  "Fluoranthen","206-44-0", "Pyren","129-00-0",
  "Benz(a)anthracen","56-55-3", "Chrysen","218-01-9",
  "Benz(b)fluoranthen","205-99-2", "Benz(k)fluoranthen","207-08-9",
  "Benz(a)pyren","50-32-8", "Dibenz(a,h)anthracen","53-70-3",
  "Benz(ghi)perylen","191-24-2",
  # Øvrige
  "4-Nonylphenol","84852-15-3",
  "Nonylphenoler","25154-52-3",
  "4-tert-octylphenol","140-66-9",
  "Pentachlorphenol","87-86-5",
  "Tributyltin","36643-28-4",
  "2,3,7,8-Tetrachlordibenzo-p-dioxin",         "1746-01-6",
  "1,2,3,7,8-Pentachlordibenzo-p-dioxin",       "40321-76-4",
  "1,2,3,4,7,8-Hexachlordibenzo-p-dioxin",      "39227-28-6",
  "1,2,3,6,7,8-Hexachlordibenzo-p-dioxin",      "57653-85-7",
  "1,2,3,7,8,9-Hexachlordibenzo-p-dioxin",      "19408-74-3",
  "1,2,3,4,6,7,8-Heptachlordibenzo-p-dioxin",   "35822-46-9",
  "1,2,3,4,6,7,8,9-Octachlordibenzodioxin",     "3268-87-9",
  ## Furaner (PCDF)
  "2,3,7,8-Tetrachlordibenzofuran",             "51207-31-9",
  "1,2,3,7,8-Pentachlordibenzofuran",           "57117-41-6",
  "2,3,4,7,8-Pentachlordibenzofuran",           "57117-31-4",
  "1,2,3,4,7,8-Hexachlordibenzofuran",          "70648-26-9",
  "1,2,3,6,7,8-Hexachlordibenzofuran",          "57117-44-9",
  "1,2,3,7,8,9-Hexachlordibenzofuran",          "72918-21-9",
  "2,3,4,6,7,8-Hexachlordibenzofuran",          "60851-34-5",
  "1,2,3,4,6,7,8-Heptachlordibenzofuran",       "67562-39-4",
  "1,2,3,4,7,8,9-Heptachlordibenzofuran",       "55673-89-7",
  "1,2,3,4,6,7,8,9-Octachlordibenzofuran",      "39001-02-0",
  ## Dioxin-lignende PCB (TJEK apostrof-tegnet, se note)
  "3,3',4,4'-Tetrachlorbiphenyl",               "32598-13-3",  # PCB-77
  "3,3',4,4',5-Pentachlorbiphenyl",             "57465-28-8",  # PCB-126
  "3,3',4,4',5,5'-Hexachlorbiphenyl",           "32774-16-6",  # PCB-169
  ## Phthalater / adipat
  "Di(2-ethylhexyl)phthalat",                   "117-81-7",    # DEHP (har EQS)
  "Dibutylphthalat",                            "84-74-2",
  "Diethylphthalat",                            "84-66-2",
  "Benzylbutylphthalat",                        "85-68-7",
  "Di-n-octylphthalat",                         "117-84-0",
  "Diisononylphthalat",                         "28553-12-0",
  "Diisodecyl phthalat",                        "26761-40-0",
  "Di(2-ethylhexyl)adipat",                     "103-23-1",
  ## Organotin (CAS-konvention varierer — se note)
  "Monobutyltin",                               "78763-54-9",
  "Dibutyltin",                                 "14488-53-0",
  "Triphenyltin",                               "668-34-8",
  ## PAH
  "Indeno(1,2,3-cd)pyren",                      "193-39-5",
  "Benzo(e)pyren",                              "192-97-2",
  "Perylen",                                    "198-55-0",
  "Dibenzothiophen",                            "132-65-0",
  "Benz(a)fluoren",                             "238-84-6",
  "1-Methylpyren",                              "2381-21-7",
  "2-Methylpyren",                              "3442-78-2",
  "2-Methylphenanthren",                        "2531-84-2",
  "Crysen/triphenylen",                         "218-01-9",    # chrysen
  ## Øvrige organiske
  "Hexachlorbutadien",                          "87-68-3",     # HCBD (har EQS)
  "Octylphenol",                                "140-66-9",
  "4-n-octylphenol",                            "1806-26-4",
  ## Normaliserings-metaller (ingen EQS)
  "Aluminium",                                  "7429-90-5",
  "Lithium",                                    "7439-93-2")

oda_cas_biota <- tribble(
  ~Parameter,                               ~CAS,
  ## --- PCB6 (matcher Group_table scb6) ---
  "2,4,4'-Trichlorbiphenyl",                "7012-37-5",    # PCB-28
  "2,2',5,5'-Tetrachlorbiphenyl",           "35693-99-3",   # PCB-52
  "2,2',4,5,5'-Pentachlorbiphenyl",         "37680-73-2",   # PCB-101
  "2,2',3,4,4',5'-Hexachlorbiphenyl",       "35065-28-2",   # PCB-138
  "2,2',4,4',5,5'-Hexachlorbiphenyl",       "35065-27-1",   # PCB-153
  "2,2',3,4,4',5,5'-heptachlorbiphenyl",    "35065-29-3",   # PCB-180
  ## --- dioxin-lignende PCB (matcher WHOTEQ-PCB) ---
  "3,3',4,4'-Tetrachlorbiphenyl",           "32598-13-3",   # PCB-77
  "3,4,4',5-Tetrachlorbiphenyl",            "70362-50-4",   # PCB-81
  "2,3,3',4,4'-Pentachlorbiphenyl",         "32598-14-4",   # PCB-105 (RIGTIG CAS; se note)
  "2,3,4,4',5-Pentachlorbiphenyl",          "74472-37-0",   # PCB-114
  "2,3',4,4',5-Pentachlorbiphenyl",         "31508-00-6",   # PCB-118
  "2,3',4,4',5'-Pentachlorbiphenyl",        "65510-44-3",   # PCB-123
  "2,3,3',4,4',5-Hexachlorbiphenyl",        "38380-08-4",   # PCB-156
  "2,3,3',4,4',5'-Hexachlorbiphenyl",       "69782-90-7",   # PCB-157
  "2,3',4,4',5,5'-Hexachlorbiphenyl",       "52663-72-6",   # PCB-167
  "3,3',4,4',5,5'-Hexachlorbiphenyl",       "32774-16-6",   # PCB-169
  "2,3,3',4,4',5,5'-Heptachlorbiphenyl",    "39635-31-9",   # PCB-189
  ## --- øvrige PCB (ingen EQS/aktiv gruppe — valgfri, påvirker ikke CHASE) ---
  "2,4',5-Trichlorbiphenyl",                "16606-02-3",   # PCB-31
  "2,2',3,5'-Tetrachlorbiphenyl",           "41464-39-5",   # PCB-44
  "2,2',4,5'-Tetrachlorbiphenyl",           "41464-40-8",   # PCB-49
  "2,2',4,4',5-Pentachlorbiphenyl",         "38380-01-7",   # PCB-99
  "2,3,3',4',6-Pentachlorbiphenyl",         "38380-03-9",   # PCB-110
  "2,2',3,3',4,4'-Hexachlorbiphenyl",       "38380-07-3",   # PCB-128
  "2,2',3,4',5',6-Hexachlorbiphenyl",       "38380-04-0",   # PCB-149
  "2,2',3,5,5',6-Hexachlorbiphenyl",        "52663-63-5",   # PCB-151
  "2,2',3,3',4,4',5-Heptachlorbiphenyl",    "35065-30-6",   # PCB-170
  "2,2',3,4',5,5',6-Heptachlorbiphenyl",    "52663-68-0",   # PCB-187
  "2,2',3,3',4,4',5,5',6,6'-Decachlorbiphenyl","2051-24-3", # PCB-209
  ## --- PBDE6 (matcher sbde6) ---
  "2,4,4-Tribromodiphenylether",            "41318-75-6",   # BDE-28
  "2,2',4,4'-Tetrabromdiphenylether",       "5436-43-1",    # BDE-47
  "2,2',4,4',5-Pentabromdiphenylether",     "32534-81-9",   # BDE-99
  "2,2',4,4',6-Pentabromdiphenylether",     "189084-64-8",  # BDE-100
  "2,2',4,4',5,5'-Hexabromdiphenylether",   "68631-49-2",   # BDE-153
  "2,2',4,4',5,6'-Hexabromdiphenylether",   "207122-15-4",  # BDE-154
  ## --- øvrige PBDE (ingen aktiv gruppe) ---
  "2,3',4,4'-Tetrabromdiphenylether",       "189084-61-5",  # BDE-66
  "2,2,3,4,4-Pentabromodiphenylether",      "182346-21-0",  # BDE-85
  "2,2',3,4,4',5',6-Heptabromdiphenylether","207122-16-5",  # BDE-183
  "Decabromdiphenylether",                  "1163-19-5",    # BDE-209
  "BDE #197",                               "117964-21-3",
  "BDE #203",                               "337513-72-1",
  ## --- DDT (matcher DDTSUM) + øvrige isomerer ---
  "DDT, p,p'-",                             "50-29-3",
  "DDT, o,p'-",                             "789-02-6",
  "DDE, p,p'-",                             "72-55-9",
  "DDD, p,p'-",                             "72-54-8",
  "DDE, o,p'-",                             "3424-82-6",
  "DDD, o,p'-",                             "53-19-0",
  ## --- HCH + HCB ---
  "Gamma-hexachlorcyclohexan",              "58-89-9",      # lindan (har EQS)
  "Alfa-hexachlorcyclohexan",               "319-84-6",
  "Beta-hexachlorcyclohexan",               "319-85-7",
  "Delta-hexachlorcyclohexan",              "319-86-8",
  "Hexachlorbenzen",                        "118-74-1",     # HCB (har EQS)
  ## --- HBCDD (matcher HBCDSUM) ---
  "alpha-Hexabromcyclododecan",             "134237-50-6",
  "beta-Hexabromcyclododecan",              "134237-51-7",
  "gamma-Hexabromcyclododecan",             "134237-52-8",
  ## --- cyclodiener / chlordaner (HEPCHCEPX + øvrige) ---
  "Heptachlor",                             "76-44-8",
  "Heptachlorepoxid",                       "1024-57-3",
  "Chlordan",                               "57-74-9",
  "Trans-nonachlor",                        "39765-80-5",
  ## --- PFAS (lange kæder, ingen egen EQS men gyldig CAS) ---
  "Perfluortetradecansyre (Lineær)",        "376-06-7",     # PFTeDA
  "Perfluorhexadecansyre (Lineær)",         "67905-19-5",   # PFHxDA
  "Perfluoroctadecansyre (Lineær)",         "16517-11-6")   # PFODA

norm_param <- function(x) gsub("[\u00b4\u2019'?]", "'", x)   # ´ ' ' ?  -> '

oda_cas <- bind_rows(oda_cas, oda_cas_biota) %>%
  mutate(Parameter = norm_param(Parameter)) %>%
  distinct(Parameter, .keep_all = TRUE)     # <- fjern evt. dubletter mellem sediment/biota



## --- 4. Harmonisér til fælles skema --------------------------
## skema: source, station, date, lon, lat, cas, value_u, under_loq, limit_u, organ, biota_type

# VAND  (µg/L; X_UTM32/Y_UTM32 = EPSG:25832 -> lon/lat)
# VAND  (µg/L; X_UTM32/Y_UTM32 = EPSG:25832 -> lon/lat)
oda_vand <- oda_kemi_vand %>%
  filter(!Parameter %in% ikke_kemi) %>%
  left_join(oda_cas, by = "Parameter") %>%
  mutate(value_u   = til_ugl(Resultat, Enhed),
         under_loq = ResultatAttribut %in% c("<","<="),
         limit_u   = ifelse(under_loq, value_u, NA_real_)) %>%   # <- rettet parentes + NA_real_
 # filter(!is.na(value_u), !is.na(CAS)) %>%
  add_lonlat("X_UTM32","Y_UTM32", 25832) %>%
  transmute(source = "ODA", station = as.character(ObservationsStedNr),
            date = as.Date(Startdato), lon, lat, cas = CAS,param = Parameter,
            value_u, under_loq, limit_u,unit = "ug/l",
            organ = NA_character_, biota_type = NA_character_,
            category = "vand")

# SEDIMENT  (µg/kg tørvægt; Længde/Bredde = WGS84)
oda_sed <- oda_kemi_sed %>%
  left_join(oda_cas, by = "Parameter") %>%
  mutate(value_u   = til_ugkg(Resultat, Enhed),
         limit_u   = til_ugkg(Detektionsgrænse, Enhed),
         under_loq = Attribut %in% c("<","<="),
         date = ymd(Dato)) %>%
  transmute(source = "ODA", station = as.character(ObservationsStedNr), date,
            lon = Længde, lat = Bredde, cas = CAS,param = Parameter,
            value_u, under_loq, limit_u,unit  = "ug/kg",
            organ = NA_character_, biota_type = NA_character_,
            category = "sediment")




# ODA biota (fisk + skaldyr) - erstat filter(str_detect(Enhed,"VV")) med basis:
oda_bio <- bind_rows(oda_kemi_fisk %>% mutate(biota_type="fish"),
                     oda_kemi_skalddyr %>% mutate(biota_type="shellfish")) %>%
  left_join(oda_cas, by="Parameter") %>%
  mutate(value_u=til_ugkg(Resultat,Enhed), limit_u=til_ugkg(Detektionsgrænse,Enhed),
         under_loq=Attribut %in% c("<","<="), date=ymd(Dato),
         basis = basis_from_unit(Enhed)) %>%                     # <- basis fra enhed
  filter(!is.na(value_u), !is.na(CAS), !is.na(basis)) %>%        # dropper pct-rækker (ingen basis)
  transmute(source = "ODA", station = as.character(ObservationsStedNr), date,unit = paste0("ug/kg ",basis),
            lon = Længde, lat = Bredde, cas = CAS,param =Parameter,
            value_u, under_loq, limit_u,
            organ = Organ, biota_type,basis,
            category = "biota")


# SGU biota - tilføj basis = basis_from_unit(ENHET) i mutate og basis i transmute,
#             og !is.na(basis) i filter (resten som før)

oda_df <- bind_rows(oda_vand,oda_sed,oda_bio)

write.csv(oda_df, file.path(PATHS$input_pressure,"kemisk_forurening", "ODA","ODA_df.csv"), row.names = FALSE)
## --- 5. Hurtige tjek -----------------------------------------
# Parametre uden CAS-match (bør kun være summer/grupper):
oda_kemi_sed  %>% left_join(oda_cas, by="Parameter") %>% filter(is.na(CAS)) %>% distinct(Parameter) %>% print()
oda_kemi_fisk %>% left_join(oda_cas, by="Parameter") %>% filter(is.na(CAS)) %>% distinct(Parameter) %>% print()
c(vand = nrow(oda_vand), sed = nrow(oda_sed), bio = nrow(oda_bio))
