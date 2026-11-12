source("scripts/00_setup.R")
PATHS <- set_project_paths()


Threshold_catalogue_2024 <- read_excel(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","Threshold catalogue 2024.xlsx")) 
groups<-read.table(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","Group_table.csv"),sep=";",header = T)%>% #if Ignore = 1, the threshold is not being used (obsolete, not applicable...)
  mutate(`Substance.group`= tolower(`Substance.group`))


CAS_ICES<- read.table(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","RECO_Export_10-48-2026-08-48-04.csv"),
                      sep=",",header=T,comment.char="",fileEncoding="UTF-8",quote='"')

PARAM_ICES<- read.table(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","ICES_PARAM_names.csv"),
                        sep=",",header=T,comment.char="",fileEncoding="UTF-8",quote='"')

## ============================================================
## FÆLLES MOTOR  (bruges af vand, sediment og biota)
## ============================================================
source_priority <- c("ODA","SGU")

## Fælles skema pr. række:
## Fælles skema pr. række (nu uden grouping/multiplier):
## source, station, date, lon, lat, cas, value_u, under_loq, organ
chase_engine <- function(dat, category, conv, prio = source_priority, rnd = 10) {
  
  ## --- tærskler fra threshold_values() (grupper, vægte, CAS + type/væv/basis for biota) ---
  tt        <- threshold_values(category)
  has_bt    <- "Biota_type"       %in% names(tt)
  has_mtx   <- "Threshold_matrix" %in% names(tt)
  has_basis <- "Threshold_basis"  %in% names(tt)
  
  norm_mtx <- function(x){ x <- tolower(tidyr::replace_na(as.character(x),""))
  dplyr::case_when(x %in% c("whole","whole organism","soft tissue","sb") ~ "whole",
                   x == "muscle" ~ "muscle", x == "liver" ~ "liver", TRUE ~ "") }
  
  tt <- tt %>%
    mutate(thr_u = conv(Threshold_value, Threshold_unit),
           bt    = if (has_bt) tolower(tidyr::replace_na(Biota_type,"")) else "",
           bt    = ifelse(bt %in% c("both",""), "", bt),                         # Both/NA = alle
           mtx   = if (has_mtx)   norm_mtx(Threshold_matrix)                       else "",
           thr_basis = if (has_basis) toupper(tidyr::replace_na(Threshold_basis,"W")) else "W") %>%
    filter(!is.na(thr_u))
  
  ind <- tt %>% filter(is.na(Group), !is.na(CAS)) %>%
    transmute(cas = CAS, Substance = `Substance name`, Threshold_source, thr_u, bt, mtx, thr_basis)
  
  gmem <- tt %>% filter(Group == 1, !is.na(CAS), CAS != "", !is.na(PARAM.Group)) %>%
    transmute(cas = CAS, grouping = PARAM.Group,
              multiplier = coalesce(suppressWarnings(as.numeric(Multiplier)), 1))
  
  gthr <- tt %>% filter(Group == 1, !is.na(PARAM.Group)) %>%
    transmute(grouping = PARAM.Group, Substance = `Substance name`, Threshold_source, thr_u, bt, mtx, thr_basis)
  
  ## --- dedup + LOD/2 på målingsniveau (+ normalisér organ og måle-basis) ---
  d <- dat %>% filter(!is.na(cas), !is.na(value_u)) %>%
    mutate(prio = match(source, prio), lon_r = round(lon, rnd), lat_r = round(lat, rnd),
           btype   = tolower(tidyr::replace_na(as.character(biota_type), "")),
           organ_n = norm_organ(organ),
           m_basis = if (category == "Biota") toupper(tidyr::replace_na(as.character(basis),"")) else "",
           limit_u = coalesce(limit_u, value_u),
           conc = ifelse(under_loq, limit_u/2, value_u)) %>%
    group_by(lon_r, lat_r, date, cas, organ_n, btype, m_basis) %>%
    slice_min(prio, n=1, with_ties=FALSE) %>% ungroup() %>%
    mutate(station_id = paste(source, station, btype, organ_n, sep="_"), .rid = row_number())
  
  ## type + væv specifik foran generisk; basis er hård betingelse
  ## pick:
  pick <- function(j) j %>%
    filter((bt == "" | bt == btype) &
             (mtx == "" | mtx == organ_n) &
             (m_basis == "" | thr_basis == m_basis)) %>%        # <- thr_basis, ikke basis
    mutate(sc = (bt != "" & bt == btype) + (mtx != "" & mtx == organ_n)) %>%
    group_by(.rid) %>% arrange(desc(sc), thr_u) %>% slice(1) %>% ungroup()
  
  # p_ind <- d %>% filter(!cas %in% gmem$cas) %>%
  #   inner_join(ind, by = "cas", relationship = "many-to-many") %>% pick() %>%
  #   transmute(station_id, source, lon, lat, date, Substance, Threshold_source, thr_u,
  #             value_eff = conc, limit_eff = limit_u, under = under_loq)
  
  p_ind <- d %>% filter(!cas %in% gmem$cas) %>%
    inner_join(ind, by = "cas", relationship = "many-to-many") %>% pick() %>%
    transmute(station_id, station, source, lon, lat, date, Substance, Threshold_source, thr_u,
              value_eff = conc, limit_eff = limit_u, under = under_loq)
  
  
  # p_grp <- d %>% inner_join(gmem, by = "cas") %>%
  #   group_by(station_id, source, lon, lat, date, grouping, btype, organ_n, m_basis) %>%
  #   summarise(value_eff = sum(conc * multiplier), limit_eff = sum(limit_u * multiplier),
  #             under = all(under_loq), .groups = "drop") %>%
  #   mutate(.rid = row_number()) %>%
  #   inner_join(gthr, by = "grouping", relationship = "many-to-many") %>% pick() %>%
  #   transmute(station_id, source, lon, lat, date, Substance = grouping, Threshold_source, thr_u,
  #             value_eff, limit_eff, under)
  
  p_grp <- d %>% inner_join(gmem, by = "cas") %>%
    group_by(station_id, station, source, lon, lat, date, grouping, btype, organ_n, m_basis) %>%
    summarise(value_eff = sum(conc * multiplier), limit_eff = sum(limit_u * multiplier),
              under = all(under_loq), .groups = "drop") %>%
    mutate(.rid = row_number()) %>%
    inner_join(gthr, by = "grouping", relationship = "many-to-many") %>% pick() %>%
    transmute(station_id, station, source, lon, lat, date, Substance = grouping, Threshold_source, thr_u,
              value_eff, limit_eff, under)
  
  
  prove <- bind_rows(p_ind, p_grp)
  return(prove)
}


oda_df <- read.csv(file.path(PATHS$input_pressure,"kemisk_forurening", "ODA","ODA_df.csv"))

sgu_df <- read.csv(file.path(PATHS$input_pressure,"kemisk_forurening", "SGU","SGU_df.csv"))


## ============================================================
## VAND 
## ============================================================


vand_all <- bind_rows(oda_df %>% filter(category == "vand") , sgu_df  %>% filter(category == "vand")) %>% filter(year(date) %in% 2010:2024)%>%
  filter(!is.na(cas)) %>%
  dplyr::select(-category) # funktionen får category fra threshold, derfor fjernes den inden

res_vand <- chase_engine(vand_all, category="Water", conv=til_ugkg)
## --- CR pr. station × stof ---
cr_vand <- res_vand %>%
  group_by(station,lon,lat,Substance,Threshold_source,thr_u) %>%
  summarise(n=n(), all_under=all(under),
            limit_min=suppressWarnings(min(limit_eff[under],na.rm=TRUE)),
            mean_conc=mean(value_eff), .groups="drop") %>%
  mutate(CR=mean_conc/thr_u,
         status=case_when(all_under & is.finite(limit_min) & limit_min>=thr_u ~ "ikke-konklusiv",
                          CR>1 ~ "overskredet", TRUE ~ "god"),
         category = "Vand") %>%
  filter(!status == "ikke-konklusiv")


## ============================================================
## SEDIMENT   (alt tørvægt: ODA "TS", ICES basis="D", SGU "tv")  -> µg/kg tv
## ============================================================

sed_all <- bind_rows(oda_df %>% filter(category == "sediment") , sgu_df  %>% filter(category == "sediment")) %>% filter(year(date) %in% 2010:2024)%>%
  filter(!is.na(cas)) %>%
  dplyr::select(-category) # funktionen får category fra threshold, derfor fjernes den inden

res_sed <- chase_engine(sed_all, category="Sediment", conv=til_ugkg)



## --- CR pr. station × stof ---
cr_sed <- res_sed %>%
  group_by(station,lon,lat,Substance,Threshold_source,thr_u) %>%
  summarise(n=n(), all_under=all(under),
            limit_min=suppressWarnings(min(limit_eff[under],na.rm=TRUE)),
            mean_conc=mean(value_eff), .groups="drop") %>%
  mutate(CR=mean_conc/thr_u,
         status=case_when(all_under & is.finite(limit_min) & limit_min>=thr_u ~ "ikke-konklusiv",
                          CR>1 ~ "overskredet", TRUE ~ "god"),
         category = "Sediment") %>%
  filter(status != "ikke-konklusiv")


## ============================================================
## BIOTA   (EQS = vådvægt).  ICES=W, SGU=vv -> vådvægt; ODA: kun VV-rækker
## ============================================================
# ODA biota  (behold kun vådvægt "VV"; TS-rækker kræver tørstof-% for at omregne)

bio_all <- bind_rows(oda_df %>% filter(category == "biota") , sgu_df  %>% filter(category == "biota")) %>% filter(year(date) %in% 2010:2024) %>%
  dplyr::select(-category) # funktionen får category fra threshold, derfor fjernes den inden


res_bio <- chase_engine(bio_all, category = "Biota", conv = til_ugkg)


cr_bio <- res_bio %>%
  group_by(station,lon,lat,Substance,Threshold_source,thr_u) %>%
  summarise(n=n(), all_under=all(under),
            limit_min=suppressWarnings(min(limit_eff[under],na.rm=TRUE)),
            mean_conc=mean(value_eff), .groups="drop") %>%
  mutate(CR=mean_conc/thr_u,
         status=case_when(all_under & is.finite(limit_min) & limit_min>=thr_u ~ "ikke-konklusiv",
                          CR>1 ~ "overskredet", TRUE ~ "god"),
         category = "Biota") %>%
  filter(status != "ikke-konklusiv") 


########################### Bind all together ###############
df_bind_check <- bind_rows(res_vand,res_sed,res_bio)

cr_bind_check <- bind_rows(cr_vand,cr_sed,cr_bio)

CR_bind <- bind_rows(cr_vand,cr_sed,cr_bio) %>%
  mutate(logCR=log10(CR)) %>%
  group_by(station,Substance,category) %>%
  summarise(logCR=mean(logCR,na.rm=T), .groups="drop") %>%
  mutate(CR = 10^logCR) %>%
  group_by(station,category) %>%
  mutate(EQR_cat = EQR_CR(CR)) 


# Worst substance = highest EQR value per gridcell

df_cat <- CR_bind %>%
  mutate(station=tolower(station))%>%
  group_by(station, category) %>%
  mutate(n_subs=n()) %>%
  arrange(desc(CR)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(WorstSubst=Substance) 


# Integrated EQR = minimum EQR per gridcell

df_CHASE <- df_cat %>% #integrated one
  group_by(station) %>%
  arrange(EQR_cat) %>%
  slice(1) %>%
  ungroup()%>%
  rename(WorstCat=category)



### Tilføj koordinator  ####

all_coords <- bind_rows(vand_all,bio_all,sed_all) %>%
  group_by(station) %>%
  summarise(
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
            , .groups = "drop") %>%
  mutate(station = as.character(station))


df_CHASE <- left_join(df_CHASE, all_coords)

write.csv(df_CHASE,file.path(PATHS$input_pressure,"kemisk_forurening","CHASE.csv"), row.names = FALSE)


