### Hjælpe funktioner ####

source("scripts/00_setup.R")
PATHS <- set_project_paths()

klass_biota <- function(x){                        # -> "fish" / "shellfish" / "" (generisk)
  x <- tolower(coalesce(x, ""))
  dplyr::case_when(
    str_detect(x, "mussel|mussla|musling|oyster|ostron|clam|cockle|crust|krabb|crab|shrimp|reke|mytilus|cerasto") ~ "shellfish",
    str_detect(x, "fish|fisk|gadus|clupea|perca|zoarces|platichthys|limanda|torsk|sill|strömming|skrubb||tånglake|ål|abborre") ~ "fish",
    TRUE ~ "")
}


klass_sgu <- function(x){
  dplyr::case_when(
    x %in% c("Sill","Skrubbskadda","Gadda","Tanglake","Abborre","abborre","Mort") ~ "fish",
    x %in% c("Blamussla","Raka","StorTusensnacka")                                ~ "shellfish",
    TRUE ~ NA_character_)   # fugle/pattedyr -> NA -> droppes i biota
}

til_ugl  <- function(v,u){e<-gsub("\\s","",tolower(u))
case_when(str_detect(e,"^ng")~v/1000, 
          str_detect(e,"^(ug|µg|mikrog)")~v,
          str_detect(e,"^mg")~v*1000,
          str_detect(e,"^pg")~v/1e6, 
          TRUE~NA_real_)}

til_ugkg <- function(v,u){e<-tolower(trimws(u))
case_when(str_detect(e,"^ng")~v/1000,
          str_detect(e,"^(ug|µg|mikrog)")~v,
          str_detect(e,"^mg")~v*1000,
          str_detect(e,"^pg")~v/1e6,
          str_detect(e,"^g")~v*1e9,
          TRUE~NA_real_)}

add_lonlat <- function(df,x,y,crs){
  p <- df %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]])) %>%
    st_as_sf(coords=c(x,y), crs=crs) %>% st_transform(4326)
  xy <- st_coordinates(p); st_drop_geometry(p) %>% mutate(lon=xy[,1], lat=xy[,2])}



threshold_values<-function(category){
  category<-tolower(category)
  groups<-read.table("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/kemisk_forurening/reference_vaerdi/Group_table.csv",sep=";",header = T)%>% #if Ignore = 1, the threshold is not being used (obsolete, not applicable...)
    mutate(`Substance.group`= tolower(`Substance.group`))
  
  
  threshold<-read_excel("C:/Users/SIO/NIVA/Køge Bugt - General/data/Pressure/kemisk_forurening/reference_vaerdi/Threshold catalogue 2024.xlsx")%>%
    mutate(Category=tolower(Category))%>%
    filter(Category==category)%>% #we want only the specific category parameters
    filter(is.na(Ignore))%>% #if Ignore 1, the threshold is not being used (obsolete, not applicable...)
    mutate(`Substance name`= tolower(`Substance name`))
  
  threshold_group<-threshold%>%
    filter(Group==1)%>%
    left_join(groups,by=c("Substance name" = "Substance.group"),relationship = "many-to-many")%>% #for biota, one param can have more than one threshold (for fish and shellfish)
    mutate(CAS=CAS.y)%>%
    select(-CAS.y,-CAS.x)
  
  threshold_group<-threshold_group[, colSums(is.na(threshold_group)) != nrow(threshold_group)]
  if(nrow(threshold_group>0)){
    threshold<-threshold%>%
      filter(is.na(Group))%>%
      bind_rows(threshold_group)%>%
      mutate(Substance.X= ifelse(is.na(Substance.X),`Substance name`,Substance.X),
             Substance.X= tolower(Substance.X))
  }else{
    threshold<-threshold%>%
      filter(is.na(Group))%>%
      bind_rows(threshold_group)
  }
  
  
  threshold<-threshold[, colSums(is.na(threshold)) != nrow(threshold)]
  
  if (category=="Water"){
    
    threshold<-threshold%>%
      mutate(Harmon_name=case_when(
        .$`Substance name`=="benz[a]anthracene" ~ "benzo[a]anthracene",
        .$`Substance name`=="chrome" ~ "chromium",
        .$`Substance name`=="chrysene (triphenylene)" ~ "chrysene", #"Chrysene (including triphenylene)" in OSPAR, not sure how this should be added, only as chrisene? the sum of the two? CAS 217-59-4 triphenylene
        .$`Substance name`=="dde (p,p’)" ~ "dde (p,p')",
        .$`Substance name`=="gamma-hch" ~ "gamma-hch (gamma-hexachlorocyclohexane)",
        .$`Substance name`=="naphtalene" ~ "naphthalene",
        .$`Substance name`=="pcb-28" ~ "cb28",
        .$`Substance name`=="pcb-52" ~ "cb52",
        .$`Substance name`=="pcb-101" ~ "cb101",
        .$`Substance name`=="pcb-118" ~ "cb118",
        .$`Substance name`=="pcb-138" ~ "cb138",
        .$`Substance name`=="pcb-153" ~ "cb153",
        .$`Substance name`=="pcb-180" ~ "cb180",
        .$`Substance name`=="tbtin (tbsn+)" ~ "n-tributyltin ion", #following the source of the threshold, TBT is  36643-28-4 for sediment. https://www.ospar.org/documents?v=43227
        .$`Substance name`=="bde100" ~ "bd100",
        .$`Substance name`=="bde153" ~ "bd153",
        .$`Substance name`=="bde154" ~ "bd154",
        .$`Substance name`=="bde183" ~ "bd183",
        .$`Substance name`=="bde209" ~ "bd209",
        .$`Substance name`=="aclonifen" ~ "benzenamine, 2-chloro-6-nitro-3-phenoxy-",
        .$`Substance name`=="alachlor" ~ "alacl",
        .$`Substance name`=="c10-13 chloroalkanes" ~ "sccp",
        .$`Substance name`=="carbon-tetrachloride" ~ "carbon tetrachloride",
        .$`Substance name`=="chlorfenvinphos" ~ "phosphoric acid, 2-chloro-1-(2,4-dichlorophenyl)ethenyl diethyl ester",
        .$`Substance name`=="chlorpyrifos (chlorpyrifos-ethyl)" ~ "phosphorothioic acid, o,o-diethyl o-(3,5,6-trichloro-2-pyridinyl) ester",
        .$`Substance name`=="cybutryne / irgarol" ~ "irgarol 1051",
        .$`Substance name`=="dichloromethane" ~ "methane, dichloro-",
        .$`Substance name`=="dicofol" ~ "benzenemethanol, 4-chloro-.alpha.-(4-chlorophenyl)-.alpha.-(trichloromethyl)-",
        .$`Substance name`=="diuron" ~ "urea, n'-(3,4-dichlorophenyl)-n,n-dimethyl-",
        .$`Substance name`=="isoproturon" ~ "urea, n,n-dimethyl-n'-[4-(1-methylethyl)phenyl]-",
        .$`Substance name`=="octylphenols ((4-(1,1′,3,3′-tetramethylbutyl)-phenol))" ~ "phenol, 4-(1,1,3,3-tetramethylbutyl)-",
        .$`Substance name`=="quinoxyfen" ~ "quinoline, 5,7-dichloro-4-(4-fluorophenoxy)-",
        .$`Substance name`=="terbutryn" ~ "1,3,5-triazine-2,4-diamine, n2-(1,1-dimethylethyl)-n4-ethyl-6-(methylthio)-",
        .$`Substance name`=="tetrachloro-ethylene" ~ "tetrachloroethylene",
        .$`Substance name`=="trichloro-ethylene" ~ "trichloroethylene",
        .$`Substance name`=="trichloro-methane (chloroform)" ~ "chloroform",
        .$`Substance name`=="bifenox" ~ "benzoic acid, 5-(2,4-dichlorophenoxy)-2-nitro-, methyl ester",
        .$`Substance name`=="cypermethrin" ~ "cyclopropanecarboxylic acid, 3-(2,2-dichloroethenyl)-2,2-dimethyl-, cyano(3-phenoxyphenyl)methyl ester",
      ),
      Harmon_name= ifelse(is.na(Harmon_name),Substance.X,Harmon_name))
    
    CAS<-cas_numbers()
    threshold<-left_join(threshold,CAS,by=c("CAS"))
    threshold<-threshold %>%
      mutate(Description= ifelse(!is.na(Description),Description,Harmon_name),
             PARAM= ifelse(is.na(PARAM)& grepl("pcb",`Substance name`),Harmon_name,
                           ifelse(is.na(PARAM) & !grepl("pcb",`Substance name`),`Substance name`,PARAM)))%>%
      distinct()
    threshold$`Substance name`<-gsub("’","'",threshold$`Substance name`)
    return(threshold)
  }
  else if (category=="Sediment"){
    
    threshold<-threshold%>%
      mutate(Harmon_name=case_when(
        .$`Substance name`=="benz[a]anthracene" ~ "benzo[a]anthracene",
        .$`Substance name`=="chrome" ~ "chromium",
        .$`Substance name`=="chrysene (triphenylene)" ~ "chrysene", #"Chrysene (including triphenylene)" in OSPAR, not sure how this should be added, only as chrisene? the sum of the two? CAS 217-59-4 triphenylene
        .$`Substance name`=="dde (p,p’)" ~ "dde (p,p')",
        .$`Substance name`=="gamma-hch" ~ "gamma-hch (gamma-hexachlorocyclohexane)",
        .$`Substance name`=="naphtalene" ~ "naphthalene",
        .$`Substance name`=="pcb-28" ~ "cb28",
        .$`Substance name`=="pcb-52" ~ "cb52",
        .$`Substance name`=="pcb-101" ~ "cb101",
        .$`Substance name`=="pcb-118" ~ "cb118",
        .$`Substance name`=="pcb-138" ~ "cb138",
        .$`Substance name`=="pcb-153" ~ "cb153",
        .$`Substance name`=="pcb-180" ~ "cb180",
        .$`Substance name`=="tbtin (tbsn+)" ~ "n-tributyltin ion", #following the source of the threshold, TBT is  36643-28-4 for sediment. https://www.ospar.org/documents?v=43227
        .$`Substance name`=="bde100" ~ "bd100",
        .$`Substance name`=="bde153" ~ "bd153",
        .$`Substance name`=="bde154" ~ "bd154",
        .$`Substance name`=="bde183" ~ "bd183",
        .$`Substance name`=="bde209" ~ "bd209",
        .$`Substance name`=="aclonifen" ~ "benzenamine, 2-chloro-6-nitro-3-phenoxy-",
        .$`Substance name`=="cypermethrin" ~ "cyclopropanecarboxylic acid, 3-(2,2-dichloroethenyl)-2,2-dimethyl-, cyano(3-phenoxyphenyl)methyl ester"
      ),
      Harmon_name= ifelse(is.na(Harmon_name),Substance.X,Harmon_name))
    
    CAS<-cas_numbers()
    threshold<-left_join(threshold,CAS,by=c("Harmon_name"="Description"))%>%
      left_join(CAS,by= c("Harmon_name"="PARAM"))%>%
      mutate(CAS= ifelse(is.na(CAS.x),CAS.y,CAS.x),
             Description= ifelse(!is.na(Description),Description,Harmon_name),
             PARAM= ifelse(is.na(PARAM)& grepl("pcb",`Substance name`),Harmon_name,
                           ifelse(is.na(PARAM) & !grepl("pcb",`Substance name`),`Substance name`,PARAM)))%>%
      select(-CAS.x,-CAS.y)%>%
      distinct()
    
    
    threshold$`Substance name`<-gsub("’","'",threshold$`Substance name`)
    return(threshold)
  }
  else if (category=="Bioeffects"){
    threshold$`Substance name`<-gsub("’","'",threshold$`Substance name`)
    return(threshold)
  }
  else {
    
    threshold<-threshold%>%
      mutate(Harmon_name=case_when(
        .$`Substance name` == "aclonifen" ~ "benzenamine, 2-chloro-6-nitro-3-phenoxy-",
        .$`Substance name` == "alpha-hch" ~ "alpha-hch (alpha-hexachlorocyclohexane)",
        .$`Substance name` == "bde100" ~ "bd100",
        .$`Substance name` == "bde153" ~ "bd153",
        .$`Substance name` == "bde154" ~ "bd154",
        .$`Substance name` == "bde183" ~ "bd183",
        .$`Substance name` == "bde209" ~ "bd209",
        .$`Substance name` == "benzo[a]antracene" ~ "benzo[a]anthracene",
        .$`Substance name` == "bifenox" ~ "benzoic acid, 5-(2,4-dichlorophenoxy)-2-nitro-, methyl ester",
        .$`Substance name` == "cb101" ~ "cb101",
        .$`Substance name` == "cb118" ~ "cb118",
        .$`Substance name` == "cb138" ~ "cb138",
        .$`Substance name` == "cb153" ~ "cb153",
        .$`Substance name` == "cb180" ~ "cb180",
        .$`Substance name` == "cb28" ~ "cb28",
        .$`Substance name` == "cb52" ~ "cb52",
        .$`Substance name` == "chlorfenvinphos" ~ "phosphoric acid, 2-chloro-1-(2,4-dichlorophenyl)ethenyl diethyl ester",
        .$`Substance name` == "chlorpyrifos (chlorpyrifos-ethyl)" ~ "phosphorothioic acid, o,o-diethyl o-(3,5,6-trichloro-2-pyridinyl) ester",
        .$`Substance name` == "chrome" ~ "chromium",
        .$`Substance name` == "chrysene (triphenylene)" ~ "chrysene", #"Chrysene (including triphenylene)" in OSPAR, not sure how this should be added, only as chrisene? the sum of the two? CAS 217-59-4 triphenylene
        .$`Substance name` == "c10-13 chloroalkanes" ~ "short chain chlorinated paraffin (c10-c13)",
        .$`Substance name` == "cybutryne / irgarol" ~ "irgarol 1051",
        .$`Substance name` == "cypermethrin" ~ "cyclopropanecarboxylic acid, 3-(2,2-dichloroethenyl)-2,2-dimethyl-, cyano(3-phenoxyphenyl)methyl ester",
        .$`Substance name` == "dde (p,p’)" ~ "dde (p,p')",
        .$`Substance name` == "dicofol" ~ "benzenemethanol, 4-chloro-.alpha.-(4-chlorophenyl)-.alpha.-(trichloromethyl)-",
        .$`Substance name` == "dichloromethane" ~ "methane, dichloro-",
        .$`Substance name` == "diuron" ~ "urea, n'-(3,4-dichlorophenyl)-n,n-dimethyl-",
        .$`Substance name` == "gamma-hch" ~ "gamma-hch (gamma-hexachlorocyclohexane)",
        .$`Substance name` == "hexachloro-butadiene" ~ "hexachlorobutadiene",
        .$`Substance name` == "isoproturon" ~ "urea, n,n-dimethyl-n'-[4-(1-methylethyl)phenyl]-",
        .$`Substance name` == "naphtalene" ~ "naphthalene",
        .$`Substance name` == "octylphenols ((4-(1,1′,3,3′-tetramethylbutyl)-phenol))" ~ "phenol, 4-(1,1,3,3-tetramethylbutyl)-",
        .$`Substance name` == "pcb-101" ~ "cb101",
        .$`Substance name` == "pcb-118" ~ "cb118",
        .$`Substance name` == "pcb-138" ~ "cb138",
        .$`Substance name` == "pcb-153" ~ "cb153",
        .$`Substance name` == "pcb-180" ~ "cb180",
        .$`Substance name` == "pcb-28" ~ "cb28",
        .$`Substance name` == "pcb-52" ~ "cb52",
        .$`Substance name` == "pentachloro- benzene" ~ "pentachlorobenzene",
        .$`Substance name` == "quinoxyfen" ~ "quinoline, 5,7-dichloro-4-(4-fluorophenoxy)-",
        .$`Substance name` == "tbtin (tbsn+)" ~ "n-tributyltin ion", #following the source of the threshold, TBT is  36643-28-4 for sediment. https://www.ospar.org/documents?v=43227
        .$`Substance name` == "terbutryn" ~ "1,3,5-triazine-2,4-diamine, n2-(1,1-dimethylethyl)-n4-ethyl-6-(methylthio)-",
      ),
      Harmon_name= ifelse(is.na(Harmon_name),`Substance name`,Harmon_name))
    
    CAS<-cas_numbers()
    threshold<-left_join(threshold,CAS,by=c("Harmon_name"="Description"))%>%
      left_join(CAS,by= c("Harmon_name"="PARAM"))%>%
      mutate(CAS= ifelse(is.na(CAS.x),CAS.y,CAS.x),
             Description= ifelse(!is.na(Description),Description,Harmon_name),
             PARAM= ifelse(is.na(PARAM)& grepl("pcb",`Substance name`),Harmon_name,
                           ifelse(is.na(PARAM) & !grepl("pcb",`Substance name`),`Substance name`,PARAM)))%>%
      select(-CAS.x,-CAS.y)%>%
      distinct()
    
    threshold$`Substance name`<-gsub("’","'",threshold$`Substance name`)
    return(threshold)
  }
  
}
cas_numbers<- function(){
  
  ### Reading CAS numbers from ICES database to combine with PARAM
  
  # Read CAS numbers from ICES database
  CAS_ICES<-read.table(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","RECO_Export_10-48-2026-08-48-04.csv"),
                       sep=",",header=T,comment.char="",fileEncoding="UTF-8",quote='"')
  
  # Data preprocessing for CAS numbers
  CAS_ICES <- CAS_ICES %>%
    select(CAS=Code, Description) %>%
    mutate(Description = tolower(Description))%>%
    mutate(
      Description = gsub("\\(pbde\\s*\\d+\\)", "", Description),
      Description = gsub("\\(bde\\s*\\d+\\)", "", Description),
      Description = gsub("\\(cb\\s*\\d+\\)", "", Description),
      Description = gsub("\\(bddhx\\s*\\d+\\)", "", Description))%>%
    mutate(
      Description= ifelse(grepl("bromodiphenyl ether",Description),
                          gsub("ether.*", "ether", Description),
                          Description),
      Description = trimws(Description))
  
  # Read PARAM names from ICES database
  
  PARAM_ICES<- read.table(file.path(PATHS$input_pressure,"kemisk_forurening","reference_vaerdi","ICES_PARAM_names.csv"),
                          sep=",",header=T,comment.char="",fileEncoding="UTF-8",quote='"')
  
  PARAM_ICES <- PARAM_ICES %>%
    select(Code, Description ) %>%
    mutate(Code = tolower(Code),
           Description = tolower(Description)) %>%
    rename("PARAM" = "Code")%>%
    mutate(
      Description = ifelse(
        !grepl("\\+", PARAM) & grepl("(cb)\\d+", PARAM) | grepl("(bd)\\d+", PARAM) | grepl("(bde)\\d+", PARAM),
        gsub("\\s*\\([^)]+\\)$", "", Description), # Remove content within parentheses
        Description
      ),
      Description = trimws(Description)
    )
  
  # Combine CAS and PARAM data
  cas_nummer <- left_join(CAS_ICES,PARAM_ICES, by = "Description")%>%
    mutate(CAS= ifelse(Description=="not applicable","NA",CAS))
  
  # Remove unnecessary objects
  rm(CAS_ICES,PARAM_ICES)
  
  # Return combined data
  return(cas_nummer)
  
}

norm_organ <- function(x){ o <- tolower(trimws(tidyr::replace_na(as.character(x),"")))
dplyr::case_when(
  o %in% c("muskel","muscle","mu","file","filé","filet")                                  ~ "muscle",
  o %in% c("lever","liver","li")                                                           ~ "liver",
  o %in% c("bløddele","blöddele","mjukkropp","mjukdel","helkropp","whole","soft tissue","sb","wo","hel") ~ "whole",
  TRUE ~ "") }

basis_from_unit <- function(u){ e <- tolower(trimws(tidyr::replace_na(as.character(u),"")))
dplyr::case_when(
  stringr::str_detect(e, "vv")        ~ "W",   # µg/kg VV, mg.kg-1.vv-1
  stringr::str_detect(e, "ts|tv")     ~ "D",   # mg/kg TS, mg.kg-1.tv-1
  stringr::str_detect(e, "lipid|fett|fedt") ~ "L",
  TRUE ~ NA_character_) }                       # pct / NA -> ingen basis (droppes via manglende CAS)




EQR_CR <- function(CR, uselog=F){
  
  #' wrappper function for the 'real'
  #' EQR_CR calculation to take care
  #' of vectors of CR values
  EQR <- CR %>%
    lapply(.EQR_CR_single, uselog=uselog) %>%
    unlist()
  return(EQR)
}

.EQR_CR_single <- function(CR, uselog){
  #' function to normalise Contamination Ratio (CR)
  #' value to 0.0 - 1,0 scale
  require(dplyr)
  
  # boundaries for CR and their corresponding EQR values
  bnds <- c(0.1, 0.5, 1.0, 5,  10,  50)
  eqrs <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0)
  
  # log-transform CR and boundaries
  if(uselog){
    # it could cause confusion if we use log-linear interpolation
    # all other EQR tools use linear interpolation
    
    CR_log <- log10(CR)
    bnds_log <- log10(bnds)
  }else{
    CR_log <- CR
    bnds_log <- bnds
  }
  
  # get the number of boundaries less than CR
  i <- bnds_log[CR_log>=bnds_log] %>% length()
  
  if(i<1){
    # CR_log is less than  the lowest boundary value
    EQR <- 1
  }else if(i==length(bnds_log)){
    # CR_log is greater than  the highest boundary value
    EQR <- 0
  }else{
    bnds0 <- bnds_log[i]
    bnds1 <- bnds_log[i+1]
    eqr0 <- eqrs[i]
    eqr1 <- eqrs[i+1]
    EQR <- eqr0 + (eqr1-eqr0) * ((CR_log - bnds0)/(bnds1- bnds0))
  }
  return(EQR)
}


# ----------- tests ------------------- 

if(sys.nframe() == 0L) {
  #' this if() checks if the script 
  #' is called from source()
  #' in that case, the test code is not run
  
  # calling the function with a vector
  CR=c(0.12, 0.67, 1.1, 7.3, 34.2)
  EQR_CR(CR)
  
  # using the function in mutate() with a data frame
  df_test <- data.frame(ID=c(1,2,3,4,5), 
                        CR=c(0.12, 0.67, 1.1, 7.3, 34.2))
  
  df_test <- df_test %>%
    mutate(EQR = EQR_CR(CR)) 
  
  print(df_test)
  
}  
