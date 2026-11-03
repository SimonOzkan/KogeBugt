Sys.setlocale("LC_ALL", "en_US.UTF-8")
source_folder<-"C:/Data/Repositories/CHASE/ETC_CHASE_2024/"


# Function to combine CAS numbers from ICES database with PARAM
cas_numbers<- function(){
  
  ### Reading CAS numbers from ICES database to combine with PARAM
  
  # Read CAS numbers from ICES database
  CAS_ICES<- read.table(file=paste0(source_folder,"raw_data/ICES_CAS_numbers.csv"),
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
  PARAM_ICES<- read.table(file=paste0(source_folder,"raw_data/ICES_PARAM_names.csv"),
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
#function to read in the data from all databases
read_and_clean <- function(filepath, sep=";", header = TRUE, na.strings = c(""," ", "NA"), encoding = "UTF-8", quote = "") {
  read.table(filepath, sep = sep, header = header, na.strings = na.strings, fileEncoding = encoding, quote = quote) %>%
    distinct() %>%
    mutate(across(everything(), ~replace_na(.)))
}

#function to have all datasets with the same format for dates.
standardize_dates <- function(df, date_col, format) {
  df %>%
    mutate(across(all_of(date_col), ~ as.Date(., format = format), .names = "DATE"))
}
reference_param_sed <- function(df){
  source<-unique(df$Source)
  
  if (source %in% c("ICES","EMODNET")){
    df<-df%>%rename("tblID"="tblSampleID")
  }else{
    df<-df%>%rename("tblID"= "tblAnalysisID") 
  }
  
  required_columns <- c("PARAM", "MUNIT", "Value", 
                        "Latitude", "Longitude", "MATRX", "BASIS", "DATE", "DEPHU", "tblID")
  missing_columns <- setdiff(required_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop("The following columns are missing in the DataFrame:\n",
         paste("- ", missing_columns, collapse = "\n"))
  } 
  
  #REF PARAM CORG for organic compounds
  
  CORG  <- df%>%
    filter(PARAM %in% c("corg","organic carbon"))
  
  corg_script_units<-c("%","g/kg","mg/g","mg/kg","ug/kg")
  missing_units<- setdiff(sort(unique(CORG$MUNIT)),corg_script_units)
  if (length(missing_units) > 0) {
    stop("CORG units have not been transformed:\n",
         paste("- ", missing_units, collapse = "\n"))
  } 
  
  CORG <- CORG %>%
    mutate(CORG_pct = case_when(
      MUNIT == "g/kg"   ~ Value / 10,
      MUNIT == "ug/kg"  ~ Value / 10000000,
      MUNIT == "mg/g"   ~ Value / 10,
      MUNIT == "mg/kg"  ~ Value / 10000,
      TRUE ~ Value
    )) %>%
    group_by(Latitude, Longitude, MATRX, DATE, tblID) %>%
    summarise(CORG_pct = mean(CORG_pct), .groups = "drop")
  
  
  df1  <- df%>%
    filter(!(PARAM %in% c("corg","organic carbon")))%>%
    left_join(CORG, by= c("Latitude","Longitude","MATRX","DATE","tblID"))
  
  #REF PARAM AL for metals
  
  AL  <- df%>%
    filter(PARAM %in% c("al","aluminium"))
  
  al_script_units<-c("%","g/kg", "mg/kg", "ug/g", "ug/kg")
  missing_units<- setdiff(sort(unique(AL$MUNIT)),al_script_units)
  if (length(missing_units) > 0) {
    stop("AL units have not been transformed:\n",
         paste("- ", missing_units, collapse = "\n"))
  } 
  
  AL <- AL %>%
    mutate(AL_pct = case_when(
      MUNIT == "g/kg"   ~ Value / 10,
      MUNIT == "ug/kg"  ~ Value / 10000000,
      MUNIT == "ug/g"   ~ Value / 10000,
      MUNIT == "mg/kg"  ~ Value / 10000,
      TRUE ~ Value
    )) %>%
    group_by(Latitude, Longitude, MATRX, DATE, tblID) %>%
    summarise(AL_pct = mean(AL_pct), .groups = "drop")
  
  
  df1  <- df1%>%
    filter(!(PARAM %in% c("al","aluminium")))%>%
    left_join(AL, by= c("Latitude","Longitude","MATRX","DATE","tblID"))
  
  if (source %in% c("ICES","EMODNET")){
    df1<-df1%>%
      rename("tblSampleID"="tblID")
  }else{
    df1<-df1%>%
      rename("tblAnalysisID"= "tblID") 
  }
  
}


reference_state_sed <- function(df){
  source<-unique(df$Source)
  if (source %in% c("ICES","WISE")){
    df<-df%>%
      rename("tblID"="tblSampleID")

  }else{
    df<-df%>%
      rename("tblID"= "tblAnalysisID") 
  }
  
  
  #BASIS drywt
  drywt  <- df%>%
    filter(PARAM %in% c("drywt%","drywt"))%>%
    rename("DRYWT"="Value")%>%
    group_by(Latitude,Longitude,MATRX,DATE,tblID,DEPHU)%>%
    summarise(n=n(),DRYWT= mean(DRYWT), .groups = "drop")%>%select(-n)
  
  df1  <- df%>%
    filter(!(PARAM %in% c("drywt%","drywt")))%>%
    left_join(drywt, by= c("Latitude","Longitude","MATRX","DATE","tblID","DEPHU"),na_matches = "never")%>%
    distinct()
  
  if (source %in% c("ICES")){
    df1<-df1%>%
      rename("tblSampleID"="tblID")
  }else{
    df1<-df1%>%
      rename("tblAnalysisID"= "tblID") 
  }


}

reference_state_bio <- function(df){
  source<-unique(df$Source)
  if (source %in% c("ICES","WISE")){
    df<-df%>%
      rename("tblID"="tblBioID")
    
  }else{
    df<-df%>%
      rename("tblID"= "tblAnalysisID") 
  }
  
  
  #BASIS 
  #D	Dry weight
  #L	Lipid weight (i.e., fat weight)
  #W	Wet weight (i.e., fresh weight)
  df<-df%>%
    mutate(PARAM=tolower(PARAM))
  
  drywt  <- df%>%
    filter(PARAM %in% c("drywt%","drywt","water","WETWT"))%>%
    rename("DRYWT"="Value")%>%
    mutate(DRYWT= ifelse(PARAM=="water",DRYWT-100,DRYWT))%>%#emodnet data has "water" data as the content of water(%) in the sample
    group_by(Latitude,Longitude,Species,MATRX,DATE,tblID)%>%
    summarise(n=n(),DRYWT= mean(DRYWT), .groups = "drop")%>%select(-n)
  
  df1  <- df%>%
    filter(!(PARAM %in% c("drywt%","drywt","water")))%>%
    left_join(drywt, by= c("Latitude","Longitude","Species","MATRX","DATE","tblID"))%>%
    distinct()
  
  lipidwt <- df1%>%
    filter(PARAM %in% c("fatwt%","lipidwt%"))%>%
    rename("FATWT"="Value")%>%
    group_by(Latitude,Longitude,Species,MATRX,DATE,tblID)%>%
    summarise(n=n(),FATWT= mean(FATWT), .groups = "drop")%>%select(-n)
  df1  <- df1%>%
    filter(!(PARAM %in% c("fatwt%","lipidwt%")))%>%
    left_join(lipidwt, by= c("Latitude","Longitude","Species","MATRX","DATE","tblID"))%>%
    distinct()
  
  #MATRIX
  #LI	  Liver
  #MU	  Muscle
  #MU&EP	  Muscle and epidermis (skin)
  #SB	  Whole soft-body
  #SH   Shell
  #WO	  Whole organism
  # lisoi <- df1%>%
  #   filter(PARAM %in% c("lisoi"))%>%
  #   rename("LISOI"="Value")%>%
  #   group_by(Latitude,Longitude,Species,MATRX,BASIS,DATE,tblID)%>%
  #   summarise(LISOI= mean(LISOI), .groups = "drop")
  # df1  <- df1%>%
  #   filter(!(PARAM %in% c("lisoi")))%>%
  #   left_join(lisoi, by= c("Latitude","Longitude","Species","MATRX","BASIS","DATE","tblID"))%>%
  #   distinct()
  df1<-df1%>%
    mutate(MATRX= case_when(
      .$MATRX=="flesh" ~ "SB",
      .$MATRX=="muscle tissue" ~ "MU",
      .$MATRX=="liver" ~ "LI",
      TRUE ~MATRX
    ))%>%
    filter(MATRX %in% c("LI","MU","MU&EP","SB","SH","WB","WO",NA))
  
  
  
  if (source %in% c("ICES")){
    df1<-df1%>%
      rename("tblBioID"="tblID")
  }else{
    df1<-df1%>%
      rename("tblAnalysisID"= "tblID") 
  }
}


add_grid <- function (df){
  user <- Sys.getenv("USERNAME")
  shp_folder <- paste0("assessment grid/")
  shp <- read_sf(paste0(shp_folder, "assessment_grid_100_20_minus_land.shp")) %>%
    select(GRIDCODE, Region, Subregn,Sbrg_cd)
  shp<-st_zm(shp)
  crs_grid <- st_crs(shp)
  df<-df%>%mutate(lon=longitude,lat=latitude)
  df <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  df <- st_transform(df, crs = crs_grid)
  
  df_grid <- sf::st_intersection(shp, df)
  
  rm(crs_grid, df)
  
  df_grid$geometry <- NULL
  
  return(df_grid)
  
  
}

check_parameter <- function(df) {
  keywords <- c("pcb", "cb", "bd", "ddt","hexabromo")
  found <- FALSE
  
  for (keyword in keywords) {
    if (any(grepl(keyword, df$Substance, ignore.case = TRUE))) {
      found <- TRUE
      break
    }
  }
  
  if (found) {
    print("Data matched.")
  } else {
    cat("Parameters like", paste('"', keywords, '"', collapse = ", "), "missing. Check the code.\n")
  }
}


parameter_harmonization<- function(df,folder){
  
  
  param<-read.csv(paste0(source_folder,"raw_data/nomenclature_parameter.csv"),sep=";",na.strings = "", encoding = "utf-8",header=T)%>%
    rename("SubstGroup"=1)
  
  param_list<-sort(unique(unlist(param,use.names = F)))
  
  df$Description <- gsub("’", "'", df$Description) #some of the names have strange symbols, so lets try to use the same for all of them
  param$EMODNET <- gsub("’", "'", param$EMODNET)
  param$WISE <- gsub("’", "'", param$WISE)
  df$Description <- gsub("<u\\+2019>","'", df$Description)
  
  param_ices<-param%>%select(-EMODNET,-WISE)%>%distinct()
  df_source1<-df%>%
    mutate(CAS=ifelse(CAS=="",NA,CAS))%>%
    filter(Source=="ICES")%>%
    filter(Description %in% param_list |PARAM %in% param_list)%>%
    left_join(param_ices,by=c("PARAM"="ICES"),na_matches = "never",relationship = "many-to-many")%>%
    mutate(CAS= ifelse(!is.na(CAS.x),CAS.x,CAS.y))%>%
    select(-CAS.x,-CAS.y)%>%distinct()
  na_val<- sum(is.na(df_source1$Substance))
  if (na_val==0) {
    print("ICES parameters complete.")
  }else{
    print("ICES parameters incomplete, check code.")
  }
  param_emod<-param%>%select(-ICES,-WISE)%>%distinct()
  df_source2<-df%>%
    mutate(CAS=ifelse(CAS=="",NA,CAS))%>%
    filter(Source=="EMODNET")%>%
    mutate(Description= ifelse(CAS %in% c("117-81-7"),"bis(2-ethylhexyl)phthalate",Description),
           Description= ifelse(Description=="octachloro-dibenzofuran (group)", "1,2,3,4,6,7,8,9-octachlorodibenzofuran",Description))%>%
    filter(Description %in% param_list | PARAM %in% param_list)%>%
    left_join(param_emod,by=c("Description"="EMODNET"),na_matches = "never",relationship = "many-to-many")%>%
    mutate(CAS= ifelse(!is.na(CAS.x),CAS.x,CAS.y))%>%
    select(-CAS.x,-CAS.y)%>%distinct()
  na_val<- sum(is.na(df_source2$Substance))
  if (na_val==0) {
    print("EMODNET parameters complete.")
  }else{
    print("EMODNET parameters incomplete, check code.")
  }
  param_wise<-param%>%select(-ICES,-EMODNET)%>%distinct()
  df_source3<-df%>%
    mutate(CAS=ifelse(CAS=="",NA,CAS))%>%
    filter(Source=="WISE")%>%
    filter(Description %in% param_list | PARAM %in% param_list)%>%
    left_join(param,by=c("Description"="WISE"),na_matches = "never",relationship = "many-to-many")%>%
    mutate(CAS= ifelse(!is.na(CAS.x),CAS.x,CAS.y))%>%
    select(-CAS.x,-CAS.y)%>%distinct()
  na_val<- sum(is.na(df_source3$Substance))
  if (na_val==0) {
    print("WISE parameters complete.")
  }else{
    print("WISE parameters incomplete, check code.")
  }
  df<-bind_rows(df_source1,df_source2,df_source3)%>%select(-PARAM,-PARGROUP,-Description,-Region)
  
  df<-df%>%
    relocate(c(SubstGroup,Grouping,Multiplier,Substance_code,Substance,CAS),.before = Value )
  return(df)
  
  
}

threshold_values<-function(category){
  category<-tolower(category)
  groups<-read.table(paste0(source_folder,"thresholds/Group_table.csv"),sep=";",header = T)%>% #if Ignore = 1, the threshold is not being used (obsolete, not applicable...)
    mutate(`Substance.group`= tolower(`Substance.group`))
  
  
  threshold<- read_xlsx(path = paste0(source_folder,"thresholds/Threshold catalogue 2024.xlsx"), 
                        trim_ws = TRUE)%>%
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


#FUNCTIONS FOR PLOTTING 

grid_map<-function(df,df_integrated,palette=color_palette1,subtitle){
  assessment_map<-df
  categories<-c("Water","Sediment","Biota","Bioeffects","Integrated")
  
  assessment_map<-assessment_map%>%
    mutate(Status=factor(Status,levels=correct_order))
  group<-subst_group
  
  bg <- data.frame(x=c(560000, 560000, 6960000, 6960000, 560000),
                   y=c(774000, 6850000, 6850000, 774000, 774000)) %>%
    mutate(id=1) %>%
    sf::st_as_sf(coords=c("x","y"), crs=sf::st_crs(3035)) %>%
    group_by(id) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  map_list <- list()
  for (cate in categories){
    if (cate!="Integrated"){
      ds<-assessment_map%>%
        filter(Category==cate)%>%select(Status,GRIDCODE)%>%mutate(Status=as.character(Status),GRIDCODE=tolower(GRIDCODE))
      ds$geometry<-NULL
      ds<-left_join(grd,ds,by="GRIDCODE")%>%mutate(Status=ifelse(is.na(Status),"No Data",Status))
      
      
      
      p<-ggplot() +
        geom_sf(data=bg, fill="#EEEEEE") +
        geom_sf(data=europe_outline,fill="white",color=NA) +
        geom_sf(data=ds, aes(fill = Status),colour = alpha("grey", 0.3),show.legend=T) +
        scale_fill_manual(values = palette, breaks = correct_order,na.value = "white",drop=F)  +
        labs(fill = "Status",title = paste0(group," status - ",cate," assessment"),subtitle=subtitle) +
        annotation_scale(location = "bl", style = "ticks", width_hint = 0.4,
                         text_cex = 2,
                         text_family = "Calibri",height = unit(0.5, "cm"),) +guides(fill=guide_legend(nrow=1))+
        theme_minimal()+
        theme(
          text = element_text(size=25,family="Calibri"),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          plot.background = element_rect(fill="white",color="white"),
          panel.grid = element_blank(),
          panel.ontop = T,
          panel.background= element_rect(fill=NA,color="black")
        )+
        coord_sf(xlim = c(570000, 6950000), ylim = c(775000, 6840000),expand = FALSE)
      map_name <- paste0(group,"-",cate)
      map_list[[map_name]] <- p  # Store each plot in the list with the category as the name
      
    }else{
      integrated_map <- df_integrated %>%
        mutate(Status = factor(Status, levels = correct_order))
      
      p<-ggplot() +
        geom_sf(data=bg, fill="#EEEEEE") +
        geom_sf(data=europe_outline,fill="white",color=NA) +
        geom_sf(data=integrated_map, aes(fill = Status),colour = alpha("grey", 0.3),show.legend=T) +
        scale_fill_manual(values = palette, breaks = correct_order,na.value = "white",drop=F)  +
        labs(fill = "Status",title = paste0(group," status - ",cate," assessment"),subtitle=subtitle) +
        annotation_scale(location = "bl", style = "ticks", width_hint = 0.4,
                         text_cex = 2,
                         text_family = "Calibri",height = unit(0.5, "cm"),) +guides(fill=guide_legend(nrow=1))+
        guides(fill=guide_legend(nrow=1))+
        theme_minimal()+
        theme(
          text = element_text(size=25,family="Calibri"),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          plot.background = element_rect(fill="white",color="white"),
          panel.grid = element_blank(),
          panel.ontop = T,
          panel.background= element_rect(fill=NA,color="black")
        )+
        coord_sf(xlim = c(570000, 6950000), ylim = c(775000, 6840000),expand = FALSE)
      map_name <- paste0(group,"-",cate)
      map_list[[map_name]] <- p
      
    }
  }
  return(map_list) 
}

grid_map_general<-function(df,df_integrated,palette=color_palette1){
  
  
  assessment_map<-df
  categories<-c("Water","Sediment","Biota","Bioeffects","Integrated")
  
  assessment_map<-assessment_map%>%
    mutate(Status=factor(Status,levels=correct_order))
  
  
  bg <- data.frame(x=c(560000, 560000, 6960000, 6960000, 560000),
                   y=c(774000, 6850000, 6850000, 774000, 774000)) %>%
    mutate(id=1) %>%
    sf::st_as_sf(coords=c("x","y"), crs=sf::st_crs(assessment_map)) %>%
    group_by(id) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  map_list <- list()
  for (cate in categories){
    if (cate!="Integrated"){
      ds<-assessment_map%>%
        filter(Category==cate)%>%select(Status,GRIDCODE)%>%mutate(Status=as.character(Status),GRIDCODE=tolower(GRIDCODE))
      ds$geometry<-NULL
      ds<-left_join(grd,ds,by="GRIDCODE")%>%mutate(Status=ifelse(is.na(Status),"No Data",Status))
      ds<-ds%>%
        mutate(Status=factor(Status,levels=correct_order),
               mode = case_when(
                 Status %in% c("High", "Good") ~ "Non-problematic",
                 Status %in% c("Moderate", "Poor", "Bad") ~ "Problematic"
               ))
      ds$mode <- factor(ds$mode, levels = c("Non-problematic", "Problematic"))
      
      grd2<-st_read("assessment grid/assessment_grid_100_20_minus_land.shp")
      p<-ggplot() +
        geom_sf(data=bg, fill="#DDEEEE") +
        geom_sf(data=europe_outline,fill="white",color=NA) +
        geom_sf(data=subset(grd2,Include=="N"),fill="#EEEEEE",color="white") +
        geom_sf(data=ds, aes(fill = Status), colour = alpha("grey", 0.3),show.legend=T) +
        scale_fill_manual(values = palette, breaks = correct_order,na.value = "white",drop=F)  +
        labs(fill = "Status") +
        annotation_scale(location = "bl", style = "ticks", width_hint = 0.4,
                         text_cex = 2,
                         text_family = "Calibri",height = unit(0.5, "cm"),) +
        guides(fill=guide_legend(nrow=1))+
        theme_minimal()+
        theme(
          text = element_text(size=25,family="Calibri"),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          plot.background = element_rect(fill="white",color="white"),
          panel.grid = element_blank(),
          panel.ontop = T,
          panel.background= element_rect(fill=NA,color="black")
        )+
        coord_sf(xlim = c(570000, 6950000), ylim = c(775000, 6840000),expand = FALSE)
      map_name <- paste0(cate)
      map_list[[map_name]] <- p  # Store each plot in the list with the category as the name
      
    }else{
      integrated_map <- df_integrated %>%
        mutate(Status = factor(Status, levels = correct_order),
               mode = case_when(
                 Status %in% c("High", "Good") ~ "Non-problematic",
                 Status %in% c("Moderate", "Poor", "Bad") ~ "Problematic"
               ))
      integrated_map$mode <- factor(integrated_map$mode, levels = c("Non-problematic", "Problematic"))
      
      p<-ggplot() +
        geom_sf(data=bg, fill="#DDEEEE") +
        geom_sf(data=europe_outline,fill="white",color=NA) +
        geom_sf(data=subset(grd2,Include=="N"),fill="#EEEEEE",color="white") +
        geom_sf(data=integrated_map, aes(fill = Status), colour = alpha("grey", 0.3),show.legend=T) +
        scale_fill_manual(values = palette, breaks = correct_order,na.value = "white",drop=F)  +
        labs(fill = "Status") +
        annotation_scale(location = "bl", style = "ticks", width_hint = 0.4,
                         text_cex = 2,
                         text_family = "Calibri",height = unit(0.5, "cm"),) +
        guides(fill=guide_legend(nrow=1))+
        theme_minimal()+
        theme(
          text = element_text(size=25,family="Calibri"),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          plot.background = element_rect(fill="white",color="white"),
          panel.grid = element_blank(),
          panel.ontop = T,
          panel.background= element_rect(fill=NA,color="black")
        )+
        coord_sf(xlim = c(570000, 6950000), ylim = c(775000, 6840000),expand = FALSE)
      map_name <- paste0(cate)
      map_list[[map_name]] <- p
      
    }
  }
  return(map_list) 
}

  

bar_map1<-function(df,df_integrated,palette=color_palette2,subtitle){
  
  categories<-c("Water","Sediment","Biota","Bioeffects","Integrated")
  reg<- c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean")
  integrated_summaries <- df_integrated %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,WorstCat,Status)%>%
    group_by(Region, Status) %>%
    summarise(count = n(),area=sum(area_km2),.groups="drop") 
  integrated_summaries$geometry<-NULL
  
  
  integrated_summaries_wide<-integrated_summaries%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= "Integrated")%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    arrange(Status)
  
  
  category_summaries<-df %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,Category,Status)%>%
    group_by(Region, Category,Status) %>%
    summarise(count = n(),.groups="drop")
  
  category_summaries$geometry<-NULL
  category_summaries_wide<-category_summaries%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= factor(Category, levels= categories))%>%
    arrange(Category,Status)%>%
    filter(Status!="No Data")
  
  all_summaries <- bind_rows(category_summaries_wide, integrated_summaries_wide)
  
  i<-names(all_summaries)
  
  check<-reg[!reg %in% i]
  
  if (length(check>0)){
    for(col in check) {
      all_summaries[[col]] <- NA
    }
  }
  bar_list <- list()
  #plot
  bar_summary <- all_summaries %>%
    pivot_longer(cols = c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean"),
                 names_to = "Region", values_to = "count") %>%
    mutate(Category = factor(Category, levels = unique(all_summaries$Category)),
           Status=factor(Status,levels=correct_order))
  df_factors<-df_factors%>%filter(Region!="Europe seas")
  bar_summary<-left_join(df_factors,bar_summary,by=c("Region","Status"))
  
  p<-ggplot()+
    geom_col(data=bar_summary,aes(x=Category,y=count,fill=Status),position = "dodge",width = 0.7)+
    scale_fill_manual(values = palette,limits = correct_order,drop=F) +
    scale_x_discrete(limits = c("Water", "Sediment", "Biota","Bioeffects", "Integrated")) +
    scale_y_continuous(expand = c(0,0))+
    labs(y="Number of assessment units",title = subst_group,subtitle=subtitle)+
    theme_minimal()+
    facet_wrap(~Region,ncol=1,scales = "free",strip.position = "top")+
    theme(legend.position = "bottom",
          plot.background = element_rect(fill="white",color="white"),
          panel.grid = element_blank(),
          axis.line.x.bottom = element_line(color="black", size = 0.2),
          axis.line.y.left = element_line(color="black", size = 0.2),
          axis.ticks.y.left = element_line(color="black", size = 0.2),
          axis.ticks.x.bottom = element_line(color="black", size = 0.2),
          strip.placement = "outside",
          strip.text = element_text(hjust=0,size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20),
          subtitle = element_text(size=25),
          title = element_text(size=25),
          panel.spacing = unit(1, "lines"),
          axis.title = element_text(size=18))
  map_name <- paste0(subst_group,"_barplot")
  bar_list[[map_name]] <- p  # Store each plot in the list with the category as the name
  
  return(bar_list)
}

bar_map1_general<-function(df,df_integrated,regions=reg,palette=color_palette2){
  
  categories<-c("Water","Sediment","Biota","Bioeffects","Integrated")
  integrated_summaries <- df_integrated %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,WorstCat,Status)%>%
    group_by(Region, Status) %>%
    summarise(count = n(),area=sum(area_km2),.groups="drop") 
  integrated_summaries$geometry<-NULL
  
  integrated_summaries<-integrated_summaries%>%
    group_by(Region)%>%
    mutate(tot_area=sum(area))%>%
    ungroup()%>%
    mutate(pct_area=area/tot_area)
  
  integrated_summaries2<-integrated_summaries%>%
    mutate(state=ifelse(Status %in% c("High","Good"),"Non-P","P"))%>%
    group_by(state)%>%
    summarise(area=sum(area))%>%
    ungroup()%>%
    mutate(tot_area=sum(area))%>%
    ungroup()%>%
    mutate(pct_area=area/tot_area)
  
  integrated_summaries_wide<-integrated_summaries%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= "Integrated")%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    arrange(Status)
  
  
  category_summaries<-df %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,Category,Status)%>%
    group_by(Region, Category,Status) %>%
    summarise(count = n(),area=sum(area_km2),.groups="drop")
  
  category_summaries$geometry<-NULL
  
  category_summaries<-category_summaries%>%
    group_by(Region,Category)%>%
    mutate(tot_area=sum(area))%>%
    ungroup()%>%
    mutate(pct_area=area/tot_area)
  
  category_summaries_wide<-category_summaries%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= factor(Category, levels= categories))%>%
    arrange(Category,Status)%>%
    filter(Status!="No Data")
  
  all_summaries <- bind_rows(category_summaries_wide, integrated_summaries_wide)
  
  i<-names(all_summaries)
  
  check<-reg[!reg %in% i]
  
  if (length(check>0)){
    for(col in check) {
      all_summaries[[col]] <- NA
    }
  }
  bar_list <- list()
  #plot
  bar_summary <- all_summaries %>%
    pivot_longer(cols = c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean"),
                 names_to = "Region", values_to = "count") %>%
    mutate(Category = factor(Category, levels = unique(all_summaries$Category)),
           Status=factor(Status,levels=correct_order),
           Region= factor(Region, levels= reg))
  df_factors<-df_factors%>%filter(Region!="Europe seas")
  bar_summary<-left_join(df_factors,bar_summary,by=c("Region","Status"))
  
  b<-ggplot()+
    geom_col(data=bar_summary,aes(x=Category,y=count,fill=Status),color="black",position = "dodge",width = 0.7)+
    scale_fill_manual(values = palette,limits = c("High","Good","Moderate","Poor","Bad"),drop=F) +
    scale_x_discrete(limits = c("Water", "Sediment", "Biota","Bioeffects", "Integrated")) +
    scale_y_continuous(expand = c(0,0))+
    labs(y="Number of assessment units")+
    guides(fill=guide_legend(nrow=1))+
    theme_minimal()+
    facet_wrap(~Region,ncol=1,scales = "free",strip.position = "top")+
    theme(legend.position = "bottom",
          plot.background = element_rect(fill="white",color="white"),
          panel.grid = element_blank(),
          axis.line.x.bottom = element_line(color="black", size = 0.2),
          axis.line.y.left = element_line(color="black", size = 0.2),
          axis.ticks.y.left = element_line(color="black", size = 0.2),
          axis.ticks.x.bottom = element_line(color="black", size = 0.2),
          strip.placement = "outside",
          strip.text = element_text(hjust=0,size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20),
          legend.box.background = element_rect(colour = "black"),
          title = element_text(size=25),
          panel.spacing = unit(1, "lines"),
          axis.title.y = element_text(size=18),
          axis.title.x = element_blank(),
          axis.text = element_text(size=18),
          text = element_text(family="Calibri"))
  #map_name <- paste0(subst_group,"_barplot")
  #bar_list[[map_name]] <- p  # Store each plot in the list with the category as the name
  
  return(b)
}


bar_map2<-function(df,df_integrated,palette=color_palette2,subtitle){
  
  categories<-c("Water","Sediment","Biota","Bioeffects","Integrated")
  reg<- c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean")
  integrated_summaries <- df_integrated %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,WorstCat,Status)%>%
    group_by(Region, Status) %>%
    summarise(count = n(),.groups="drop") 
  integrated_summaries$geometry<-NULL
  
  
  integrated_summaries_wide<-integrated_summaries%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= "Integrated")%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    arrange(Status)
  
  
  category_summaries<-df %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,Category,Status)%>%
    group_by(Region, Category,Status) %>%
    summarise(count = n(),.groups="drop")
  
  category_summaries$geometry<-NULL
  category_summaries_wide<-category_summaries%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= factor(Category, levels= categories))%>%
    arrange(Category,Status)%>%
    filter(Status!="No Data")
  
  all_summaries <- bind_rows(category_summaries_wide, integrated_summaries_wide)
  
  i<-names(all_summaries)
  
  check<-reg[!reg %in% i]
  
  if (length(check>0)){
    for(col in check) {
      all_summaries[[col]] <- NA
    }
  }
  bar_list_i<-list()
  #plot
  bar_summary <- all_summaries %>%
    pivot_longer(cols = c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean"),
                 names_to = "Region", values_to = "count") %>%
    mutate(Category = factor(Category, levels = unique(all_summaries$Category)),
           Status=factor(Status,levels=correct_order))
  
  bar_summary1<-bar_summary%>%filter(Category=="Integrated")
  bar_summary2<-bar_summary%>%filter(Category=="Integrated")%>%group_by(Status,Category)%>%summarise(count=sum(count,na.rm = T),.groups = "drop")%>%
    mutate(Region="Europe seas")
  bar_summary<-bind_rows(bar_summary1,bar_summary2)
  
  bar_summary<-left_join(df_factors,bar_summary,by=c("Region","Status"))
  bar_summary$Region<-factor(bar_summary$Region,levels=c(reg,"Europe seas"))
  
  
  p1<-ggplot()+
    geom_col(data=bar_summary,aes(x=Category,y=count,fill=Status),position = "dodge",width = 0.7)+
    scale_fill_manual(values = palette,limits = correct_order,drop=F) +
    scale_x_discrete(limits = c("Integrated")) +
    scale_y_continuous(expand = c(0,0))+
    labs(y="Number of assessment units",x="",title=paste0(subst_group," assessment by Region"),subtitle=subtitle)+
    theme_minimal()+
    facet_wrap(~Region,nrow=1,scales = "free",strip.position = "top",drop = F)+
    theme(legend.position = "bottom",
          plot.background = element_rect(fill="white",color="white"),
          panel.grid = element_blank(),
          axis.line.x.bottom = element_line(color="black", size = 0.2),
          axis.line.y.left = element_line(color="black", size = 0.2),
          axis.ticks.y.left = element_line(color="black", size = 0.2),
          axis.ticks.x.bottom = element_line(color="black", size = 0.2),
          strip.placement = "outside",
          strip.text = element_text(hjust=0,size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20),
          subtitle = element_text(size=25),
          title = element_text(size=25),
          panel.spacing = unit(1, "lines"),
          axis.title = element_text(size=18))
  map_name <- paste0(subst_group,"_barplot")
  bar_list_i[[map_name]] <- p1  # Store each plot in the list with the category as the name
  return(bar_list_i)
}



table_map2<-function(df,df_integrated,palette=color_palette2,subtitle){
  
  categories<-c("Water","Sediment","Biota","Bioeffects","Integrated")
  reg<- c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean")
  integrated_summaries <- df_integrated %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,WorstCat,Status)%>%
    group_by(Region, Status) %>%
    summarise(count = n(),.groups="drop") 
  integrated_summaries$geometry<-NULL
  
  
  integrated_summaries_wide<-integrated_summaries%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= "Integrated")%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    arrange(Status)
  
  
  category_summaries<-df %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,Category,Status)%>%
    group_by(Region, Category,Status) %>%
    summarise(count = n(),.groups="drop")
  
  category_summaries$geometry<-NULL
  category_summaries_wide<-category_summaries%>%
    pivot_wider(names_from = Region,values_from = count)%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= factor(Category, levels= categories))%>%
    arrange(Category,Status)%>%
    filter(Status!="No Data")
  
  all_summaries <- bind_rows(category_summaries_wide, integrated_summaries_wide)
  
  i<-names(all_summaries)
  
  check<-reg[!reg %in% i]
  
  if (length(check>0)){
    for(col in check) {
      all_summaries[[col]] <- NA
    }
  }
  #plot
  bar_summary <- all_summaries %>%
    pivot_longer(cols = c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean"),
                 names_to = "Region", values_to = "count") %>%
    mutate(Category = factor(Category, levels = unique(all_summaries$Category)),
           Status=factor(Status,levels=correct_order))
  
  bar_summary1<-bar_summary%>%filter(Category=="Integrated")
  bar_summary2<-bar_summary%>%filter(Category=="Integrated")%>%group_by(Status,Category)%>%summarise(count=sum(count,na.rm = T),.groups = "drop")%>%
    mutate(Region="Europe seas")
  bar_summary<-bind_rows(bar_summary1,bar_summary2)
  
  bar_summary<-left_join(df_factors,bar_summary,by=c("Region","Status"))
  table_sum<-bar_summary%>%filter(Region=="Europe seas" & Status!="No Data")%>%mutate(Total=sum(count,na.rm=T),
                                                                                      State= ifelse(Status %in% c("High","Good"),"No problematic","Problematic"))
  total<-unique(table_sum$Total[!is.na(table_sum$Total)])
  
  table_sum<-table_sum%>%
    select(State,count)%>%
    group_by(State)%>%
    summarise(pct=(sum(count,na.rm=T)/total)*100)%>%distinct()
  
  return(table_sum)
}




#FUNCTIONS FOR DATA GATHERING

#WISE
#function to get all the URL for all the pages, so we get all the data
disco_url <- function(page, page_size=1000){
  url <- paste0("https://discodata.eea.europa.eu/sql?query=SELECT%20*%20FROM%20%5BWISE_SOE%5D.%5Blatest%5D.%5BWaterbase_T_WISE6_DisaggregatedData%5D%20WHERE%20%5BWISE_SOE%5D.%5Blatest%5D.%5BWaterbase_T_WISE6_DisaggregatedData%5D.%5BparameterWaterBodyCategory%5D%3D%27CW%27&p=", 
                page, "&nrOfHits=", page_size, "&mail=null&schema=null")
  return(url)
}
#function to get the data from the URL
get_disco <- function(url){
  res <- httr::GET(url)
  if(res$status_code==200){
    content <- httr::content(res,as="text",encoding = "utf-8")
    df <- jsonlite::fromJSON(content)
    df <- as.data.frame(df)
  }else{
    # something didn't work
    df <- data.frame()
    message("WISE Data Request failed.")
  }
  
  names(df) <- stringr::str_remove(names(df), "results\\.")
  
  return(df)
}
