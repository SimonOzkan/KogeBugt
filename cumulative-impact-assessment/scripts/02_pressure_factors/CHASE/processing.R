#source_folder<-"C:/Data/Repositories/CHASE/ETC_CHASE_2024/"

## ==================================================================
## TILPASNING (under_loq + limit):
##   Alle ICES- og WISE-funktioner bevarer nu et logisk flag `under_loq`
##   og den fulde grænse `limit` (LOD/LOQ) hele vejen til output.
##   Værdien (`Value`) beholdes ved den FULDE grænse for censurerede
##   rækker - selve LOD/2 (og "ikke-konklusiv"-tjekket) laves ét sted:
##   i chase_engine. EMODnet har ingen LOD/LOQ-kolonne, så der sættes
##   under_loq = FALSE og limit = NA_real_ for skema-ensartethed.
##   `>` (greater than) tælles IKKE som under grænsen - det er en
##   nedre grænse, ikke en censurering - så den beholdes som målt.
## ==================================================================

#sediments
process_sediments <- function(path) {
  user <- Sys.getenv("USERNAME")
  switch (path,
          ICESpath= process_ICES_sediment(),
          EMODNETpath= process_EMODNET_sediment(),
          WISEpath= process_WISE_sediment(),
          stop("Invalid source path provided. Only acceptable paths: ICESpath, EMODNETpath, WISEpath")
  )
}


process_ICES_sediment<-function (){
  
  #df_sedim_ICES<-read_and_clean(ICESpath,sep="\t")
  df_sedim_ICES <- read.table(file = ICESpath, sep = ",", header = TRUE,
                              comment.char = "", quote = "\"",
                              fileEncoding = "UTF-8-BOM")
  
  
  
  df_sedim_ICES<-standardize_dates(df_sedim_ICES,"DATE","%d/%m/%Y")
  #QFLAG
  #<	less than
  # >	greater than
  # D	reported value is less than the detection limit (DETLI)
  # Q	reported value is less than the limit of quantification (LMQNT)
  
  # VFLAG
  # A: means it has been checked and quality assured
  # c: calculated value
  df_sedim_ICES <- df_sedim_ICES %>%
    mutate(
      QFLAG = ifelse(QFLAG == "", NA, QFLAG),
      VFLAG = ifelse(VFLAG == "", NA, VFLAG),
      # censureret = under LOD/LOQ (D, <, <~D, Q), men ikke hvis efterprøvet/beregnet (VFLAG A/C)
      under_loq = !is.na(QFLAG) & QFLAG %in% c("D", "<", "<~D", "Q") &
        !(VFLAG %in% c("C", "A")),
      # grænsen for censurerede værdier: DETLI for D/<, ellers LMQNT
      limit = case_when(
        !under_loq                                   ~ NA_real_,
        QFLAG %in% c("D", "<", "<~D") & !is.na(DETLI) ~ DETLI,
        TRUE                                         ~ LMQNT),
      # rapporteret værdi: detekteret -> Value; censureret -> fuld grænse (motoren tager /2)
      Value = ifelse(under_loq, dplyr::coalesce(limit, Value), Value)) %>%
    select(Country,Station=STATN,DATE,Latitude,Longitude,DEPHU,DEPHL,PARGROUP,PARAM,MATRX,BASIS,Value,MUNIT,under_loq,limit,tblAnalysisID,tblParamID,tblSampleID)%>%
    mutate(Category = "Sediment",
           Source= "ICES",
           PARAM=tolower(PARAM))%>%
    filter(!is.na(PARAM))
  
  
  #add as their own column the values for the reference parameters: CORG, AL and the dry weight (%)
  #reference_state_sed extracts the dryweight of the datasets and adds it as a column.
  df_sedim_ICES<-reference_state_sed(df_sedim_ICES)
  #reference_param_sed  extracts the CORG and AL and adds it as a column.
  df_sedim_ICES<-reference_param_sed(df_sedim_ICES)
  
  df_sedim_ICES<-df_sedim_ICES%>%
    mutate(tblAnalysisID=as.character(tblAnalysisID),
           tblParamID=as.character(tblParamID),
           tblSampleID=as.character(tblSampleID),
           Latitude=as.numeric(Latitude),
           Longitude=as.numeric(Longitude),
           Station=as.character(Station))
  cat("ICES sediment data contains:", nrow(df_sedim_ICES),"rows.\n")
  
  
  return(df_sedim_ICES)
  
}
process_EMODNET_sediment <- function(path = EMODNETpath, sep = "\t", region = "Baltic") {
  
  ## --- indlæsning (quote="" pga. apostroffer; check.names=TRUE -> punkt-navne) ---
  EMODNET <- read.delim(path, sep = sep, quote = "", comment.char = "",
                        fill = TRUE, na.strings = c("", "NA", " "),
                        stringsAsFactors = FALSE)
  
  # SeaDataNet bruger blanktegn som "tom" -> NA
  EMODNET <- EMODNET %>%
    mutate(across(where(is.character), ~ { x <- trimws(.); ifelse(x == "", NA, x) }))
  
  ## --- grundfelter: dato/tid, koordinater, værdi ---
  df <- EMODNET %>%
    mutate(
      Value     = as.numeric(Value),
      DATE      = as.Date(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 10), "%Y-%m-%d"),
      Time      = substr(yyyy.mm.ddThh.mm.ss.sss, 12, 19),
      Time      = hms::as_hms(ifelse(is.na(Time) | Time == "", "00:00:00", Time)),
      MYEAR     = lubridate::year(DATE),
      Latitude  = as.numeric(Latitude..degrees_north.),
      Longitude = as.numeric(Longitude..degrees_east.),
      Longitude = ifelse(Longitude > 180, Longitude - 360, Longitude)
    ) %>%
    drop_na(Value)
  
  ## --- dybde i sediment: direkte fra DepBelowBed (punktprøve: øvre = nedre) ---
  df <- df %>%
    mutate(DEPHU = suppressWarnings(as.numeric(DepBelowBed..m.)),
           DEPHU = ifelse(is.na(DEPHU), 0, DEPHU),
           DEPHL = DEPHU)
  
  ## --- behold kun relevante måletyper (S06_preflabel findes allerede i filen) ---
  df <- df %>%
    filter(S06_preflabel %in% c("Concentration", "Dry weight",
                                "Proportion", "Proportion by dry weight", "Wet weight"),
           !grepl("particles", P01_preflabel))   # specifikke kornstørrelser udelades
  
  ## --- parameternavn (PARAM) ---
  df <- df %>%
    mutate(PARAM = tolower(S27_preflabel),
           PARAM = case_when(
             grepl("organic carbon", P01_preflabel, ignore.case = TRUE)        ~ "corg",
             P01_preflabel == "Dry weight of sample"                            ~ "drywt",
             P01_preflabel == "Wet weight of sample"                            ~ "wetwt",
             grepl("Proportion by dry weight", P01_preflabel, ignore.case = TRUE) ~ "drywt",
             PARAM == "benz(a)anthracene"                                       ~ "benzo(a)anthracene",
             PARAM == "alpha-hexachlorocyclohexane+beta-hexachlorocyclohexane+gamma-hexachlorocyclohexane"
             ~ "alpha+beta+gamma-hexachlorocyclohexane",
             TRUE ~ PARAM))
  
  ## --- sedimentstørrelse (MATRX) ---
  df <- df %>%
    filter(is.na(S26_preflabel) | (!grepl("pore water", S26_preflabel) &
                                     !S26_preflabel %in% c("geological sample", "wet sediment", "sediment 2500-5000um"))) %>%
    mutate(MATRX = case_when(
      S26_preflabel == "sediment <2000um" ~ "SED2000",
      S26_preflabel == "sediment <20um"   ~ "SED20",
      S26_preflabel == "sediment <250um"  ~ "SED250",
      S26_preflabel == "sediment <63um"   ~ "SED63",
      is.na(S26_preflabel) | S26_preflabel == "sediment" ~ "SEDTOT",
      TRUE ~ NA_character_))
  
  ## --- basis (BASIS); udeladelse af volumen-baserede ---
  df <- df %>%
    mutate(BASIS = case_when(
      S02_preflabel == "per unit dry weight of" ~ "D",
      S02_preflabel == "per unit wet weight of" ~ "W",
      S02_preflabel == "per unit volume of"     ~ "volume",
      is.na(S02_preflabel) & grepl("Dry weight", P01_preflabel) ~ "D",
      is.na(S02_preflabel) & grepl("Wet weight", P01_preflabel) ~ "W",
      TRUE ~ NA_character_)) %>%
    filter(is.na(BASIS) | BASIS != "volume")
  
  ## --- CAS + ren beskrivelse ---
  df <- df %>%
    mutate(CAS = gsub("\\.", "-", CAS.no),
           Description = P01_preflabel,
           Description = gsub("Concentration of ", "", Description),
           Description = gsub(" per unit dry weight of sediment", "", Description),
           Description = gsub("Proportion of ", "", Description),
           Description = gsub(" <2000um| <63um", "", Description),
           Description = gsub("\\{.*", "", Description),
           Description = trimws(Description))
  
  ## --- sample-/analyse-id robust (Sample identifier er en KOLONNE nu, ofte tom) ---
  df <- df %>%
    mutate(tblSampleID = dplyr::coalesce(
      as.character(Sample.identifier),
      paste(LOCAL_CDI_ID, ODV.internal.sample.number, sep = "_")),
      tblAnalysisID = as.character(CDI.record.id))
  
  ## --- saml i samme skema som før (EMODnet har ingen LOD/LOQ -> under_loq=FALSE, limit=NA) ---
  df_sedim_EMOD <- df %>%
    transmute(
      Station = as.character(Station),
      DATE, Time,
      Latitude, Longitude,
      DEPHU, DEPHL,
      PARAM, MATRX, BASIS,
      Value,
      MUNIT = Units,
      under_loq = FALSE,
      limit     = NA_real_,
      tblSampleID, tblAnalysisID,
      CAS,
      Description,
      Region   = region,
      Category = "Sediment",
      Source   = "EMODNET")
  
  df_sedim_EMOD <- reference_param_sed(df_sedim_EMOD)
  df_sedim_EMOD <- reference_state_sed(df_sedim_EMOD)
  
  cat("EMODNET sediment data contains:", nrow(df_sedim_EMOD), "rows.\n")
  df_sedim_EMOD   # lat/lon i decimalgrader (WGS84)
}

process_WISE_sediment<- function (){
  
  lon_lat<-read.csv(spatial_data_wise,sep="\t")
  lon_lat <- lon_lat %>%
    group_by(countryCode,Station) %>%
    summarise(Latitude=mean(lat,na.rm=T), Longitude=mean(lon,na.rm=T), .groups = "drop")%>%
    select(-countryCode)
  # If, for a specific sample value, the measured concentration is below the limit of quantification (LOQ), then leave the "Observed Value" field empty, and report 'True' (or '1') in the Boolean "Observed value below LOQ" field.
  
  WISE_prep<-read_and_clean(WISEpath,quote = "\"'")
  WISE_prep<-standardize_dates(WISE_prep,"phenomenonTimeSamplingDate","%Y-%m-%d")
  
  WISE_prep <- WISE_prep %>%
    rename("Station" ="monitoringSiteIdentifier",
           "MATRX" = "procedureAnalysedMatrix",
           "MUNIT" ="resultUom",
           "tblSampleID" = "sampleIdentifier",
           "CAS"= "observedPropertyDeterminandCode",
           "Description"= "observedPropertyDeterminandLabel")%>%
    mutate(under_loq = resultQualityObservedValueBelowLOQ == 1,
           limit     = suppressWarnings(as.numeric(procedureLOQValue)),
           Value= ifelse(resultQualityObservedValueBelowLOQ==0,resultObservedValue,procedureLOQValue),
           Category="Sediment")%>%
    select(-resultObservedValue,-procedureLOQValue,-resultQualityObservedValueBelowLOQ,
           -phenomenonTimeSamplingDate, -phenomenonTimeSamplingDate_year)%>%
    filter(MATRX %in% c("S","S-0063"))
  
  
  
  WISE_prep <-WISE_prep%>%
    left_join(lon_lat, by=c("Station")) %>%
    mutate(Source = "WISE",
           CAS= gsub("CAS_", "", CAS),
           Description=tolower(Description))%>%
    select_if(~!all(is.na(.)))
  
  #depth
  #Definitions:
  #ParameterSedimentDepthSampled: Depth of the collected sample of sediment, measured as a range, in centimeters, starting at the seafloor surface. The range would start by zero if the top of the sediment sample is the seafloor surface.
  #ParameterSampleDepth: Depth at which sample was taken in meter below water surface, in meters.
  
  #we are keeping the depth in the sediment
  WISE_prep <-WISE_prep%>%
    mutate(DEPHU= as.numeric(ifelse(grepl("-",parameterSedimentDepthSampled),substr(parameterSedimentDepthSampled,1,1),parameterSedimentDepthSampled)),
           DEPHL= as.numeric(ifelse(grepl("-",parameterSedimentDepthSampled),substr(parameterSedimentDepthSampled,3,3),parameterSedimentDepthSampled)),
           DEPHU= (DEPHU/100),
           DEPHL= (DEPHL/100))%>%
    select(-parameterSedimentDepthSampled,-parameterSampleDepth)
  
  #eea_wise<-WISE_prep%>%select(CAS,Description)%>%filter(grepl("EEA",CAS))%>%distinct()
  
  #harmonize CORG and AL, so after they can be foun with the function.
  df_sedim_WISE<-WISE_prep%>%
    mutate(PARAM= ifelse(grepl("aluminium",Description),"al",
                         ifelse(grepl("organic carbon",Description),"corg",Description)), #toc can be used as corg
           BASIS=ifelse(grepl("dry",Remarks),"D",NA),
           Value=as.numeric(Value)
    )%>%
    group_by(Station,parameterWaterBodyCategory,MATRX,metadata_beginLifeSpanVersion,DATE,Category,Source)%>%
    mutate(tblAnalysisID=cur_group_id())%>%
    ungroup()
  
  df_sedim_WISE<- reference_param_sed(df_sedim_WISE)
  
  df_sedim_WISE<-df_sedim_WISE%>%
    mutate(MYEAR=year(DATE))
  
  countrycode<-read.table(country_code_wise, sep=",",header=T)%>%
    select(Code=1,Label)
  
  df_sedim_WISE<-df_sedim_WISE%>%
    left_join(countrycode,by=c("countryCode"="Code"))%>%
    select(-countryCode)
  
  # sikkerhedsnet: hvis ingen censurerede rækker, kan select_if have droppet limit
  if (!"limit" %in% names(df_sedim_WISE))     df_sedim_WISE$limit <- NA_real_
  if (!"under_loq" %in% names(df_sedim_WISE)) df_sedim_WISE$under_loq <- FALSE
  
  df_sedim_WISE<-df_sedim_WISE%>%
    select(Station,
           DATE,
           Latitude,
           Longitude,
           DEPHU,
           DEPHL,
           Description,
           MATRX,
           BASIS,
           Value,
           MUNIT,
           under_loq,
           limit,
           AL_pct,
           CORG_pct,
           tblSampleID,
           tblAnalysisID,
           CAS,
           Country=Label,
           Category,
           Source
    )%>%
    distinct()%>%
    mutate(tblAnalysisID=as.character(tblAnalysisID),
           tblSampleID=as.character(tblSampleID),
           Latitude=as.numeric(Latitude),
           Longitude=as.numeric(Longitude),
           Station=as.character(Station),
           under_loq=as.logical(under_loq),
           limit=as.numeric(limit),
           Value=as.numeric(Value))
  
  cat("WISE sediment data contains:", nrow(df_sedim_WISE),"rows.\n")
  #latitude and longitude measurements are in decimal degrees.
  return(df_sedim_WISE)
}

#biota
process_biota <- function (path){
  user <- Sys.getenv("USERNAME")
  switch (path,
          ICESpath= process_ICES_biota(),
          EMODNETpath= process_EMODNET_biota(),
          WISEpath= process_WISE_biota(),
          stop("Invalid source path provided. Only acceptable paths: ICESpath, EMODNETpath, WISEpath")
  )
}


process_ICES_biota<- function(){
  df_biota_ICES <- read.csv(ICESpath,
                            header = TRUE,
                            fileEncoding = "UTF-8",
                            stringsAsFactors = FALSE,
                            check.names = TRUE)
  #QFLAG
  #<	less than
  # >	greater than
  # D	reported value is less than the detection limit (DETLI)
  # Q	reported value is less than the limit of quantification (LMQNT)
  
  # VFLAG
  # A: means it has been checked and quality assured
  # c: calculated value
  df_biota_ICES <- df_biota_ICES %>%
    mutate(
      QFLAG = ifelse(QFLAG == "", NA, QFLAG),
      VFLAG = ifelse(VFLAG == "", NA, VFLAG),
      under_loq = !is.na(QFLAG) & QFLAG %in% c("D", "<", "<~D", "Q") &
        !(VFLAG %in% c("C", "A")),
      limit = case_when(
        !under_loq                                   ~ NA_real_,
        QFLAG %in% c("D", "<", "<~D") & !is.na(DETLI) ~ DETLI,
        TRUE                                         ~ LMQNT),
      Value = ifelse(under_loq, dplyr::coalesce(limit, Value), Value)
    )
  
  df_biota_ICES<- df_biota_ICES%>%
    select(Country,Station=STATN,DATE,Latitude,Longitude,Species,NOINP,MATRX,PARGROUP,PARAM,BASIS,Value,MUNIT,under_loq,limit,SUBNO,tblAnalysisID,tblParamID,tblBioID,tblSampleID)%>%
    mutate(Category = "Biota",
           Source= "ICES",
           PARAM=tolower(PARAM))
  #if there is no matrx, we will asume whole organism
  #if there is no basis, we will asume wet weight
  df_biota_ICES<-df_biota_ICES%>%
    mutate(MATRX= ifelse(grepl("WO",MATRX),"SB", MATRX),
           MATRX= ifelse(is.na(MATRX)|MATRX=="","SB",MATRX),
           BASIS= ifelse(is.na(BASIS)|BASIS=="","W",BASIS))%>%
    filter(MATRX %in% c("MU", "LI", "SB"))
  
  df_biota_ICES<-reference_state_bio(df_biota_ICES)
  
  df_biota_ICES<-df_biota_ICES%>%
    mutate(tblAnalysisID=as.character(tblAnalysisID),
           tblParamID=as.character(tblParamID),
           tblSampleID=as.character(tblSampleID),
           tblBioID=as.character(tblBioID),
           Latitude=as.numeric(Latitude),
           Longitude=as.numeric(Longitude),
           DATE=as.Date(DATE,format="%d/%m/%Y"),
           Station=as.character(Station))
  cat("ICES biota data contains:", nrow(df_biota_ICES),"rows.\n")
  
  return(df_biota_ICES)
}

process_EMODNET_biota <- function(path = EMODNETpath, sep = "\t", region = "Baltic") {
  
  EMODNET <- read.delim(path, sep = sep, quote = "", comment.char = "",
                        na.strings = c("NA", "", " "),
                        fill = TRUE, stringsAsFactors = FALSE)   # check.names=TRUE -> punkt-navne
  
  EMODNET <- EMODNET %>%                                         # blanktegn -> NA
    mutate(across(where(is.character), ~ { x <- trimws(.); ifelse(x == "", NA, x) })) %>%
    distinct()
  
  ## --- sample-/subsample-id ligger som PARAMETERRÆKKER i denne eksport ---
  ids <- EMODNET %>%
    filter(P01_preflabel %in% c("Sample identifier", "Subsample identifier")) %>%
    mutate(idtype = ifelse(P01_preflabel == "Sample identifier", "SAMPID", "BIOID")) %>%
    select(LOCAL_CDI_ID, ODV.internal.sample.number, idtype, idval = Value) %>%
    distinct() %>%
    tidyr::pivot_wider(names_from = idtype, values_from = idval,
                       values_fn = ~ as.character(dplyr::first(.)))
  for (cc in c("SAMPID", "BIOID")) if (!cc %in% names(ids)) ids[[cc]] <- NA_character_
  
  ## --- kun rigtige biota-målinger (fjerner samtidig id-rækkerne, S26 = "not applicable") ---
  df <- EMODNET %>%
    filter(S26_preflabel == "biota") %>%
    drop_na(Value) %>%
    mutate(Value = suppressWarnings(as.numeric(Value)),
           Category = "Biota", Source = "EMODNET") %>%
    drop_na(Value)
  
  ## --- dato/tid + koordinater ---
  df <- df %>%
    mutate(Date = substr(yyyy.mm.ddThh.mm.ss.sss, 1, 10),
           Time = substr(yyyy.mm.ddThh.mm.ss.sss, 12, 19),
           Latitude  = as.numeric(Latitude..degrees_north.),
           Longitude = as.numeric(Longitude..degrees_east.),
           Longitude = ifelse(Longitude > 180, Longitude - 360, Longitude))
  
  ## --- frasortér ikke-koncentrationer (længde, aktivitet) ---
  df <- df %>% filter(!S06_preflabel %in% c("Length", "Activity"))
  
  ## --- PARAM (fra S27_preflabel), BASIS ---
  df <- df %>%
    mutate(PARAM = tolower(S27_preflabel),
           BASIS = case_when(
             S02_preflabel == "per unit mass of lipids in the" ~ "L",
             S02_preflabel == "per unit dry weight of"         ~ "D",
             S02_preflabel == "per unit wet weight of"         ~ "W",
             TRUE ~ S02_preflabel))
  
  ## --- CAS, Description, MATRX, Species (fra P01_preflabel / S25_preflabel) ---
  df <- df %>%
    mutate(
      CAS = gsub("\\.", "-", CAS.no),
      Description = sub(".*Concentration of\\s(.*?)\\s\\{.*", "\\1", P01_preflabel),
      Description = sub(" per unit (wet|dry) weight of biota", "", Description),
      MATRX_raw = sub(".*Subcomponent:\\s(.*?)\\].*", "\\1", P01_preflabel),
      MATRX = case_when(
        MATRX_raw %in% c("flesh", "whole organism", "soft tissue") ~ "SB",
        MATRX_raw == "liver"         ~ "LI",
        MATRX_raw %in% c("muscle tissue", "muscle") ~ "MU",
        TRUE ~ NA_character_),
      Species = sub("^(.*?)\\s\\(.*", "\\1", S25_preflabel)) %>%
    filter(MATRX %in% c("MU", "LI", "SB"))
  
  ## --- knyt sample-/subsample-id på ---
  df <- df %>%
    left_join(ids, by = c("LOCAL_CDI_ID", "ODV.internal.sample.number")) %>%
    mutate(tblSampleID   = dplyr::coalesce(as.character(Sample.identifier), SAMPID,
                                           paste(LOCAL_CDI_ID, ODV.internal.sample.number, sep = "_")),
           tblBioID      = dplyr::coalesce(as.character(Subsample.identifier), BIOID),
           tblAnalysisID = as.character(CDI.record.id))
  
  ## --- output-skema som før (EMODnet har ingen LOD/LOQ -> under_loq=FALSE, limit=NA) ---
  df_biota_EMOD <- df %>%
    transmute(
      Station = as.character(Station),
      DATE = as.Date(Date, "%Y-%m-%d"),
      Time, Datum,
      Latitude, Longitude,
      PARAM, Description, Species,
      MATRX, BASIS, Value,
      MUNIT = Units,
      under_loq = FALSE,
      limit     = NA_real_,
      tblSampleID, tblBioID, tblAnalysisID,
      CAS,
      Region = region, Category, Source) %>%
    mutate(PARAM = ifelse(grepl("lipids", Description), "lipidwt%", PARAM),
           Description = trimws(Description)) %>%
    distinct()
  
  df_biota_EMOD <- reference_state_bio(df_biota_EMOD)
  
  cat("EMODNET biota data contains:", nrow(df_biota_EMOD), "rows.\n")
  df_biota_EMOD   # lat/lon i decimalgrader (WGS84)
}

process_WISE_biota<- function (){
  user <- Sys.getenv("USERNAME")
  lon_lat<-read.csv(spatial_data_wise,sep=",")
  lon_lat <- lon_lat %>%
    group_by(countryCode,Station) %>%
    summarise(Latitude=mean(lat,na.rm=T), Longitude=mean(lon,na.rm=T), .groups = "drop")%>%
    select(-countryCode)
  # If, for a specific sample value, the measured concentration is below the limit of quantification (LOQ), then leave the "Observed Value" field empty, and report 'True' (or '1') in the Boolean "Observed value below LOQ" field.
  WISE_prep <- read.table(WISEpath, sep=";",header=T) %>%
    rename("Station" ="monitoringSiteIdentifier", #Unique international identifier of the monitoring site in which the data were obtained.
           "MATRX" = "procedureAnalysedMatrix", #Matrix where the measurement has been made.
           "MUNIT" ="resultUom", #Unit of measure for the reported values.
           "DATE" = "phenomenonTimeSamplingDate", #Date in which the sample was taken.
           "tblSampleID" = "sampleIdentifier", #Unique sample identifier. Its main purpose is to distinguish multiple samples taken on the same day from the same location.
           "Species"="parameterSpecies", #Species monitored.
           "CAS"= "observedPropertyDeterminandCode", #Unique code of the determinand monitored, as defined in the codelist.
           "Description"= "observedPropertyDeterminandLabel")%>%
    mutate(under_loq = resultQualityObservedValueBelowLOQ == 1,
           limit     = suppressWarnings(as.numeric(procedureLOQValue)),
           Value= ifelse(resultQualityObservedValueBelowLOQ==0,resultObservedValue,procedureLOQValue), #checking qflags
           Category="Biota")%>%
    select(-resultObservedValue,-procedureLOQValue,-resultQualityObservedValueBelowLOQ)
  
  #deleting columns with only na values.
  #WISE_prep<-WISE_prep[, colSums(is.na(WISE_prep)) != nrow(WISE_prep)]
  
  WISE_prep <-WISE_prep%>%
    left_join(lon_lat, by=c("Station")) %>%
    mutate(Source = "WISE")%>%
    mutate(across(everything(), ~gsub("CAS_", "", .)))%>%
    mutate(DATE=as.Date(DATE,format("%Y-%m-%d"), tryFormats = c("%Y%m%d", "%Y/%m/%d")))%>%
    mutate(Description=tolower(Description))%>%
    select_if(~!all(is.na(.)))
  
  ### Species codes from WISE
  df_species_code <- read.table(paste0(source_folder,"raw_data/codelist_dataelements_87972_WISE.csv"),
                                sep=",",header=T,comment.char="",fileEncoding="UTF-8",quote="") %>%
    rename("Species" = "Label",
           "parameterSpecies" = "Code") %>%
    mutate(parameterSpecies=as.character(parameterSpecies))
  
  WISE_prep<-WISE_prep%>%
    left_join(df_species_code, by=c("Species"="parameterSpecies"))%>%
    mutate(Species= ifelse(!is.na(Species.y),Species.y,Species),
           Species= ifelse(grepl("Gobius",Species),Remarks,Species))%>%
    select(-Species.y,-Remarks)
  
  WISE_prep<-WISE_prep%>%
    mutate(MYEAR=substr(DATE,1,4))
  
  countrycode<-read.table(country_code_wise, sep=",",header=T)%>%
    select(Code=1,Label)
  
  WISE_prep<-WISE_prep%>%
    left_join(countrycode,by=c("countryCode"="Code"))%>%
    select(-countryCode)%>%
    rename("Country"="Label")
  
  #MATRX
  #B = Whole organism
  #B-ST = soft tissue
  #B-MU = Muscle
  #B-LI = Liver
  
  WISE_prep<-WISE_prep%>%
    filter(MATRX %in% c("B","B-ST","B-MU","B-LI"))%>%
    mutate(MATRX= ifelse(MATRX=="B","SB",
                         ifelse(MATRX=="B-ST", "SB",
                                ifelse(MATRX=="B-MU","MU",
                                       ifelse(MATRX=="B-LI","LI",MATRX)))))
  
  
  #BASIS
  #"The biota data are expected to be provided in wet weight, so the percent moisture of the tissue should be reported if available."
  #https://dd.eionet.europa.eu/dataelements/98355
  WISE_prep<-WISE_prep%>%
    mutate(BASIS= "W")%>%
    mutate_at('resultMoisture',as.numeric) %>%
    mutate(`DRYWT` = 100-resultMoisture,
           FATWT = ifelse(is.na(resultFat),resultExtractableLipid,resultFat))%>%
    select(-resultExtractableLipid,-resultFat,-resultMoisture)
  
  
  #eea_wise<-WISE_prep%>%select(CAS,Description)%>%filter(grepl("EEA",CAS))%>%distinct()
  
  df_biota_WISE<-WISE_prep%>%
    mutate(tblAnalysisID=UID,
           Value=as.numeric(Value))%>%
    group_by(DATE,Station,Longitude,Latitude,Species,tblSampleID)%>%
    mutate(tblBioID=cur_group_id())%>%
    ungroup()
  
  #no need of applying additional functions for the drywt and lipidwt, its already in the dataset,
  
  # sikkerhedsnet: hvis ingen censurerede rækker, kan select_if have droppet limit
  if (!"limit" %in% names(df_biota_WISE))     df_biota_WISE$limit <- NA_real_
  if (!"under_loq" %in% names(df_biota_WISE)) df_biota_WISE$under_loq <- FALSE
  
  df_biota_WISE<-df_biota_WISE%>%
    select(Country,
           Station,
           DATE,
           Latitude,
           Longitude,
           Description,
           CAS,
           MATRX,
           BASIS,
           Value,
           MUNIT,
           under_loq,
           limit,
           DRYWT,
           FATWT,
           Species,
           tblBioID,
           tblSampleID,
           tblAnalysisID,
           Category,
           Source
    )%>%
    distinct()%>%
    mutate(tblAnalysisID=as.character(tblAnalysisID),
           tblSampleID=as.character(tblSampleID),
           tblBioID= as.character(tblBioID),
           Latitude=as.numeric(Latitude),
           Longitude=as.numeric(Longitude),
           Station=as.character(Station),
           under_loq=as.logical(under_loq),
           limit=as.numeric(limit),
           FATWT=as.numeric(FATWT),
           DRYWT=as.numeric(DRYWT),
           Value=as.numeric(Value))
  
  cat("WISE biota data contains:", nrow(df_biota_WISE),"rows.\n")
  #latitude and longitude measurements are in decimal degrees.
  return(df_biota_WISE)
}


#water
process_water <- function (path){
  user <- Sys.getenv("USERNAME")
  switch (path,
          ICESpath= process_ICES_water(),
          EMODNETpath= process_EMODNET_water(),
          WISEpath= process_WISE_water(),
          stop("Invalid source path provided. Only acceptable paths: ICESpath, EMODNETpath, WISEpath")
  )
}

process_ICES_water<- function(){
  
  df_water_ICES <- read.table(file = ICESpath, sep = ",", header = TRUE,
                              comment.char = "", quote = "\"",
                              fileEncoding = "UTF-8-BOM")
  
  #QFLAG
  #<	less than
  # >	greater than
  # D	reported value is less than the detection limit (DETLI)
  # Q	reported value is less than the limit of quantification (LMQNT)
  
  # VFLAG
  # A: means it has been checked and quality assured
  # c: calculated value
  df_water_ICES <- df_water_ICES %>%
    mutate(
      QFLAG = ifelse(QFLAG == "", NA, QFLAG),
      VFLAG = ifelse(VFLAG == "", NA, VFLAG),
      under_loq = !is.na(QFLAG) & QFLAG %in% c("D", "<", "<~D", "Q") &
        !(VFLAG %in% c("C", "A")),
      limit = case_when(
        !under_loq                                   ~ NA_real_,
        QFLAG %in% c("D", "<", "<~D") & !is.na(DETLI) ~ DETLI,
        TRUE                                         ~ LMQNT),
      Value = ifelse(under_loq, dplyr::coalesce(limit, Value), Value)
    )
  
  df_water_ICES<- df_water_ICES%>%
    select(Country,Station=STATN,DATE, Latitude, Longitude, DEPHU, DEPHL,
           PARGROUP, PARAM, MATRX, BASIS, Value, MUNIT, under_loq, limit,
           tblAnalysisID, tblParamID, tblSampleID) %>%
    mutate(Category = "Water",
           Source= "ICES",
           PARAM=tolower(PARAM))
  
  #Type of matrix in water
  #"AF"   Sea water sample - After filtration (deprecated)
  #"BF"   Sea water sample - Before filtration (deprecated)
  #"WT"   Water
  #"SI"  	Sample / Site information (miscellaneous)
  #"SPM"  Sea water sample - Suspended particulate matter
  
  
  df_water_ICES<-df_water_ICES%>%
    mutate(tblAnalysisID=as.character(tblAnalysisID),
           tblParamID=as.character(tblParamID),
           tblSampleID=as.character(tblSampleID),
           Latitude=as.numeric(Latitude),
           Longitude=as.numeric(Longitude),
           Station=as.character(Station))
  cat("ICES water data contains:", nrow(df_water_ICES),"rows.\n")
  
  return(df_water_ICES)
}
## ==================================================================
## process_EMODNET_water() tilpasset EMODnet Chemistry 2026-formatet
## ------------------------------------------------------------------
## Erstatter funktionen af samme navn i processing.R.
## Forskelle fra den oprindelige fil, som funktionen var skrevet til:
##   - Filen er tab-separeret (ikke ";")
##   - Der er ingen Type- og Region-kolonne (filen indeholder kun vand)
##   - Dybden ligger i "DepBelowSurf [m]" (ikke "Depth [m]")
##   - Der er ingen time_ISO8601-kolonne
##   - Tomme felter står som " " (mellemrum) og skal også være NA
## ==================================================================

process_EMODNET_water <- function(region = "Baltic Sea"){
  
  EMODNET <- read.table(EMODNETpath, sep = "\t", header = TRUE,
                        na.strings   = c("NA", "", " "),
                        quote        = "",      # apostroffer i stofnavne må ikke tolkes som citat
                        comment.char = "",
                        fileEncoding = "UTF-8")
  
  df_Emod <- EMODNET %>%
    distinct() %>%
    filter(grepl("water", S26_preflabel)) %>%
    mutate(Value = suppressWarnings(as.numeric(Value))) %>%
    drop_na(Value) %>%
    # SeaDataNet-kvalitetsflag for værdien: 3 = sandsynligvis dårlig, 4 = dårlig, 9 = mangler.
    # Fjern linjen, hvis du vil beholde alle værdier som i den oprindelige funktion.
    filter(!QV.SEADATANET.1 %in% c(3, 4, 9)) %>%
    mutate(Category      = "Water",
           Source        = "EMODNET",
           Region        = region,
           MYEAR         = as.numeric(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
           Date          = substr(yyyy.mm.ddThh.mm.ss.sss, 1, 10),
           Time          = substr(yyyy.mm.ddThh.mm.ss.sss, 12, 19),
           S27_preflabel = tolower(S27_preflabel),
           Station  = gsub(";", ",", Station),
           DEPHU         = as.numeric(DepBelowSurf..m.),   # prøvens dybde under overfladen
           DEPHL         = as.numeric(Water.depth..m.))    # som i den oprindelige funktion
  
  # Fjern rækker der kun beskriver dybde/tryk samt ikke-kemiske parametre
  df_Emod <- df_Emod %>%
    filter(!S06_preflabel %in% c("Depth (spatial coordinate)", "Pressure (spatial coordinate)"),
           !grepl("Sea-floor depth", P01_preflabel)) %>%
    mutate(S06_preflabel = ifelse(is.na(S06_preflabel) & grepl("Concentration", P01_preflabel),
                                  "Concentration", S06_preflabel)) %>%
    filter(!is.na(S06_preflabel),
           !S06_preflabel %in% c("Temperature", "Temperature (ITS-90)", "Turbidity",
                                 "Salinity", "Practical salinity", "pH (unspecified scale)",
                                 "Activity", "Enrichment"))
  
  # Matrix og basis
  df_Emod <- df_Emod %>%
    mutate(MATRX = ifelse(S26_preflabel == "suspended particulate material", "SPM", "WT"),
           BASIS = case_when(S02_preflabel == "per unit volume of the" ~ "W",
                             S02_preflabel == "per unit dry weight of" ~ "D",
                             TRUE ~ S02_preflabel)) %>%
    filter(MATRX != "SPM")
  
  # Harmonisering af stofnavne og CAS-numre (som i den oprindelige funktion)
  df_Emod <- df_Emod %>%
    mutate(S27_preflabel = case_when(
      is.na(S27_preflabel) & grepl("Concentration of suspended particulate material", P01_preflabel) ~ "SPM",
      is.na(S27_preflabel) & grepl("Triton", P01_preflabel)                 ~ "Triton x-100",
      is.na(S27_preflabel) & grepl("hexachlorocyclohexane", P01_preflabel)  ~ "alpha+beta+gamma-hexachlorocyclohexane",
      is.na(S27_preflabel) & grepl("nonylphenol", P01_preflabel)            ~ "4-nonylphenol (branched isomers)",
      is.na(S27_preflabel) & grepl("endosulfan", P01_preflabel)             ~ "alpha-endosulfan+beta-endosulfan+endosulphan sulphate",
      TRUE ~ S27_preflabel),
      # case_when i stedet for ifelse, så CAS ikke overskrives med NA hvor S27 er NA
      CAS.no = case_when(
        S27_preflabel == "Triton x-100"                     ~ "9002-93-1",
        S27_preflabel == "4-nonylphenol (branched isomers)" ~ "84852-15-3",
        TRUE ~ as.character(CAS.no)),
      CAS.no = gsub("\\.", "-", CAS.no))
  
  df_water_EMOD <- df_Emod %>%
    select(Station,
           DATE        = Date,
           Time,
           Datum,
           Latitude    = Latitude..degrees_north.,
           Longitude   = Longitude..degrees_east.,
           DEPHU,
           DEPHL,
           PARGROUP    = S27_altlabel,
           PARAM       = S27_preflabel,
           MATRX,
           BASIS,
           Value,
           MUNIT       = Units,
           tblSampleID = Sample.identifier,
           tblAnalysisID = CDI.record.id,
           CAS         = CAS.no,
           Description = P01_preflabel,
           Region,
           Category,
           Source) %>%
    mutate(tblAnalysisID = as.character(tblAnalysisID),
           tblSampleID   = as.character(tblSampleID),
           Latitude      = as.numeric(Latitude),
           Longitude     = as.numeric(Longitude),
           Longitude     = ifelse(Longitude > 180, Longitude - 360, Longitude),
           DATE          = as.Date(DATE, format = "%Y-%m-%d"),
           Station       = as.character(Station),
           under_loq     = FALSE,          # EMODnet har ingen LOD/LOQ-kolonne
           limit         = NA_real_)
  
  df_water_EMOD <- df_water_EMOD %>%
    mutate(Description = str_extract(Description, ".*(?= per unit volume)"),
           Description = gsub("Concentration of ", "", Description)) %>%
    mutate(Description = ifelse(is.na(CAS) & grepl("CAS", Description), Description,
                                str_extract(Description, ".*?(?= \\{)"))) %>%
    mutate(Description = case_when(
      grepl("dde", PARAM)                              ~ "DDT+DDD+DDE",
      grepl("Triton", PARAM)                           ~ "Triton X-100",
      grepl("tributyltin compounds", PARAM)            ~ "tributyltin compounds",
      grepl("benzo(b,k)fluoranthene", PARAM, fixed = TRUE) ~ "benzo(b,k)fluoranthene",
      TRUE ~ Description))
  
  cat("EMODNET water data contains:", nrow(df_water_EMOD), "rows.\n")
  return(df_water_EMOD)
}

process_WISE_water<- function (){
  
  lon_lat<-read.csv(spatial_data_wise,sep=",")
  lon_lat <- lon_lat %>%
    group_by(countryCode,Station) %>%
    summarise(Latitude=mean(lat,na.rm=T), Longitude=mean(lon,na.rm=T), .groups = "drop")%>%
    select(-countryCode)
  
  # If, for a specific sample value, the measured concentration is below the limit of quantification (LOQ), then leave the "Observed Value" field empty, and report 'True' (or '1') in the Boolean "Observed value below LOQ" field.
  WISE_prep <- read.table(WISEpath, sep=";",header=T, comment.char = "") %>%
    rename("Station" ="monitoringSiteIdentifier",
           "MATRX" = "procedureAnalysedMatrix",
           "MUNIT" ="resultUom",
           "DATE" = "phenomenonTimeSamplingDate",
           "tblSampleID" = "sampleIdentifier",
           "CAS"= "observedPropertyDeterminandCode",
           "Description"= "observedPropertyDeterminandLabel")%>%
    mutate(under_loq = resultQualityObservedValueBelowLOQ == 1,
           limit     = suppressWarnings(as.numeric(procedureLOQValue)),
           Value= ifelse(resultQualityObservedValueBelowLOQ==0,resultObservedValue,procedureLOQValue),
           Category="Water")%>%
    select(-resultObservedValue,-procedureLOQValue,-resultQualityObservedValueBelowLOQ)%>%
    filter(MATRX %in% c("W","W-DIS","W-SPM"))
  
  #WISE_prep<-WISE_prep[, colSums(is.na(WISE_prep)) != nrow(WISE_prep)]
  
  
  WISE_prep <-WISE_prep%>%
    left_join(lon_lat, by=c("Station")) %>%
    mutate(Source = "WISE")%>%
    mutate(across(everything(), ~gsub("CAS_", "", .)))%>%
    mutate(DATE=as.Date(DATE,format("%Y-%m-%d"), tryFormats = c("%Y%m%d", "%Y/%m/%d")))%>%
    mutate(Description=tolower(Description))%>%
    select_if(~!all(is.na(.)))
  
  #depth
  #Definitions:
  #ParameterSedimentDepthSampled: Depth of the collected sample of sediment, measured as a range, in centimeters, starting at the seafloor surface. The range would start by zero if the top of the sediment sample is the seafloor surface.
  #ParameterSampleDepth: Depth at which sample was taken in meter below water surface, in meters.
  
  WISE_prep <-WISE_prep%>%
    mutate(parameterSampleDepth=as.numeric(parameterSampleDepth))%>%
    rename("DEPHU"="parameterSampleDepth")
  
  #eea_wise<-WISE_prep%>%select(CAS,Description)%>%filter(grepl("EEA",CAS))%>%distinct()
  
  df_water_WISE<-WISE_prep%>%
    mutate(tblAnalysisID=UID,
           Value=as.numeric(Value)
    )%>%
    filter(!is.na(Value))
  
  WISE_prep<-df_water_WISE%>%
    mutate(MYEAR=substr(DATE,1,4))
  
  countrycode<-read.table(country_code_wise, sep=",",header=T)%>%
    select(Code=1,Label)
  
  WISE_prep<-WISE_prep%>%
    left_join(countrycode,by=c("countryCode"="Code"))%>%
    select(-countryCode)
  
  # sikkerhedsnet: hvis ingen censurerede rækker, kan select_if have droppet limit
  if (!"limit" %in% names(WISE_prep))     WISE_prep$limit <- NA_real_
  if (!"under_loq" %in% names(WISE_prep)) WISE_prep$under_loq <- FALSE
  
  df_water_WISE<-WISE_prep%>%
    select(Station,
           DATE,
           Latitude,
           Longitude,
           DEPHU,
           Description,
           #Remarks,
           MATRX,
           Value,
           MUNIT,
           under_loq,
           limit,
           tblSampleID,
           tblAnalysisID,
           CAS,
           Country=Label,
           Category,
           Source
    )%>%
    distinct()%>%
    mutate(tblAnalysisID=as.character(tblAnalysisID),
           tblSampleID=as.character(tblSampleID),
           Latitude=as.numeric(Latitude),
           Longitude=as.numeric(Longitude),
           Station=as.character(Station),
           under_loq=as.logical(under_loq),
           limit=as.numeric(limit),
           Value=as.numeric(Value))
  
  cat("WISE water data contains:", nrow(df_water_WISE),"rows.\n")
  #latitude and longitude measurements are in decimal degrees.
  return(df_water_WISE)
}

