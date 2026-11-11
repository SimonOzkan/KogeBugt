library(readxl)
library(sf)
library(dplyr)
library(lubridate)
library(tidyverse)
library(patchwork)
library(ggspatial)

#load("C:/Data/Repositories/CHASE/ETC_CHASE_2024/R data/data_thresholds.RData")


source_folder<-"C:/Data/Repositories/CHASE/ETC_CHASE_2024/"

source("CHASE/R scripts/processing.R")
source("CHASE/R scripts/utilities.R")

#assessment year
  user <- Sys.getenv("USERNAME")
  
  #if you want all the data, choose a very low number (like 1900)
  
  assessment_year<-2013 #we are doing a 10 year period.

#WATER----
  
  water<-water%>%
    mutate(year=substr(date,1,4),
           year=as.numeric(year))%>%
    filter(year>=assessment_year)
  
  water_assessment<-water%>%
    select(GridID=gridcode,
           Category=category,
           SubstGroup=substgroup,
           Substance=substance,
           Unit=munit,
           Threshold=threshold_value,
           Value=value)
  
  rm(water)
  
#SEDIMENT----
  
  sedim<-sedim%>%
    mutate(year=substr(date,1,4),
           year=as.numeric(year))%>%
    filter(year>=assessment_year)

# Use the same basis to compare to threshold
  sedim<-sedim%>%
    mutate(basis=ifelse(is.na(basis),"D",basis))%>%
    mutate(value= case_when(
      .$basis=="W" & .$threshold_basis=="D" ~ value/(drywt/100),
      .$basis=="D" & .$threshold_basis=="D" ~ value,
      TRUE ~NA_integer_))
  
  sedim<-sedim%>%filter(!is.na(value)) #take away the ones that does not match their basis
  sedim<-sedim%>%          #the value has been normalized when possible
    mutate(valuecorr= case_when(
      .$ref_param=="AL" ~ value*(al_pct/ref_value),
      .$ref_param=="CORG" ~ value*(corg_pct/ref_value),
      TRUE ~ NA_integer_
    ))%>%
    mutate(valuecorr= ifelse(is.na(valuecorr),value, valuecorr))%>%
    relocate(valuecorr,.after=value)
  
  
  sediment_assessment<-sedim%>%
    select(GridID=gridcode,
           Category=category,
           SubstGroup=substgroup,
           Substance=substance,
           Unit=munit,
           Threshold=threshold_value,
           Value=valuecorr)
  
  rm(sedim)
  
#BIOTA----
  
  biota<-biota%>%
    mutate(year=substr(date,1,4),
           year=as.numeric(year))%>%
    filter(year>=assessment_year)
  
# Use the same basis and matrix to compare to threshold
  biota_assessment<-biota%>%
    filter(!is.na(threshold_value))%>%
    mutate(valuecorr= case_when(
      .$basis=="D" & .$threshold_basis=="W" ~value*(drywt/100),
      .$basis=="W" & .$threshold_basis=="D" ~value/((drywt)/100),
      .$basis=="L" & .$threshold_basis=="W" ~value*(fatwt/100),
      .$basis=="W" & .$threshold_basis=="L" ~value/((fatwt)/100),
      .$basis=="L" & .$threshold_basis=="D" ~value*(fatwt/drywt),
      .$basis=="D" & .$threshold_basis=="L" ~value/(fatwt/drywt),
      TRUE ~ NA_real_
    ))%>%
    mutate(valuecorr= case_when(
      .$basis==.$threshold_basis & .$matrx==.$threshold_matrix ~ value,
      TRUE ~ valuecorr))%>%
    relocate(valuecorr,.after=value)
  
  biota_assessment<-biota_assessment%>%
    filter(!is.na(threshold_value))%>%
    mutate(basiscorr= case_when(
      basis=="D" & threshold_basis=="W" ~"W",
      basis=="W" & threshold_basis=="D" ~"D", 
      basis=="L" & threshold_basis=="W" ~"W",
      basis=="W" & threshold_basis=="L" ~"L", 
      basis=="L" & threshold_basis=="D" ~"D",
      basis=="D" & threshold_basis=="L" ~"L",
      TRUE ~ NA_character_
    ))%>%
    mutate(basiscorr= case_when(
      basis==threshold_basis ~ basis,
      TRUE ~ basiscorr))%>%
    relocate(basiscorr,.after=basis)
  
  #mercury can be applied for muscle, liver and shellfish directly
  biota_assessment<-biota_assessment%>%
    mutate(valuecorr= ifelse(substance=="mercury" & basis==threshold_basis,value,valuecorr))
  
  
  biota_assessment<-biota_assessment%>%
    filter(!is.na(valuecorr))%>%
    select(GridID=gridcode,
           Category=category,
           SubstGroup=substgroup,
           Substance=substance,
           Type=type,
           Tissue=matrx,
           Unit=munit,
           Basis=basis,
           Threshold=threshold_value,
           Value=value) 
  
  rm(biota)
  
#BIOEFFECTS----
  
  bioeffects<-bioeffects%>%
    mutate(year=substr(date,1,4),
           year=as.numeric(year))%>%
    filter(year>=assessment_year)
  
  bioeffects_assessment<-bioeffects%>%
    select(GridID=gridcode,
           Category=category,
           SubstGroup=substgroup,
           Substance=substance,
           Type=species,
           Unit=munit,
           Threshold=threshold_value,
           Value=value)
  
  rm(bioeffects)

#ALL DATA ----

  data<-bind_rows(water_assessment,sediment_assessment,bioeffects_assessment,biota_assessment)%>%arrange(GridID)%>%
      select(GridID,Category,SubstGroup,Substance,Type,Tissue,Basis,Unit,Value,Threshold)
  
  
#save.image("C:/Data/Repositories/CHASE/ETC_CHASE_2024/R data/data_pre-assessment.RData")
saveRDS(data,file="CHASE/assessment_2013_2024_base_only.rds")  
