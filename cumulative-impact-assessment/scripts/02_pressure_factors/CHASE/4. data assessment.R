library(readxl)
library(sf)
library(dplyr)
library(lubridate)
library(tidyverse)
library(patchwork)
library(ggspatial)

source_folder<-"C:/Data/Repositories/CHASE/ETC_CHASE_2024/"

source("CHASE/R scripts/function_EQR_CR.R")
source("CHASE/R scripts/utilities.R")

# Read in assessment data 
  df<-readRDS("CHASE/assessment_2013_2024_base_only.rds")%>%
    mutate(Value = ifelse(Value == 0, 0.000001, Value)) %>% 
    mutate(logvalue= ifelse(SubstGroup=="Imposex",Value,log10(Value)))%>% #the index measured data does not need log transform
    group_by(GridID,Category,SubstGroup,Substance,Type,Tissue,Unit,Basis,Threshold) %>% 
    summarize(StatusLog = mean(logvalue), StatusMean = mean(Value),n=n(), .groups="drop") %>%
    mutate(Value= ifelse(SubstGroup=="Imposex",StatusLog,(10^StatusLog)))%>%select(-StatusMean,-n,-StatusLog) 

  
#Add here any extra data that has been gathered

  load("CHASE/data_greece.RData")
  df<-bind_rows(df,sediment_assessment,water_assessment)%>%
    mutate(Category= str_to_title(Category))

  #write.table(df,"CHASE/assessment_2013_2024.csv",sep=";",row.names = F)
  rm(sediment_assessment,water_assessment)
# EEA assessment grid 
# Filter out areas not belonging to Europe

  grd<-read_sf( "assessment grid/assessment_grid_100_20_minus_land.shp")%>%
    select(GRIDCODE,Region,Subregion=Subregn,Include)%>%
    st_zm()

  
# Utilities

  correct_order <- c("High", "Good", "Moderate", "Poor", "Bad","No Data")
  
  reg<- c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean")
  subreg<- c("Baltic Sea",
             "Black Sea",
             "Adriatic Sea",
             "Aegean-Levantine Sea",
             "Ionian Sea and the Central Mediterranean Sea",
             "Western Mediterranean Sea",
             "Bay of Biscay and the Iberian Coast",
             "Celtic Seas",
             "Greater North Sea, incl. the Kattegat and the English Channel",
             "Icelandic Sea",
             "Macaronesia",
             "Norwegian Sea",
             "Barents Sea")
  StatusLevels <- c("High","Good","Mod","Poor","Bad")
  

  
# Factorize data
  
  df_factors <- data.frame(Status=factor(correct_order, levels=correct_order))
  df_regions <- data.frame(Region = factor(c(reg,"Europe seas"),
                                           levels = c(reg,"Europe seas")))
  df_subregions <- data.frame(Subregion = factor(c(subreg),
                                           levels = c(subreg)))
  df_factors <- df_factors %>% merge(df_regions, all=T)  

# Calculate EQR value
  
  df_CR <- df %>%
    mutate(CR=Value/Threshold) %>%
    mutate(logCR=log10(CR)) %>%
    group_by(GridID,SubstGroup, Substance,Category,Threshold) %>%
    summarise(logCR=mean(logCR,na.rm=T), .groups="drop") %>%
    mutate(CR = 10^logCR) %>%
    group_by(GridID,Category) %>%
    mutate(EQR_cat = EQR_CR(CR)) 
  
  
# Worst substance = highest EQR value per gridcell
  
  df_cat <- df_CR %>%
    mutate(GridID=tolower(GridID))%>%
    group_by(GridID, Category) %>%
    mutate(n_subs=n()) %>%
    arrange(desc(CR)) %>%
    slice(1) %>%
    ungroup() %>%
    rename(WorstGroup=SubstGroup, WorstSubst=Substance) 
  

# Integrated EQR = minimum EQR per gridcell
  
  df_CHASE <- df_cat %>% #integrated one
    group_by(GridID) %>%
    arrange(EQR_cat) %>%
    slice(1) %>%
    ungroup()%>%
    rename(WorstCat=Category)
 
  
  
#add status
  df_cat<-df_cat%>%
    mutate(Status= case_when(
      EQR_cat > 0.8 ~ "High",
      EQR_cat > 0.6 & EQR_cat <= 0.8 ~ "Good",
      EQR_cat > 0.4 & EQR_cat <= 0.6 ~ "Moderate",
      EQR_cat > 0.2 & EQR_cat <= 0.4 ~ "Poor",
      EQR_cat <= 0.2 ~ "Bad",
      is.na(EQR_cat) ~"No Data",
    ))
  
  df_CHASE<-df_CHASE%>%
    mutate(Status= case_when(
      EQR_cat > 0.8 ~ "High",
      EQR_cat > 0.6 & EQR_cat <= 0.8 ~ "Good",
      EQR_cat > 0.4 & EQR_cat <= 0.6 ~ "Moderate",
      EQR_cat > 0.2 & EQR_cat <= 0.4 ~ "Poor",
      EQR_cat <= 0.2 ~ "Bad",
      is.na(EQR_cat) ~"No Data",
    ))

# Add regions to the data
  grd<-grd%>%mutate(GRIDCODE=tolower(GRIDCODE))
  df_CHASE_category <- left_join(grd,df_cat, by=c("GRIDCODE"="GridID"))%>%mutate(Status= ifelse(is.na(Status),"No Data",Status))
  metadata<-df_CHASE_category%>%select(GRIDCODE,Category,Region,Subregion,Status,EQR_cat,Include)
  metadata$geometry<-NULL
  metadata<-metadata%>%distinct()
  write.table(metadata,"CHASE/Figures/metadata_maps.csv",sep=";",row.names = F)
  rm(metadata)
  
  df_CHASE_integrated<-left_join(grd,df_CHASE, by=c("GRIDCODE"="GridID"))%>%mutate(Status= ifelse(is.na(Status),"No Data",Status))
  rm(df_CHASE,df_cat,df)
save.image("CHASE/data_assessment_for_visualization.RData")
