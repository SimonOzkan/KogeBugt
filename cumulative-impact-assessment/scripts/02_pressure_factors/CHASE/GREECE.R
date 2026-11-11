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

greece<-read.table(paste0(source_folder,"raw_data/D8-C1_HCMR - greece.csv"), sep=";",header=T,na.strings = NA)%>%mutate(CAS= ifelse(CAS=="",NA,CAS))
greece_param<-greece%>%select(parameter,CAS)%>%distinct()
param<-read.table(paste0(source_folder,"raw_data/nomenclature_parameter.csv"), sep=";",header=T,quote="")%>%
  mutate(CAS=trimws(CAS))%>%select(-ICES,-EMODNET,-WISE)%>%distinct()

greece_param<-left_join(greece_param,param,by="CAS",na_matches = "never")%>%
  mutate(SubstGroup= ifelse(parameter=="Total DDT","Other organohalogens",SubstGroup),
         Grouping= ifelse(parameter=="Total DDT","DDTSUM",Grouping),
         Substance_code= ifelse(parameter=="Total DDT","DDTSUM",Substance_code),
         Substance= ifelse(parameter=="Total DDT","ddt total",Substance))%>%
  filter(!is.na(Substance))



  
greece<-left_join(greece_param,greece)%>%
  rename("longitude"="station_lng",
         "latitude"="station_lat")%>%
  mutate(Date= as.Date(sampling_date,format="%m/%d%/Y"),
         Source="National Greece")%>%
  rename("munit"="Unit",
         "station"="station_name")


greece_water<-greece%>%filter(Matrix=="water")%>%
  mutate(Category="water")

names(greece_water) <- tolower(names(greece_water))
ni<-nrow(greece_water)
greece_water<-add_grid(greece_water)
nj<-nrow(greece_water)
names(greece_water) <- tolower(names(greece_water))


water<-greece_water%>%
  mutate(munit=tolower(munit))
matching<-water%>%
  select(munit)%>%
  distinct()

#no need of changing units

water$value <- format(water$value, nsmall = 4)
water<-water%>%
  arrange(gridcode,date,substance,cas,value)%>%
  mutate(value=as.numeric(value))%>%
  mutate(matrx=ifelse(matrix %in% c("water"),"W",NA),
         basis= "W")%>%
  filter(!is.na(value))%>%select(-matrix)

ni<-nrow(water)
#first we are going to check that we have all the parameters present in the data
water_thresholds <- threshold_values("Water")%>%
  mutate(`Substance name`=tolower(`Substance name`))

df1<-sort(unique(water_thresholds$`Substance name`))
df2<-sort(unique(water$substance))
check<-df1[!df1 %in% df2]
rm(greece_water)

if (length(check)>0) {
  cat("These elements are not in the dataset:\n",check,"\nIf they are from a group (for example,nonylphenols (4-nonylphenol)), it is okey, if not, re-check the code.\n",sep="\n")
} else {
  print("All elements present in the dataset.\n")
}
rm(df1,df2,check)

#now we are going to divide the data between the ones that have a threshold value for the group, and the ones that have a threshold value for the parameter individually. This will be done by stations
water_parameter<-water%>%
  filter(is.na(multiplier))%>%
  group_by(gridcode,region,station, longitude, latitude, date,substgroup, substance, basis, matrx, munit, category, source) %>%
  summarise(value=mean(value),.groups = "drop")

water_group<- water%>%
  filter(!is.na(multiplier))%>%
  split(.$grouping)%>%
  map(~ .x %>%
        group_by(gridcode,region,station, longitude, latitude, date, substgroup, substance, basis, matrx, munit, category, source,grouping,multiplier) %>%
        summarise(value=mean(value),.groups = "drop"))%>%
  map(~.x %>%
        group_by(gridcode,region,station,longitude, latitude, date, substgroup, grouping, basis, matrx, munit, category, source)%>%
        summarise(value= sum(value*multiplier),.groups = "drop"))

#first we calculated the mean for parameters that have measured more than once on the same day, and then we use the multiplier to sum the different parameters into the groups.
water_group<- water_group%>%
  map(~.x %>%
        mutate(substance= case_when(
          .$grouping=="DDTSUM" ~ "ddt total",
          .$grouping=="DLDALD" ~ "cyclodiene pesticides: aldrin, dieldrin, endrin, isodrin",
          .$grouping=="HBCDSUM" ~ "hexabromocyclododecane (hbcdd)",
          .$grouping=="HEPCHCEPX" ~ "heptachlor and heptachlor epoxide",
          .$grouping=="NOPHESUM" ~ "nonylphenols (4-nonylphenol)",
          .$grouping=="PBDE6" ~ "bde total",
          .$grouping=="WHOTEQ" ~ "dioxins",
          .$grouping=="WHOTEQ-PCB" ~ "dioxins-pcb",
          .$grouping=="TRCB3" ~ "trichloro-benzenes",
          TRUE ~ "NO_group"
        ))%>%
        relocate(substance,.after = grouping))%>%
  bind_rows()%>%select(-grouping)

water<-bind_rows(water_parameter,water_group)

nj<-nrow(water) #----529136
cat("Before merging the parameter groups, the data had",ni,"rows. After checking, we have",nj,"rows.")
rm(water_parameter, water_group)


water_thresholds<-water_thresholds%>%
  select(-Substance.X,-Harmon_name,-PARAM,-Description,-CAS,-PARAM.Group,-P_source,-Multiplier,-Group)%>%distinct()

#before adding the thresholds, we need to address a problem in the dataset. We have also included the parameter "tbtin" additionally to the "tbtin (sn+)". Since the threshold is made for the "sn" part, then we will calculate the cation part. 
#I noticed that this will not be needed, since we have both parameters for the same record, so we will filter out the tbtin, since our threshold is for the tbsn+
water<-water%>%
  filter(!(substance %in% c("tbtin")))

water<-left_join(water,water_thresholds,by=c("substance"="Substance name","category"="Category"))
#no_th<-water%>%filter(is.na(Threshold_value))%>%select(substance)%>%distinct()
water<-water%>%filter(!is.na(Threshold_value))#-----409780
names(water)<-tolower(names(water))

water_assessment<-water%>%
  select(GridID=gridcode,
         Category=category,
         SubstGroup=substgroup,
         Substance=substance,
         Unit=munit,
         Threshold=threshold_value,
         Value=value)
#write.table(water,file=paste0("input data/water2024_withthresholds_greece.csv"),sep=";",row.names = F)
rm(water, water_thresholds)


#sediment----
greece<-read.table(paste0(source_folder,"raw_data/D8-C1_HCMR - greece.csv"), sep=";",header=T,na.strings = NA)%>%mutate(CAS= ifelse(CAS=="",NA,CAS))
greece_param<-greece%>%select(parameter,CAS)%>%distinct()
param<-read.table(paste0(source_folder,"raw_data/nomenclature_parameter.csv"), sep=";",header=T,quote="")%>%
  mutate(CAS=trimws(CAS))%>%select(-ICES,-EMODNET,-WISE)%>%distinct()

greece_param<-left_join(greece_param,param,by="CAS",na_matches = "never")%>%
  mutate(SubstGroup= ifelse(parameter=="Total DDT","Other organohalogens",SubstGroup),
         Grouping= ifelse(parameter=="Total DDT","DDTSUM",Grouping),
         Substance_code= ifelse(parameter=="Total DDT","DDTSUM",Substance_code),
         Substance= ifelse(parameter=="Total DDT","ddt total",Substance))%>%
  filter(!is.na(Substance))




greece<-left_join(greece_param,greece)%>%
  rename("longitude"="station_lng",
         "latitude"="station_lat")%>%
  mutate(Date= as.Date(sampling_date,format="%m/%d%/Y"),
         Source="National Greece",
         Basis="D")%>%
  rename("munit"="Unit",
         "station"="station_name")
rm(greece_param)

greece_sed<-greece%>%filter(Matrix=="sediment")%>%
  mutate(Category="sediment")

names(greece_sed) <- tolower(names(greece_sed))
ni<-nrow(greece_sed)
greece_sed<-add_grid(greece_sed)
nj<-nrow(greece_sed)
names(greece_sed) <- tolower(names(greece_sed))


sedim<-greece_sed%>%
  mutate(munit=tolower(munit))
matching<-sedim%>%
  select(munit)%>%
  distinct()

sedim<-sedim%>%
  mutate(value= ifelse(munit=="g/kg", value*1000000,value),
         value= ifelse(munit=="gm/g", value*1000000000,value),
         value= ifelse(munit=="mg/kg", value*1000,value),
         value= ifelse(munit=="ng/g", value*1,value),
         value= ifelse(munit=="ng/kg", value/1000,value),
         value= ifelse(munit=="ug/g", value*1000,value),
         value= ifelse(munit=="%", value*100000000,value),
         munit= ifelse(munit %in% c("g/kg","gm/g","mg/kg","ng/g","ng/kg","ug/g","%","ug sn/kg"), "ug/kg",munit))


sedim$value <- format(sedim$value, nsmall = 4)
sedim<-sedim%>%
  arrange(gridcode,date,substance,cas,value)%>%
  mutate(value=as.numeric(value))%>%
  mutate(matrx=ifelse(matrix %in% c("sediment"),"S",NA))%>%
  filter(!is.na(value))%>%
    select(-matrix)

rm(greece_sed)
#first we are going to check that we have all the parameters present in the data

sedim_thresholds <- threshold_values("Sediment")%>%
  mutate(`Substance name`=tolower(`Substance name`))

df1<-sort(unique(sedim_thresholds$`Substance name`))
df2<-sort(unique(sedim$substance))
check<-df1[!df1 %in% df2]


if (length(check)>0) {
  cat("These elements are not in the dataset:\n",check,"\nIf they are from a group (for example,nonylphenols (4-nonylphenol)), it is okey, if not, re-check the code.\n",sep="\n")
} else {
  print("All elements present in the dataset.\n")
}
rm(df1,df2,check)

#now we are going to divide the data between the ones that have a threshold value for the group, and the ones that have a threshold value for the parameter individually. This will be done by stations
sedim_parameter<-sedim%>%
  filter(is.na(multiplier))%>%
  group_by(gridcode,region,station,longitude, latitude, date, substgroup, substance, basis, matrx, munit, category, source) %>%
  summarise(value=mean(value,na.rm=T),.groups = "drop")

sedim_group<- sedim%>%
  filter(!is.na(multiplier))%>%
  split(.$grouping)%>%
  map(~ .x %>%
        group_by(gridcode,region,station,longitude, latitude, date, substgroup, substance, basis, matrx, munit, category, source,grouping,multiplier) %>%
        summarise(value=mean(value),.groups = "drop"))%>%
  map(~.x %>%
        group_by(gridcode,region,station, longitude, latitude,date,substgroup,grouping, basis, matrx, munit, category, source)%>%
        summarise(value= sum(value*multiplier),.groups = "drop"))

#first we calculated the mean for parameters that have measured more than once on the same day, and then we use the multiplier to sum the different parameters into the groups.
sedim_group<- sedim_group%>%
  map(~.x %>%
        mutate(substance= case_when(
          .$grouping=="DDTSUM" ~ "ddt total",
          .$grouping=="DLDALD" ~ "cyclodiene pesticides: aldrin, dieldrin, endrin, isodrin",
          .$grouping=="HBCDSUM" ~ "hexabromocyclododecane (hbcdd)",
          .$grouping=="HEPCHCEPX" ~ "heptachlor and heptachlor epoxide",
          .$grouping=="NOPHESUM" ~ "nonylphenols (4-nonylphenol)",
          .$grouping=="PBDE6" ~ "bde total",
          .$grouping=="WHOTEQ" ~ "dioxins",
          .$grouping=="WHOTEQ-PCB" ~ "dioxins-pcb",
          .$grouping=="TRCB3" ~ "trichloro-benzenes",
          TRUE ~ "NO_group"
        ))%>%
        relocate(substance,.after = grouping))%>%
  bind_rows()

sedim<-bind_rows(sedim_parameter,sedim_group)
sedim<-sedim%>%
  mutate(across(everything(), ~ replace(., is.nan(.), NA)))
nj<-nrow(sedim) #----529136
cat("Before merging the parameter groups, the data had",ni,"rows. After checking, we have",nj,"rows.")
rm(sedim_parameter,sedim_group,ni,nj)

sedim_thresholds<-sedim_thresholds%>%
  select(-Substance.X,-Harmon_name,-PARAM,-Description,-CAS,-PARAM.Group,-P_source,-Multiplier,-Group)%>%distinct()

#before adding the thresholds, we need to address a problem in the dataset. We have also included the parameter "tbtin" additionally to the "tbtin (sn+)". Since the threshold is made for the "sn" part, then we will calculate the cation part. 
#I noticed that this will not be needed, since we have both parameters for the same record, so we will filter out the tbtin, since our threshold is for the tbsn+
sedim<-sedim%>%
  filter(!(substance %in% c("tbtin")))


sedim<-left_join(sedim,sedim_thresholds,by=c("substance"="Substance name","category"="Category"))
#no_th<-water%>%filter(is.na(Threshold_value))%>%select(substance)%>%distinct()
sedim<-sedim%>%filter(!is.na(Threshold_value))#-----409780
names(sedim)<-tolower(names(sedim))
rm(sedim_thresholds)


#use the same basis to compare to threshold

sedim<-sedim%>%filter(!is.na(value)) #take away the ones that does not match their basis
sedim<-sedim%>%          #the value has been normalized when possible
  mutate(valuecorr= value)%>%
  relocate(valuecorr,.after=value)
names(sedim)<-tolower(names(sedim))
sediment_assessment<-sedim%>%
  select(GridID=gridcode,
         Category=category,
         SubstGroup=substgroup,
         Substance=substance,
         Unit=munit,
         Threshold=threshold_value,
         Value=value)


#write.table(water,file=paste0("input data/sediment2024_withthresholds_greece.csv"),sep=";",row.names = F)
rm(sedim)
rm(matching,param,greece)

save.image("CHASE/data_greece.RData")
