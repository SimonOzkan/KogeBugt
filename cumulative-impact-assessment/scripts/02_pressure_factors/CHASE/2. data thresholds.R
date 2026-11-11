library(readxl)
library(sf)
library(dplyr)
library(lubridate)
library(tidyverse)
library(patchwork)
library(ggspatial)

#load("C:/Data/Repositories/CHASE/ETC_CHASE_2024/R data/data_processing.RData")

source_folder<-"C:/Data/Repositories/CHASE/ETC_CHASE_2024/"

source("CHASE/R scripts/processing.R")
source("CHASE/R scripts/utilities.R")

# WATER ----

# Check parameters present in the data

ni<-nrow(water_cleaned)

water_thresholds <- threshold_values("Water")%>%
  mutate(`Substance name`=tolower(`Substance name`))
names_water<-sort(unique(water_thresholds$`Substance name`))
water_thresholds<-water_thresholds%>%
  select(-Substance.X,-Harmon_name,-PARAM,-Description,-CAS,-PARAM.Group,-P_source,-Multiplier,-Group)%>%distinct()

df1<-sort(unique(water_thresholds$`Substance name`))
df2<-sort(unique(water$substance))
check<-df1[!df1 %in% df2]


if (length(check)>0) {
  cat("These elements are not in the dataset:\n",check,"\nIf they are from a group (for example,nonylphenols (4-nonylphenol)), it is okey, if not, re-check the code.\n",sep="\n")
} else {
  print("All elements present in the dataset.\n")
}
rm(df1,df2,check)

# Add threshold for parameter 

water_parameter<-water%>%
  filter(is.na(multiplier))%>%
  group_by(gridcode,region,station, longitude, latitude, date,substgroup, substance, basis, matrx, munit, category, source) %>%
  summarise(value=mean(value),.groups = "drop")

# Add threshold for group   

water_group<- water%>%
  filter(!is.na(multiplier))%>%
  split(.$grouping)%>%
  map(~ .x %>%
        group_by(gridcode,region,station, longitude, latitude, date, substgroup, substance, basis, matrx, munit, category, source,grouping,multiplier) %>%
        summarise(value=mean(value),.groups = "drop"))%>%
  map(~.x %>%
        group_by(gridcode,region,station,longitude, latitude, date, substgroup, grouping, basis, matrx, munit, category, source)%>%
        summarise(value= sum(value*multiplier),.groups = "drop"))

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

# Join data together

water<-bind_rows(water_parameter,water_group)

nj<-nrow(water) #----529136
cat("Before merging the parameter groups, the data had",ni,"rows. After checking, we have",nj,"rows.")
#Before merging the parameter groups, the data had 673853 rows. After checking, we have 533758 rows.

rm(ni,nj,water_group,water_parameter)

# Filter out the tbtin, since our threshold is for the tbsn+

water<-water%>%
  filter(!(substance %in% c("tbtin")))

#Join the data with the thresholds

water<-left_join(water,water_thresholds,by=c("substance"="Substance name"))

no_th<-water%>%filter(is.na(Threshold_value))%>%select(substance)%>%distinct()
print(sort(no_th$substance))
check<-names_water[names_water %in% no_th]
if (length(check)>0) {
  cat("Parameters that should have threshold:\n",check,"\n",sep="\n")
} else {
  print("All parameters have threshold.\n")
}

water<-water%>%filter(!is.na(Threshold_value))%>%select(-Category)
names(water)<-tolower(names(water))

# Save the data

#write.table(water,file=paste0("input data/water2025_withthresholds.csv"),sep=";",row.names = F)

rm(water_thresholds,no_th,names_water,check)

# SEDIMENTS ----

# Check parameters present in the data

ni<-nrow(sedim)
sedim_thresholds <- threshold_values("Sediment")%>%
  mutate(`Substance name`=tolower(`Substance name`))
names_sedim<-sort(unique(sedim_thresholds$`Substance name`))

df1<-sort(unique(sedim_thresholds$`Substance name`))
df2<-sort(unique(sedim$substance))
check<-df1[!df1 %in% df2]


if (length(check)>0) {
  cat("These elements are not in the dataset:\n",check,"\nIf they are from a group (for example,nonylphenols (4-nonylphenol)), it is okey, if not, re-check the code.\n",sep="\n")
} else {
  print("All elements present in the dataset.\n")
}
rm(df1,df2,check)


# Add threshold for parameter 

sedim_parameter<-sedim%>%
  filter(is.na(multiplier))%>%
  group_by(gridcode,region,station,longitude, latitude, date, substgroup, substance, basis, matrx, munit, category, source) %>%
  summarise(value=mean(value,na.rm=T),corg_pct=mean(corg_pct,na.rm = T),al_pct=mean(al_pct,na.rm = T),drywt=mean(drywt,na.rm=T),.groups = "drop")

# Add threshold for group   

sedim_group<- sedim%>%
  filter(!is.na(multiplier))%>%
  split(.$grouping)%>%
  map(~ .x %>%
        group_by(gridcode,region,station,longitude, latitude, date, substgroup, substance, basis, matrx, munit, category, source,grouping,multiplier) %>%
        summarise(value=mean(value),corg_pct=mean(corg_pct,na.rm = T),al_pct=mean(al_pct,na.rm = T),drywt=mean(drywt,na.rm=T),.groups = "drop"))%>%
  map(~.x %>%
        group_by(gridcode,region,station, longitude, latitude,date,substgroup,grouping,basis, matrx, munit, category, source)%>%
        summarise(value= sum(value*multiplier),corg_pct=mean(corg_pct,na.rm = T),al_pct=mean(al_pct,na.rm = T),drywt=mean(drywt,na.rm=T),.groups = "drop"))

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
  bind_rows()%>%select(-grouping)

# Join data together

sedim<-bind_rows(sedim_parameter,sedim_group)
sedim<-sedim%>%
  mutate(across(everything(), ~ replace(., is.nan(.), NA)))
nj<-nrow(sedim)

cat("Before merging the parameter groups, the data had",ni,"rows. After checking, we have",nj,"rows.")
#Before merging the parameter groups, the data had 663743 rows. After checking, we have 428110 rows.

rm(ni,nj,sedim_group,sedim_parameter)

# Filter out the tbtin, since our threshold is for the tbsn+

sedim_thresholds<-sedim_thresholds%>%
  select(-Substance.X,-Harmon_name,-PARAM,-Description,-CAS,-PARAM.Group,-P_source,-Multiplier,-Group)%>%distinct()

sedim<-sedim%>%
  filter(!(substance %in% c("tbtin")))

#Join the data with the thresholds

sedim<-left_join(sedim,sedim_thresholds,by=c("substance"="Substance name"))
no_th<-sedim%>%filter(is.na(Threshold_value))%>%select(substance)%>%distinct()
print(sort(no_th$substance))
check<-names_sedim[names_sedim %in% no_th]
if (length(check)>0) {
  cat("Parameters that should have threshold:\n",check,"\n",sep="\n")
} else {
  print("All parameters have threshold.\n")
}
sedim<-sedim%>%filter(!is.na(Threshold_value))%>%select(-Category)
names(sedim)<-tolower(names(sedim))
# Save the data


#write.table(sedim,file=paste0("input data/sediment2025_withthresholds.csv"),sep=";",row.names = F)
rm(sedim_thresholds,no_th, check, names_sedim)

# BIOTA ----

# Check parameters present in the data

ni<-nrow(biota)
biota_thresholds <- threshold_values("Biota")
names_biota<-sort(unique(biota_thresholds$`Substance name`))

df1<-sort(unique(biota_thresholds$`Substance name`))
df2<-sort(unique(biota$substance))
check<-df1[!df1 %in% df2]


if (length(check)>0) {
  cat("These elements are not in the dataset:\n",check,"\nIf they are from a group (for example,nonylphenols (4-nonylphenol)), it is okey, if not, re-check the code.\n",sep="\n")
} else {
  print("All elements present in the dataset.\n")
}
rm(df1,df2,check)
#in this assessment, there are no data for: 
#trichloro-benzene, dichloromethane, endosulfan.

# Add threshold for parameter 
biota_parameter<-biota%>%
  filter(is.na(multiplier))%>%
  group_by(gridcode,region,station, date,longitude, latitude, type, species,matrx,basis, substgroup,substance, munit, category, source) %>%
  summarise(value=mean(value,na.rm=T),
            drywt=mean(drywt,na.rm = T),
            fatwt=mean(fatwt,na.rm = T),
            .groups = "drop")

# Add threshold for group 

biota_group<- biota%>%
  filter(!is.na(multiplier))%>%
  split(.$grouping)%>%
  map(~ .x %>%
        group_by(gridcode,region,station, date,longitude, latitude,type,species,substance, substgroup, basis, matrx, munit, category, source,grouping,multiplier) %>%
        summarise(value=mean(value),
                  drywt=mean(drywt,na.rm = T),
                  fatwt=mean(fatwt,na.rm = T),
                  .groups = "drop"))%>%
  map(~.x %>%
        group_by(gridcode,region,station, date,longitude,latitude, type,species, substgroup,grouping,basis, matrx, munit, category, source)%>%
        summarise(value= sum(value*multiplier),
                  drywt=mean(drywt,na.rm = T),
                  fatwt=mean(fatwt,na.rm =T),
                  .groups = "drop"))

#first we calculated the mean for parameters that have measured more than once on the same day, and then we use the multiplier to sum the different parameters into the groups.
biota_group<- biota_group%>%
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

# Join data together

biota<-bind_rows(biota_parameter,biota_group)
biota<-biota%>%
  mutate(across(everything(), ~ replace(., is.nan(.), NA)))

nj<-nrow(biota)
cat("Before merging the parameter groups, the data had",ni,"rows. After checking, we have",nj,"rows.")
#Before merging the parameter groups, the data had 1272065 rows. After checking, we have 324533 rows.

rm(ni,nj,biota_group,biota_parameter)

# Harmonize matrix and basis of the threshold values  

biota_thresholds<-biota_thresholds%>%
  select(-Substance.X,-Harmon_name,-PARAM,-Description,-CAS,-PARAM.Group,-P_source,-Multiplier,-Category,-Group)%>%distinct()

biota_thresholds<-biota_thresholds%>%
  mutate(Threshold_matrix= tolower(Threshold_matrix))%>%
  mutate(Threshold_matrix= case_when(
    .$Threshold_matrix %in% c("whole") ~ "SB",
    .$Threshold_matrix %in% c("muscle") ~ "MU",
    .$Threshold_matrix %in% c("liver") ~ "LI",
    .$Threshold_matrix %in% c(NA_character_) ~ "SB",
    TRUE ~ NA_character_
  ))

biota_thresholds<-biota_thresholds%>%
  mutate(Threshold_basis= ifelse(is.na(Threshold_basis),"W",Threshold_basis))
biota_thresholds<-biota_thresholds%>%
  mutate(Biota_type= ifelse(is.na(Biota_type),
                            "Both",
                            Biota_type))%>%
  distinct()

# Filter out the tbtin, since our threshold is for the tbsn+

biota<-biota%>%
  filter(!(substance %in% c("tbtin")))

#Join the data with the thresholds

biota<-left_join(biota,biota_thresholds,by=c("substance"="Substance name"), na_matches = "never", relationship = "many-to-many")
no_th<-biota%>%filter(is.na(Threshold_value))%>%select(substance)%>%distinct()
print(sort(no_th$substance))
check<-names_biota[names_biota %in% no_th]
if (length(check)>0) {
  cat("Parameters that should have threshold:\n",check,"\n",sep="\n")
} else {
  cat("All parameters have threshold.\n")
}

biota<-biota%>%filter(!is.na(Threshold_value))

names(biota)<-tolower(names(biota))

# Filtering out the no-matches between specie values and matrix, and threshold

filtering1<-biota%>% #---50862
  mutate(keep= case_when(
    .$type == .$biota_type ~ "keep",
    .$biota_type=="Both" ~ "keep",
    .$type=="Shellfish" & .$biota_type=="Crustacean" ~ "out",
    .$type=="Shellfish" & .$biota_type=="Fish" ~ "out",
    .$type=="Crustacean" & .$biota_type=="Fish" ~ "out",
    .$type=="Crustacean" & .$threshold_sp=="Mytilus" ~ "out",
    .$type=="Crustacean" & .$threshold_sp=="Oysters" ~ "out",
    .$type=="Crustacean" & .$biota_type=="Shellfish" & is.na(.$threshold_sp) ~ "keep",
    .$type=="Fish" & .$biota_type=="Shellfish" ~ "out",
    .$type=="Fish" & .$biota_type=="Crustacean" ~ "out",
    is.na(.$biota_type) ~"keep",
    TRUE ~ NA_character_
  ))%>%
  filter(keep=="keep")%>%
  select(-keep)%>%
  distinct()

filtering2<-filtering1%>%
  mutate(keep= case_when(
    .$threshold_sp=="Mytilus" & grepl("Mytilus",species) ~ "keep",
    .$threshold_sp=="Mytilus" & !grepl("Mytilus",species) ~ "out",
    .$threshold_sp=="Oysters" & grepl("Ostrea",species) ~"keep",
    .$threshold_sp=="Oysters" & !grepl("Ostrea",species) ~"out",
    TRUE ~ "keep"))%>%
  filter(keep=="keep")%>%
  select(-keep)
filtering2<-filtering2%>%
  group_by(date,station,longitude,latitude,substance,species,value,matrx,basis)%>%
  mutate(n=n(),ID=cur_group_id())%>%
  arrange(substance,species,n,ID)%>%
  ungroup()

filtering3 <- filtering2 %>%
  mutate(priority = case_when(
    .$n == 1 ~ 0,
    .$n == 2 & .$matrx==.$threshold_matrix ~ 0,
    .$n == 2 & .$matrx!=.$threshold_matrix ~ 1,
    .$n == 2 & .$basis==.$threshold_basis ~ 0,
    .$n == 2 & .$basis!=.$threshold_basis ~ 1,
    TRUE ~ NA_real_)) %>%
  relocate(c(ID,priority),.before=everything())%>%
  arrange(ID,priority)%>%
  group_by(ID)%>%
  filter(priority== min(priority)) %>% 
  ungroup()%>%
  select(-n, -ID, -priority)

biota<-filtering3

rm(filtering1,filtering2,filtering3)
rm(biota_thresholds)

# Converting basis values to match threshold

biota<-biota%>%
  mutate(drywt= ifelse(is.nan(drywt),NA,drywt),
         fatwt= ifelse(is.nan(fatwt),NA,fatwt))

species_weight<-biota%>%
  select(species,matrx,basis,drywt,fatwt)%>%
  distinct()%>%
  mutate_all(~ ifelse(is.nan(.), NA, .))%>%
  filter(!(is.na(drywt) & is.na(fatwt)))

species_lw<-biota%>%
  select(species,matrx, basis,fatwt,drywt)%>%
  group_by(species,matrx,basis)%>%
  summarise(mean_lw=mean(fatwt,na.rm=T),SD_lw=sd(fatwt,na.rm=T),n_lw=n(),.groups="drop")

species_dw<-biota%>%
  select(species,matrx, basis,fatwt,drywt)%>%
  group_by(species,matrx,basis)%>%
  summarise(mean_dw=mean(drywt,na.rm = T),SD_dw=sd(fatwt,na.rm=T),n_dw=n(),.groups="drop")

species_means<-left_join(species_dw,species_lw, by= c("species", "matrx","basis"))%>%
  select(-SD_lw,-SD_dw,-n_lw,-n_dw)%>%
  mutate_all(~ ifelse(is.nan(.), NA, .))%>%
  filter(!(is.na(mean_dw) & is.na(mean_lw)))

species_lw_type<-biota%>%
  select(type,matrx, basis,fatwt,drywt)%>%
  group_by(type,matrx,basis)%>%
  summarise(mean_lw=mean(fatwt,na.rm=T),SD_lw=sd(fatwt,na.rm=T),n_lw=n(),.groups="drop")

species_dw_type<-biota%>%
  select(type,matrx, basis,fatwt,drywt)%>%
  group_by(type,matrx,basis)%>%
  summarise(mean_dw=mean(drywt,na.rm = T),SD_dw=sd(fatwt,na.rm=T),n_dw=n(),.groups="drop")

species_means_type<-left_join(species_dw_type,species_lw_type, by= c("type", "matrx","basis"))%>%
  select(-SD_lw,-SD_dw,-n_lw,-n_dw)%>%
  mutate_all(~ ifelse(is.nan(.), NA, .))%>%
  filter(!(is.na(mean_dw) & is.na(mean_lw)))

rm(species_dw_type,species_lw_type)

species_ices<-read.csv(paste0(source_folder,"thresholds/matrix_conversion_ices.csv"),sep=";")
species_ices <- species_ices %>%
  pivot_longer(cols = c(-species,-common.name), names_to = "variable") %>%
  mutate(matrx = case_when(
    grepl("muscle", variable, ignore.case = TRUE) ~ "MU",
    grepl("liver", variable, ignore.case = TRUE) ~ "LI",
    grepl("soft.body", variable, ignore.case = TRUE) ~ "SB",
    grepl("tail.muscle", variable, ignore.case = TRUE) ~ "TM"
  ),
  basis = case_when(
    grepl("lw", variable, ignore.case = TRUE) ~ "L",
    grepl("dw", variable, ignore.case = TRUE) ~ "D"
  )) %>%
  select(-variable)%>%filter(!is.na(value))%>%
  rename("value_weight"="value")
species_ices_lw<-species_ices%>%
  filter(basis=="L")%>%
  select(-common.name,-basis)
species_ices_dw<-species_ices%>%
  filter(basis=="D")%>%
  select(-common.name,-basis)

biota <- biota %>%
  left_join(species_means, by = c("species", "matrx", "basis")) %>%
  mutate(drywt = coalesce(drywt, mean_dw),#this function fills NA values with the second df
         fatwt = coalesce(fatwt, mean_lw)) %>%
  select(-mean_dw, -mean_lw) 

biota<-biota%>%
  left_join(species_ices_dw, by=c("species","matrx"))%>%
  mutate(drywt= coalesce(drywt,value_weight))%>%
  select(-value_weight)

biota<-biota%>%
  left_join(species_ices_lw, by=c("species","matrx"))%>%
  mutate(fatwt= coalesce(fatwt,value_weight))%>%
  select(-value_weight)%>%
  filter(!is.na(threshold_value))

biota <- biota %>%
  left_join(species_means_type, by = c("type", "matrx", "basis")) %>%
  mutate(drywt = coalesce(drywt, mean_dw),#this function fills NA values with the second df
         fatwt = coalesce(fatwt, mean_lw)) %>%
  select(-mean_dw, -mean_lw) 

summary_biota<-biota%>%
  group_by(region,substance,species)%>%
  summarise(dates=n_distinct(date),gridcodes=n_distinct(gridcode),.groups = "drop")  

names(biota)<-tolower(names(biota))

rm(species_ices,species_ices_dw,species_ices_lw,species_dw,species_lw,species_means_type,species_means,species_weight)

# Save the data

#write.table(biota,file=paste0("input data/biota2025_withthresholds.csv"),sep=";",row.names = F)
rm(summary_biota, no_th)

# BIOEFFECTS ----

# Already added them in the processing.


#save.image("C:/Data/Repositories/CHASE/ETC_CHASE_2024/R data/data_thresholds.RData")
