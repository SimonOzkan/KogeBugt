#This script prepares the EMODnet/WISE/ICES data 

library(dplyr)
library(tidyr)
library(sf)
library(readxl)
library(data.table)

stationSamples <- fread("C:/Users/SIO/OneDrive - NIVA/Projekter/2024/HEAT-Eutrophication/data/indicator/baltic/StationSamples2014-2023_1_region.csv") 
stationSamples_Kattegat <- fread("C:/Users/SIO/OneDrive - NIVA/Projekter/2024/HEAT-Eutrophication/data/indicator/baltic/StationSamples2014-2023_2_region.csv") 

stationSamples_original <- stationSamples %>%
  rename("Latitude" = "Latitude..degrees_north.",
         "Longitude" = "Longitude..degrees_east.",
         "Sounding" = "Bot..Depth..m.",
         "Bathymetric" = "BathymetricAvg",
         "Depth" = "Depth..m.",
         "DepthQ" = "QV.ODV.Depth..m.",
         "Temperature" = "Temperature..degC.",
         "TemperatureQ" = "QV.ODV.Temperature..degC.",
         "Salinity" = "Practical.Salinity..dmnless.",
         "SalinityQ" = "QV.ODV.Practical.Salinity..dmnless.",
         "Oxygen" = "Dissolved.Oxygen..ml.l.",
         "OxygenQ" = "QV.ODV.Dissolved.Oxygen..ml.l.",
         "Phosphate" = "Phosphate.Phosphorus..PO4.P...umol.l.",
         "PhosphateQ" = "QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l.",
         "TotalPhosphorus" = "Total.Phosphorus..P...umol.l.",
         "TotalPhosphorusQ" = "QV.ODV.Total.Phosphorus..P...umol.l.",
         "Nitrate" = "Nitrate.Nitrogen..NO3.N...umol.l.",
         "NitrateQ" = "QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l.",
         "Nitrite" = "Nitrite.Nitrogen..NO2.N...umol.l.",
         "NitriteQ" = "QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l.",
         "Ammonium" = "Ammonium.Nitrogen..NH4.N...umol.l.",
         "AmmoniumQ" = "QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l.",
         "TotalNitrogen" = "Total.Nitrogen..N...umol.l.",
         "TotalNitrogenQ" = "QV.ODV.Total.Nitrogen..N...umol.l.",
         "HydrogenSulphide" = "Hydrogen.Sulphide..H2S.S...umol.l.",
         "HydrogenSulphideQ" = "QV.ODV.Hydrogen.Sulphide..H2S.S...umol.l.",
         "Chlorophyll" = "Chlorophyll.a..ug.l.",
         "ChlorophyllQ" = "QV.ODV.Chlorophyll.a..ug.l.")

stationSamples_original_kat <- stationSamples_Kattegat %>%
  rename("Latitude" = "Latitude..degrees_north.",
         "Longitude" = "Longitude..degrees_east.",
         "Sounding" = "Bot..Depth..m.",
         "Bathymetric" = "BathymetricAvg",
         "Depth" = "Depth..m.",
         "DepthQ" = "QV.ODV.Depth..m.",
         "Temperature" = "Temperature..degC.",
         "TemperatureQ" = "QV.ODV.Temperature..degC.",
         "Salinity" = "Practical.Salinity..dmnless.",
         "SalinityQ" = "QV.ODV.Practical.Salinity..dmnless.",
         "Oxygen" = "Dissolved.Oxygen..ml.l.",
         "OxygenQ" = "QV.ODV.Dissolved.Oxygen..ml.l.",
         "Phosphate" = "Phosphate.Phosphorus..PO4.P...umol.l.",
         "PhosphateQ" = "QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l.",
         "TotalPhosphorus" = "Total.Phosphorus..P...umol.l.",
         "TotalPhosphorusQ" = "QV.ODV.Total.Phosphorus..P...umol.l.",
         "Nitrate" = "Nitrate.Nitrogen..NO3.N...umol.l.",
         "NitrateQ" = "QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l.",
         "Nitrite" = "Nitrite.Nitrogen..NO2.N...umol.l.",
         "NitriteQ" = "QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l.",
         "Ammonium" = "Ammonium.Nitrogen..NH4.N...umol.l.",
         "AmmoniumQ" = "QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l.",
         "TotalNitrogen" = "Total.Nitrogen..N...umol.l.",
         "TotalNitrogenQ" = "QV.ODV.Total.Nitrogen..N...umol.l.",
         "HydrogenSulphide" = "Hydrogen.Sulphide..H2S.S...umol.l.",
         "HydrogenSulphideQ" = "QV.ODV.Hydrogen.Sulphide..H2S.S...umol.l.",
         "Chlorophyll" = "Chlorophyll.a..ug.l.",
         "ChlorophyllQ" = "QV.ODV.Chlorophyll.a..ug.l.")

#Adding mm.dd and filtering duplicates -the group by excludes lat/lon and database, as these are the ones where the duplicated dataframes can occu
stationSamples <- stationSamples_original %>%
  distinct(BathymetricMin, BathymetricMax, Bathymetric, BathymetricStDev, 
           Within20km, m, iY, rK, iX, ClusterID, SeaRegionID, 
           Cruise, Station, Type, Year, Month, Day, Hour, Minute, Sounding, DataSourceID, 
           Depth, DepthQ, Temperature, TemperatureQ, Salinity, SalinityQ, Oxygen, OxygenQ, 
           Phosphate, PhosphateQ, TotalPhosphorus, TotalPhosphorusQ, Nitrate, NitrateQ, 
           Nitrite, NitriteQ, Ammonium, AmmoniumQ, TotalNitrogen, TotalNitrogenQ, 
           HydrogenSulphide, HydrogenSulphideQ, Chlorophyll, ChlorophyllQ, LOCAL_CDI_ID, 
           EDMO_code, `Instrument...gear.type`, SeaRegionName, .keep_all = TRUE) %>%
  mutate(mm.dd = sprintf("%02d.%02d", Month, Day)) %>%
  dplyr::select(-c(BathymetricMin,BathymetricMax,BathymetricStDev, Within20km, m, iY, rK, iX,Station,DataSourceID,LOCAL_CDI_ID))


stationSamples_kat <- stationSamples_original_kat %>%
  distinct(BathymetricMin, BathymetricMax, Bathymetric, BathymetricStDev, 
           Within20km, m, iY, rK, iX, ClusterID, SeaRegionID, 
           Cruise, Station, Type, Year, Month, Day, Hour, Minute, Sounding, DataSourceID, 
           Depth, DepthQ, Temperature, TemperatureQ, Salinity, SalinityQ, Oxygen, OxygenQ, 
           Phosphate, PhosphateQ, TotalPhosphorus, TotalPhosphorusQ, Nitrate, NitrateQ, 
           Nitrite, NitriteQ, Ammonium, AmmoniumQ, TotalNitrogen, TotalNitrogenQ, 
           HydrogenSulphide, HydrogenSulphideQ, Chlorophyll, ChlorophyllQ, LOCAL_CDI_ID, 
           EDMO_code, `Instrument...gear.type`, SeaRegionName, .keep_all = TRUE) %>%
  mutate(mm.dd = sprintf("%02d.%02d", Month, Day)) %>%
  dplyr::select(-c(Instrument...gear.type,BathymetricMin,BathymetricMax,BathymetricStDev, Within20km, m, iY, rK, iX,Station,DataSourceID,LOCAL_CDI_ID,EDMO_code)) 

#Filtering to only contain points around the Kattegat Sea
stationSamples_kat_coord <- stationSamples_kat %>%
  filter(Longitude >7.5 & Latitude<59) 

stationSamples_baltic <-stationSamples %>%
  rbind(stationSamples_kat_coord,  fill=TRUE) %>%
  mutate(SeaRegionID == 1) %>%
  filter(!DepthQ %in% c(4,8))



#### DIN #### (Winter) Nitrit-Nitrat 
#Filtering to only contain points around the Kattegat Seafiltering outliers for DIN 

baltic_Nutrient_percentile <- stationSamples_baltic %>%
  summarise(upperPercentileNO3 = quantile(Nitrate, 0.99, na.rm = TRUE),
            upperPercentileNO2 = quantile(Nitrite, 0.99, na.rm = TRUE),
            upperPercentileNH4 = quantile(Ammonium, 0.99, na.rm = TRUE))

baltic_DIN <- stationSamples_baltic %>%
  mutate(Nitrate = ifelse(NitrateQ == 4 | NitrateQ == 8, NA,Nitrate),
         Nitrite = ifelse(NitriteQ == 4 |NitriteQ == 8, NA, Nitrite),
         Ammonium = ifelse(AmmoniumQ == 4 |AmmoniumQ == 8, NA, Ammonium)) %>%
  filter(!(is.na(Nitrate) & is.na(Nitrite) & is.na(Ammonium)))%>%
  dplyr::select(Longitude, Latitude, ClusterID, SeaRegionID, Year, Month, Day, Hour, Minute, Depth,DepthQ,
                Nitrate,Nitrite,Ammonium) 

coalesce <- function(x) if (all(is.na(x)) | is.na(x["Nitrate"])) NA else sum(x, na.rm = TRUE)

baltic_DIN$DIN <- apply(baltic_DIN[, c("Nitrate", "Nitrite", "Ammonium")], 1, coalesce) 


baltic_DIN_df <- baltic_DIN %>%
  filter(!is.na(DIN), 
         Depth <= 10) %>%
  pivot_longer(cols = DIN,
               names_to = "Parameter",
               values_to = "Value") %>%
  mutate(unit = "umol/l")



write.csv2(baltic_DIN_df, "C:\\Users\\SIO\\NIVA\\Køge Bugt - General\\data\\Pressure\\MFS_DIN_DIP\\DIN_ices.csv", row.names = FALSE)


#### PO4/DIP ##### (Winter)
baltic_DIP <- stationSamples_baltic %>%
  mutate(Phosphate = ifelse(PhosphateQ == 4 | PhosphateQ == 8, NA,Phosphate)) %>%
  filter(!is.na(Phosphate),
         Depth <= 10) %>%
  dplyr::select(Longitude, Latitude, ClusterID, SeaRegionID, Year, Month, Day, Hour, Minute, Depth, DepthQ,Phosphate) %>%
  pivot_longer(cols = Phosphate,
               names_to = "Parameter",
               values_to = "Value") %>%
  mutate(Parameter = "DIP")

write.csv2(baltic_DIP, "C:\\Users\\SIO\\NIVA\\Køge Bugt - General\\data\\Pressure\\MFS_DIN_DIP\\DIP_ices.csv", row.names = FALSE)


