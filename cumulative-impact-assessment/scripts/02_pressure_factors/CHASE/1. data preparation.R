source("scripts/00_setup.R")
PATHS <- set_project_paths()


folder <- "C:\\Users\\SIO\\NIVA\\Køge Bugt - General\\data\\Pressure\\kemisk_forurening\\ICES_WISE_EMOD"

source("C:/Users/SIO/KogeBugt/R projects/R projects/KogeBugt/cumulative-impact-assessment/scripts/02_pressure_factors/CHASE/processing.R")
source("C:/Users/SIO/KogeBugt/R projects/R projects/KogeBugt/cumulative-impact-assessment/scripts/02_pressure_factors/CHASE/utilities.R")
Sys.setlocale("LC_ALL", "en_US.UTF-8")
# WATER ----

# Add here the path of your data:


ICESpath <- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","ICES","DomeSeawater_Data_0723112447.csv"))
  

ices_df <- read.table(file = ICESpath, sep = ",", header = TRUE,
           comment.char = "", quote = "\"",
           fileEncoding = "UTF-8-BOM") %>%
  filter(Latitude >=55 & Latitude <=60 & Longitude >= 11 & Longitude <= 13)


ices_df_tst <- ices_df %>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>%
  filter(MYEAR >= 2010)

EMODNETpath<- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","EMD_Contaminants_Baltic_2026_unrestricted","EMD_contaminants_Baltic_profiles_water_2026_unrestricted_transposed.txt"))


emodnet_df <- read.table(EMODNETpath, sep = "\t", header = TRUE,
           na.strings   = c("NA", "", " "),
           quote        = "",      # apostroffer i stofnavne må ikke tolkes som citat
           comment.char = "",
           fileEncoding = "UTF-8")

emodnet_df_tst <- emodnet_df %>%
  filter(Latitude..degrees_north. >=55 & Latitude..degrees_north. <=60 & Longitude..degrees_east. >= 11 & Longitude..degrees_east. <= 13
         ) %>%
  mutate(DATE = as.Date(yyyy.mm.ddThh.mm.ss.sss, format = "%Y/%m/%d")) %>%
  filter(year(DATE) >= 2010)
###

# ############# Ingen kemi Data i Koge bugt i WISE datasæt ################################ 

#WISEpath<- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","waterbase_t_wise6_disaggregateddata.csv"))
# spatial_data_wise <- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","spatial_data_original_WISE.csv"))
# 
# 
# ## Hentet vha duckdb nedenfor, kun udsnit for Køge Bugt Området
# WISEpath <- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","wise_water_projektomraade.csv"))
# 
# 
# readLines(WISEpath, n = 3)
# 
# 
# country_code_wise <- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","Country_codelist_dataelements_68941_WISE.csv"))
# 
# 
# # install.packages("duckdb")
# # library(duckdb)
# # 
# # WISE_full  <- file.path(PATHS$input_pressure, "kemisk_forurening", "ICES_WISE_EMOD",
# #                         "waterbase_t_wise6_disaggregateddata.csv")
# # WISE_small <- file.path(PATHS$input_pressure, "kemisk_forurening", "ICES_WISE_EMOD",
# #                         "wise_water_projektomraade.csv")
# # 
# # con <- dbConnect(duckdb())
# # 
# # dbExecute(con, sprintf("
# #   COPY (
# #     SELECT w.*
# #     FROM read_csv('%s', header = true, delim = ';', quote = '\"', all_varchar = true) AS w
# #     WHERE w.parameterWaterBodyCategory IN ('CW', 'TW')          -- kyst- og overgangsvand
# #       AND w.monitoringSiteIdentifier IN (
# #             SELECT Station FROM read_csv('%s', header = true)
# #             WHERE lat BETWEEN 54 AND 56.7                     -- projektområdet + buffer
# #               AND lon BETWEEN 11 AND 14)
# #   ) TO '%s' (HEADER, DELIMITER ';')",
# #                        WISE_full, spatial_data_wise, WISE_small))
# # 
# # dbDisconnect(con, shutdown = TRUE)
# # 
# # WISEpath <- WISE_small
# # WISE_water <- process_water("WISEpath")
# #####




  
# Processing of the data 

ICES_water <- process_water("ICESpath")#---723370

ICES_water <- ICES_water %>%filter(Latitude >=55 & Latitude <=60 & Longitude >= 11 & Longitude <= 13,
                                   !Country =="Russia",
                                   !PARAM == "%oxsat" )

EMOD_water <- process_water("EMODNETpath")#--480174

EMOD_water <- EMOD_water %>%
  filter(Latitude >=55 & Latitude <=60 & Longitude >= 11 & Longitude <= 13)

### Ingen nye målinger i ICES og slet ingen i EMODnet
#--------------------------------------------------- SEDIMENT --------------------------

# Add here the path of your data:


ICESpath <- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","ICES","DomeSediment_Data_0723110032.csv"))


EMODNETpath<- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","EMD_Contaminants_Baltic_2026_unrestricted","EMD_contaminants_Baltic_profiles_sediment_2026_unrestricted_transposed.txt"))
# Processing of the data 

ICES_sed<- process_sediments("ICESpath")#-- 774589

ICES_sed  <- ICES_sed %>%
  filter(Latitude >=55 & Latitude <=60 & Longitude >= 11 & Longitude <= 13,
            !Country =="Russia")

EMOD_sed <- process_EMODNET_sediment(EMODNETpath) 

EMOD_sed <- EMOD_sed %>%
  filter(Latitude >=55 & Latitude <=60 & Longitude >= 11 & Longitude <= 13)


sedim<-bind_rows(ICES_sed,EMOD_sed)%>% #-------1285886
  mutate(CAS= ifelse(PARAM=="potassium","7440-09-7",CAS), #for emodnet, the CAS number was wrong
         Description = trimws(Description)) %>%
  filter(!is.na(Value))#it deletes the whitespace at the end or beginning of a string.
nrow(sedim)
# Check for duplicates

sed_upper <- sedim 

sedim_harm <- parameter_harmonization(sed_upper)
names(sedim_harm) <- tolower(names(sedim_harm))

sedim <- sedim_harm

checking_dup <- sedim %>%                       # hvis n>1, er data gentaget
  dplyr::select(latitude, longitude, date, source, pargroup,) %>%   # <- lat/lon i stedet for gridcode
  unique() %>%
  group_by(latitude, longitude, date, pargroup,param) %>%                   # <- lat/lon
  summarise(count = n(), .groups = "drop")

repeated <- checking_dup %>%
  filter(count > 1) %>% 
  select(-count)

# mere end én måling pr. dag på samme position betyder ikke nødvendigvis en dublet
# (kan være forskellig dybde eller tid) — så vi går dybere.

repeated <- left_join(repeated, sedim, by = NULL, relationship = "many-to-many")

repeated2 <- repeated %>%
  mutate(CustomOrder = case_when(
    source == "ICES"    ~ 1,
    source == "EMODNET" ~ 2)) %>%               # <- WISE fjernet
  group_by(latitude, longitude, date, param, value) %>%            # <- lat/lon; samme værdi registreret?
  mutate(    Repeated = ifelse(n() > 1, "Repeated", "Not Repeated")) %>%
  group_by(latitude, longitude) %>%                              # kommer de fra samme kilde?
  mutate(
    Source_2 = ifelse(n_distinct(source) > 1, "Different", "Same")) %>%
  relocate(c("Repeated", "Source_2"), .before = everything()) %>%
  arrange(latitude, longitude, date, param, cas, value)            # <- lat/lon

# gentaget værdi (samme tal) fra FORSKELLIG kilde -> brug prioritet (ICES > EMODNET);
# samme kilde -> behold (forskellig tid/dybde).

repeated2 <- repeated2 %>%
  group_by(latitude, longitude) %>%
  mutate(keep = ifelse(Repeated == "Not Repeated", "keep",
                       ifelse(Source_2 == "Same", "keep",
                              ifelse(CustomOrder == min(CustomOrder), "keep", "out")))) %>%
  ungroup() %>%
  filter(keep != "out") %>%
  select(-keep, -CustomOrder, -Repeated, -Source_2)

sedim <- anti_join(sedim, repeated, by = NULL) %>%   # fjern alle gentagne
  bind_rows(repeated2) %>%                            # tilføj kun én pr. gentaget værdi
  mutate(value = as.numeric(value)) %>%
  mutate(matrx = "S",
         yr = year(date)) %>%
  filter(yr >= 2010)


write.csv(sedim, file.path(PATHS$input_pressure, "kemisk_forurening","ICES_WISE_EMOD","sediment_cleaned.csv"),row.names = FALSE)

# BIOTA ----

# Add here the path of your data:


ICESpath <- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","ICES","DomeBiota_Data_0723115563.csv"))


EMODNETpath<- paste0(file.path(PATHS$input_pressure,"kemisk_forurening","ICES_WISE_EMOD","EMD_Contaminants_Baltic_2026_unrestricted","EMD_contaminants_Baltic_profiles_biota_2026_unrestricted_transposed.txt"))



# Processing of the data 

#WISEpath<- paste0(source_folder,"raw_data/original_WISE_CW_to_be_cleaned.csv")

# Processing of the data 

ICES_bio<- process_biota("ICESpath")#---3005731

ICES_bio <- ICES_bio %>%
  filter(Latitude >=55 & Latitude <=60 & Longitude >= 11 & Longitude <= 13,
         !Country =="Russia")


EMOD_bio <- process_EMODNET_biota(EMODNETpath) 

EMOD_bio <- EMOD_bio  %>%
  filter(Latitude >=55 & Latitude <=60 & Longitude >= 11 & Longitude <= 13)

biota<-bind_rows(ICES_bio,EMOD_bio)%>%#----- 3604720
  mutate(Description = trimws(Description))%>%
  mutate(PARAM= ifelse(is.na(PARAM) & Source=="ICES" & PARGROUP=="I-MET","na",PARAM))
nrow(biota)
#rm(ICES_bio,EMOD_bio,WISE_bio,WISEpath,EMODNETpath,ICESpath)

# Data harmonization

gc()


# Grid data

biota_harm <- parameter_harmonization(biota)

names(biota_harm) <- tolower(names(biota_harm))
ni<-nrow(biota_harm)
nj<-nrow(biota_harm)

biota<-biota_harm%>%
  select(-country)

cat("The full biota data contained",ni,"rows of data, but after considering only data within the EEA grid we get",nj,"rows of data.\n")
#The full biota data contained 1589200 rows of data, but after considering only data within the EEA grid we get 1557827 rows of data.

rm(ni,nj)
rm(biota_harm)

# Convert data units to the same one per parameter

biota<-biota%>%
  filter(!is.na(species))%>%
  mutate(matrx= ifelse(matrx=="WO","SB",matrx)) #to have the same names

biota<-biota%>%
  mutate(munit=tolower(munit))
matching<-biota%>%
  select(munit)%>%
  distinct()

ni<-nrow(biota)
cat("The data has these units:\n")
cat(sort(unique(matching$munit)),sep="\n")
cat("The code can take care of conversion to ug/kg, from the units: g/g, gm/g, mg/kg, ng/g, ng/kg, pg/g, pg/kg, and ug/g. If present unit not included here, check the code.")

biota <- biota %>%
  filter(!(munit == "umol/min/mg protein")) %>%
  mutate(konv = case_when(
    munit %in% c("g/g","gm/g")                      ~ 1e9,
    munit == "mg/kg"                                ~ 1e3,
    munit == "ug/g"                                 ~ 1e3,
    munit == "ng/g"                                 ~ 1,
    munit == "ng/kg"                                ~ 1/1e3,
    munit == "pg/g"                                 ~ 1/1e3,
    munit == "pg/kg"                                ~ 1/1e6,
    munit %in% c("ug/kg","ug sn/kg","ug{teq}/kg")   ~ 1,
    TRUE ~ NA_real_),
    value = ifelse(is.na(konv), value, value * konv),
    limit = ifelse(is.na(konv), limit, limit * konv),   # <- samme omregning på grænsen
    munit = ifelse(is.na(konv), munit, "ug/kg")) %>%
  select(-konv)

rm(matching)

# Check for harmonization of species

biota_fish <- c("Abramis brama", "Anguilla anguilla", "Anarhichas lupus", 
                "Anarhichas minor", "Aphanopus carbo", "Bathysaurus ferox",
                "Boreogadus saida", "Brosme brosme", "Capros aper", "Chelidonichthys kumu",
                "Chelon labrosus", "Chimaera monstrosa", "Clupea harengus",
                "Clupea harengus membras", "Conger conger", "Coryphaenoides rupestris",
                "Eutrigla gurnardus", "Gadus morhua", "Glyptocephalus cynoglossus",
                "Gobius", "Gobius niger", "Helicolenus dactylopterus",
                "Hippoglossoides platessoides",  "Hoplostethus atlanticus",
                "Leucoraja naevus", "Lepidorhombus whiffiagonis", "Limanda limanda",
                "Liza ramada", "Lophius budegassa", "Lophius piscatorius",
                "Mallotus villosus", "Melanogrammus aeglefinus", "Merlangius merlangus",
                "Merluccius merluccius", "Microstomus kitt", "Micromesistius poutassou",
                "Molva dypterygia", "Molva molva", "Mullus barbatus", "Myoxocephalus scorpius",
                "Neogobius melanostomus", "Osmerus eperlanus", "Perca fluviatilis",
                "Phycis phycis", "Platichthys flesus", "Pleuronectes platessa",
                "Pollachius pollachius", "Pollachius virens", "Psetta maxima",
                "Raja brachyura", "Raja clavata", "Raja montagui",
                "Reinhardtius hippoglossoides", "Salmo salar", "Salmo trutta",
                "Salvelinus alpinus", "Sardina pilchardus", "Scophthalmus aquosus",
                "Scophthalmus rhombus", "Scyliorhinus canicula", "Sebastes marinus",
                "Serranus scriba", "Solea solea", "Solea vulgaris", "Scomber scombrus",
                "Sprattus sprattus", "Squalus acanthias", "Thunnus alalonga",
                "Thunnus alalunga", "Trachurus trachurus", "Trigla lucerna",
                "Triglidae", "Trisopterus luscus", "Zeus faber", "Zoarces viviparus")

biota_shellfish <- c("Bivalvia", "Bolinus brandaris", "Callista chione", "Cancer pagurus",
                     "Cerastoderma edule", "Cerastoderma glaucum", "Charonia rubicunda",
                     "Chamelea gallina", "Crassostrea angulata", "Crassostrea gigas",
                     "Crepidula fornicata", "Donax trunculus", "Dreissena",
                     "Dreissena polymorpha","Ensis arcuatus", "Ensis ensis", "Ensis siliqua",
                     "Glycymeris glycymeris", "Haliotis tuberculata", "Limecola balthica",
                     "Littorina littorea", "Loligo vulgaris", "Lutraria lutraria",
                     "Macoma baltica", "Macoma balthica", "Mya arenaria",
                     "Mya truncata", "Mytilus", "Mytilus edulis", "Mytilus galloprovincialis",
                     "Nassarius reticulatus", "Nucella lapillus", "Ostrea edulis",
                     "Patella", "Pecten maximus", "Pegusa lascaris", "Peringia ulvae",
                     "Pharus legumen", "Ruditapes decussatus", "Ruditapes philippinarum",
                     "Solen marginatus", "Spisula solida", "Tritia nitida", "Venus verrucosa",
                     "Venerupis decussata", "Venerupis philippinarum")

biota_crustacean <- c("Crangon crangon", "Maja squinado", "Mysidacea", "Nephrops norvegicus", "Palaemon serratus", "Saduria entomon")

biota <- biota %>%
  mutate(type = ifelse(species %in% biota_fish, "Fish",
                       ifelse(species %in% biota_shellfish, "Shellfish",
                              ifelse(species %in% biota_crustacean,"Crustacean",NA))))


out<-biota%>%select(species,type)%>%distinct()%>%filter(is.na(type))%>%select(species)%>%unique()
print(sort(unique(out$species)))
#if NA in the next species, dont worry, they are birds or algae.

# Balaenoptera acutorostrata
# Delphinus delphis
# Fucus vesiculosus (all fucus species)
# Haematopus ostralegus
# Holothuria tubulosa (sea cucumber)
# Larus argentatus 
# Larus hyperboreus
# Paracentrotus lividus (sea urchin)
# Phoca hispida
# Phoca vitulina
# Phocoena phocoena
# Rissa tridactyla
# Somateria mollissima 
# Sterna hirundo 
# Uria aalge 
# Uria lomvia

rm(out)

biota<-biota%>% #-----1554551
  filter(!is.na(type))
nrow(biota)
names(biota)<-tolower(names(biota))
rm(biota_crustacean,biota_fish,biota_shellfish)

# Check for duplicates 

biota$value <- format(biota$value, nsmall = 4)

checking_dup<-biota%>% #if n>1, then the data is repeated
  dplyr::select(latitude,longitude,date,substance,species,source)%>%
  unique()%>%
  group_by(latitude,longitude,date,substance,species)%>%
  summarise(count=n(),.groups = "drop")

repeated<-checking_dup%>% #-----------15932
  filter(count>1)%>%select(-count)

#all this data has more than one observation per day, 
#but this does not mean it is a duplicate, 
#it can be in different depths or different time. So we need to go more in depth.


repeated<-left_join(repeated,biota,by=NULL,relationship="many-to-many")

repeated2<-repeated%>%
  mutate(CustomOrder = case_when(
    source == "ICES" ~ 1,
    source == "EMODNET" ~ 2))%>%
  group_by(latitude,longitude,date,substance,species,value)%>% #check if its the same value registered
  mutate(
    ID=cur_group_id(),
    Repeated = ifelse(n() > 1, "Repeated", "Not Repeated")) %>%
  group_by(ID)%>% #check if it comes from the same source
  fill(c("matrx","basis","drywt","fatwt"),.direction = "up")%>% #if they are the same value and another source has registered the matrx and basis, we will use it
  mutate(
    Source_2 = ifelse(n_distinct(source) > 1, "Different", "Same")) %>%
  relocate(c("ID","Repeated","Source_2"),.before=everything())%>%
  arrange(latitude,longitude,date,substance,cas,value)

#now, if its a repeated value (same number) and they come from a different source, 
#we will apply priority levels, and if they come from the same one, 
#we will keep them since maybe its because they are at different times or depths.

repeated2<-repeated2%>%
  group_by(latitude,longitude)%>%
  mutate(keep= ifelse(Repeated=="Not Repeated","keep",
                      ifelse(Source_2=="Same", "keep",
                             ifelse(CustomOrder==min(CustomOrder),"keep", "out"))))%>%
  ungroup()%>%
  filter(keep!="out")%>%
  select(-keep,-CustomOrder,-Repeated,-ID,-Source_2)


biota<-anti_join(biota,repeated,by=NULL)%>% #filter out all the repeated values 
  bind_rows(repeated2)%>% #add only one value per repeated value
  mutate(value=as.numeric(value))


write.csv(biota, file.path(PATHS$input_pressure, "kemisk_forurening","ICES_WISE_EMOD","biota_cleaned.csv"),row.names = FALSE)


# BIOEFFECTS ----

# Add here the path of your data:

bioeffpath<- paste0(source_folder,"raw_data/ICES_downloaded_2025/DomeBiota/DomeBiota_Data.csv")

# Processing of the data 

bioeff <- process_bioeffects(bioeffpath) #------2144

rm(bioeffpath)


# Data harmonization

gc()
bioeff_thresholds <- threshold_values("Bioeffects")%>%
  select(-Harmon_name,-PARAM,-Description,-CAS,-P_source)%>%distinct()


bioeffects <- left_join(bioeff, bioeff_thresholds, by = c("species" = "Threshold_sp", "substance"="Substance name"))

na_bioeffects <- bioeffects %>% filter(is.na(Threshold_value))

unique_species <- unique(na_bioeffects$species)
cat("Species without threshold value:",unique_species,"\n",fill = T,sep="\t")
cat("The code includes fixing threshold value for Tritia nitida (Nassarius nititdus).\n")


#Tritia nitida and Nassarius nitidus, are the same species, Nassarius nitidus is not accepted. Source: https://www.marinespecies.org/aphia.php?p=taxdetails&id=140509
#According to OSPAR, Nassarius species have a threshold value of 0.3 Source: https://www.ospar.org/documents?v=7316

bioeffects <- bioeffects%>%
  mutate(Threshold_value= ifelse(species %in% c("Tritia nitida","Nassarius nitidus") , 0.3,Threshold_value))%>%
  select(-category)

na_bioeffects <- bioeffects %>% filter(is.na(Threshold_value))

names(bioeffects)<-tolower(names(bioeffects))
bioeffects<-bioeffects%>%
  mutate(tblbioid=as.character(tblbioid),
         tblsampleid= as.character(tblsampleid))
cat("Observations without threshold value:",nrow(na_bioeffects),"\n")
#Observations without threshold value: 19 

rm(na_bioeffects,bioeff,bioeff_thresholds,unique_species)


# Convert data units to the same one per parameter

bioeffects<-bioeffects%>%
  mutate(munit=tolower(munit),
         threshold_unit= ifelse(substance=="vas deferens sequence index (vdsi)","idx",NA))%>%
  mutate(munit= ifelse(munit=="%","idx",munit))%>%
  mutate(category="Bioeffects")

ni<-nrow(bioeffects)

bioeffects_mean<-bioeffects%>%
  group_by(gridcode,region,station,latitude,longitude,date,species,matrx,substgroup,substance)%>%
  summarise(value=mean(value),.groups="drop")


# Re-add threshold values

threshold_bioeffects<-bioeffects%>%
  select(substance,matrx,munit,category,threshold_type,threshold_value,threshold_source,threshold_unit,species)%>%
  distinct()%>%
  group_by(substance,matrx,munit,category,threshold_type,threshold_value,threshold_source,threshold_unit,species)%>%
  slice(1)%>%
  ungroup()

bioeffects<-left_join(bioeffects_mean,threshold_bioeffects, by=c("substance", "matrx","species"),na_matches="never")#-----2537

nj<-nrow(bioeffects)
cat("Original bioeffects data had",ni,"rows, but after merging the grouping parameters we have", nj,"data rows.")
#Original bioeffects data had 2144 rows, but after merging the grouping parameters we have 2142 data rows.
rm(bioeffects_mean,threshold_bioeffects)
rm(ni,nj)

summary_bioeffects<-bioeffects%>%
  group_by(gridcode,substance)%>%
  summarise(n=n(),.groups="drop")

# No need to check duplicates, only one dataset (ICES)

# Save the data

#write.table(bioeffects,file=paste0("input data/bioeffects2025_withthresholds.csv"),sep=";",row.names = F)
#write.table(summary_bioeffects,file=paste0("input data/summary_bioeffects2025.csv"),sep=";",row.names = F)
rm(summary_bioeffects)

#save.image("C:/Data/Repositories/CHASE/ETC_CHASE_2024/R data/data_processing.RData")
