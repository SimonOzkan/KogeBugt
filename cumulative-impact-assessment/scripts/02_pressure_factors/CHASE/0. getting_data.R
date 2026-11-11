#it's better to run it as a background job
library(RSQLite)
library(purrr)
library(cli)
library(tidyverse)

#WISE---------
#https://discodata.eea.europa.eu/#

# to get data from WISE database we need to know how many rows of data there are and the pages we need. Since this factors are included in their SQL API.
 
# the category of waterbody is given a 2 or 3 character code
# https://dd.eionet.europa.eu/vocabulary/wise/WFDWaterBodyCategory/

# CW 	Coastal water body
# GW 	Groundwater body
# LW 	Lake water body
# MW 	Marine waters - Checked the data and it is the same as the CW data
# RW 	River water body
# TW 	Transitional water body
# TeW 	Territorial waters


# how many rows: SQL: SELECT COUNT(UID) AS "count" FROM [WISE_SOE].[latest].[Waterbase_T_WISE6_DisaggregatedData] WHERE [parameterWaterBodyCategory]='CW'
url <- "https://discodata.eea.europa.eu/sql?query=SELECT%20COUNT(UID)%20AS%20%22count%22%20FROM%20%5BWISE_SOE%5D.%5Blatest%5D.%5BWaterbase_T_WISE6_DisaggregatedData%5D%20WHERE%20%5BparameterWaterBodyCategory%5D%3D%27CW%27&p=1&nrOfHits=100&mail=null&schema=null"
res <- httr::GET(url)
if(res$status_code==200){
  content <- httr::content(res,as="text",encoding = "utf-8")
  dfn <- jsonlite::fromJSON(content)
  dfn <- as.data.frame(dfn)
  nrows <- dfn %>% pluck("count")
}else{
  # something didn't work
  message("WISE Data Request failed.")
}

page_size <- 2000
pages <- ceiling(nrows/page_size) #ceiling is to round it up to the top
pages <- 1:pages #447

#function to get all the URL for all the pages, so we get all the data
disco_url <- function(page, page_size=2000){
  url <- paste0("https://discodata.eea.europa.eu/sql?query=SELECT%20*%20FROM%20%5BWISE_SOE%5D.%5Blatest%5D.%5BWaterbase_T_WISE6_DisaggregatedData%5D%20WHERE%20%5BWISE_SOE%5D.%5Blatest%5D.%5BWaterbase_T_WISE6_DisaggregatedData%5D.%5BparameterWaterBodyCategory%5D%20%3D%20%27CW%27&p=", 
                page, "&nrOfHits=", page_size, "&mail=null&schema=null")
  return(url)
}

#function to get the data from the URL
get_disco <- function(url, id, max_tries=100, folder="raw_data/WISE_downloaded_2025/download_parts/", prog=NULL){
  retries <- 0
  success <- FALSE
df<- data.frame()
  
  while (!success && retries < max_tries){
    res <- httr::GET(url)
    if(res$status_code==200){
      content <- httr::content(res,as="text",encoding = "utf-8")
      df <- jsonlite::fromJSON(content)
      df <- as.data.frame(df)
      success <- TRUE
      names(df) <- stringr::str_remove(names(df), "results\\.")
      if(!is.na(folder)){
        saveRDS(df, file=paste0(folder,"page",id,".rds" ))
        #saveRDS(df, file=paste0(folder,"page387.rds" ))
      }
      res=0
    }else{
      # something didn't work
      retries <- retries + 1
      #
      message("WISE Data Request failed. Attempt ",retries," failed, Retrying...\n")
      Sys.sleep(7)
    }
  }
  if (!success) {
    message("WISE Data Request failed for URL after ", max_tries, " attempts:\n", url)
    df <- data.frame()
    res=1
  }
  
  cli::cli_progress_update(id = prog)
  return(res)
}

#run the functions, first the one to gather the  different url, and then the data one.
#pages<- 387
page_urls <- lapply(pages, disco_url)

# Create a progress bar
p <- cli::cli_progress_bar(total = length(page_urls))

res <- purrr::imap(page_urls, get_disco,prog=p) %>% unlist()

cli::cli_progress_done(id = p)

page_urls_problem <- page_urls[res!=0]


# Load all RDS files and combine them into a single dataframe
pag<-c()
folder_RDS<-"raw_data/WISE_downloaded_2025/download_parts/"
rds_files <- list.files(path=folder_RDS, pattern="*.rds", full.names=T)
file_df <- lapply(rds_files, readRDS)
#check that the data has been gathered adequately

for(i in 1:length(file_df)){
if (nrow(file_df[[i]])==2000) {
  next
}else {
  cat("There is missing data from the RDS files. Check page",i,"\n")
  
  pag<-c(pag,i)
  next
  }
}


#if pag exists rerun the res function for those pages. Be careful because it will
#save it as pag1 and so on... so you need to change the saving function and go one by one.

file_df <- bind_rows(lapply(rds_files, readRDS))%>%distinct()


if (nrow(file_df)==dfn$count) {
  cat("All data has been retrieved successfully from WISE database.\n")
}else {
  cat("There is missing data from the RDS files. Data should have", dfn$count, "rows, but has", nrow(file_df),"rows.\n")
}

write.table(file_df,file="raw_data/original_WISE_CW_to_be_cleaned.csv",row.names = F,sep=";") #-----892399

rm(file_df,rds_files,folder_RDS)

#It may be that some symbols have not been well saved. Open the file created and look for
#"<u+", and change it accordingly.


#EMODNET-------
# EMODnet data are delivered by EMODnet chemistry 
# You need to download the most updated data folders
# https://emodnet.ec.europa.eu/en/chemistry
library(RSQLite)
library(purrr)
library(cli)
library(tidyverse)


filepath <- "raw_data/EMODNET_downloaded_2025/"
folders <- list.files(path=filepath,
                      pattern="EMD_Contaminants_",full.names=T)

#function ro read EMODNET files
read_emodnet <- function(file,id){
  df <- read.table(file,
                   header=T,
                   sep="\t",
                   #fileEncoding="UTF-8", 
                   quote='',
                   fill=T,
                   colClasses="character",
                   na.strings = "NA",
                   dec = ".")
  return(df)
}

#retrieve all the files where data has been stored
all_filenames<-c()
for (folder in folders){
  filenames <- list.files(path=folder,
                          pattern="*transposed*",
                          full.names=T)
  all_filenames <- c(all_filenames, filenames)
}

folder_RDS<-"raw_data/EMODNET_downloaded_2025/download_parts/"

for (file in all_filenames) {
  
  file_df <- read_emodnet(file)

  
  file_df <-file_df%>%
    mutate(Type= case_when(
      grepl("sediment",file) ~ "sediment",
      grepl("water",file) ~ "water",
      grepl("biota",file) ~ "biota",
      TRUE ~ "other"
    ))%>%
    mutate(Region= case_when(
      grepl("Arctic",file) ~ "Arctic Sea",
      grepl("Atlantic",file) ~ "Atlantic Sea",
      grepl("Baltic",file) ~ "Baltic Sea",
      grepl("BlackSea",file) ~ "Black Sea",
      grepl("Med_",file) ~ "Mediterranean Sea",
      grepl("NorthSea",file) ~ "North Sea",
      
    ))%>%
    mutate(file= file)
  file_name_save<-ifelse(grepl("profile",file),
                         paste0(file_df$Region[1],"_",file_df$Type[1],"_profile"),
                         paste0(file_df$Region[1],"_",file_df$Type[1],"_timeseries"))
  if(!is.na(folder_RDS)){
    saveRDS(file_df, file=paste0(folder_RDS,file_name_save,".rds"))
  }
}

#check if there is no missing data (check the date column), if its missing, 
#its usually because there are "#" in some columns in the data. 
#Go to the data and replace it for "_"
#	station 101 Dublin Bay stn 85 has missing dates


# Load all RDS files and combine them into a single dataframe
rds_files <- list.files(path=folder_RDS, pattern="*.rds", full.names=T)
file_df <- bind_rows(lapply(rds_files, readRDS))%>%distinct() #---2013959


write.table(file_df,file="raw_data/EMODNET.csv",row.names = F,sep=";")

rm(file_df,rds_files,folder_RDS)



#ICES--------
#get it from the web hehe
#https://data.ices.dk/view-map?theme=201831

#last update (date when the data was downloaded): 17.03.2025
#The collection period goes from 2000-2025 (there is still no data for 2025)
