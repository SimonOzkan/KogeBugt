#library(httr)
#library(jsonlite)

# #get the observation data
# # Define API URL with offset and limit
# offset <- 0  # Change this as needed
# limit <- 100000 # Change this as needed
# url <- paste0("https://esas.ices.dk/api/getPositionRecords/", offset, "/", limit)
# url <- paste0("https://esas.ices.dk/api/getPositionRecords")
# 
# # Make the GET request
# response <- GET(url)
# # Check response status
# if (status_code(response) == 200) {
#   data <- content(response, as = "text", encoding = "UTF-8")
#   data_parsed <- fromJSON(data)
#   print(data_parsed)
# } else {
#   print(paste("Error:", status_code(response)))
# }
# #get the position of the observations for our study area
# position_coord<-data_parsed$results%>%
#   select(campaignID,sampleID,positionID,time,latitude,longitude)%>%
#   distinct()
# position_coord<-position_coord%>%
#   filter(latitude<=59 & latitude >=57)%>%
#   filter(longitude <= 11.6 & longitude >=7)
# rm(data_parsed)
# #get the species
# #the next dataframe is based on the data from ICES (they use these codes)
# df_species<-data.frame(ScientificName= c("Alcidae",
#                                          "Clangula hyemalis",
#                                          "Fulmarus glacialis",
#                                          "Gavia adamsii",
#                                          "Gavia arctica",
#                                          "Gavia immer",
#                                          "Gavia immer / adamsii",
#                                          "Gavia sp.",
#                                          "Gavia stellata",
#                                          "Melanitta nigra",
#                                          "Mergus serrator",
#                                          "Somateria mollissima"),
#                        Code= c(6549,
#                                2120,
#                                220,
#                                50,
#                                30,
#                                40,
#                                45,
#                                59,
#                                20,
#                                2130,
#                                2210,
#                                2060))
# species<-unique(df_species$ScientificName)
# data_species<-list()
# 
# for (sp in species){
#   sp_code<-df_species%>%
#     filter(ScientificName==sp)%>%
#     pull(Code)
#   url <- paste0("https://esas.ices.dk/api/getObservationRecords?SpeciesCode=",sp_code)
#   # Make the GET request
#   response <- GET(url)
#   # Check response status
#   if (status_code(response) == 200) {
#     data <- content(response, as = "text", encoding = "UTF-8")
#     data_parsed <- fromJSON(data)
#     print(data_parsed)
#   } else {
#     print(paste("Error:", status_code(response)))
#   }
#   data_species[[sp]]<-data_parsed$results
# }
# data_species<-bind_rows(data_species)
# 
# #check when the campaings were. Filter suitably.
# # Define API URL with offset and limit
# offset <- 0  # Change this as needed
# limit <- 100000 # Change this as needed
# url <- paste0("https://esas.ices.dk/api/getCampaignRecords/", offset, "/", limit)
# url <- paste0("https://esas.ices.dk/api/getCampaignRecords")
# # Make the GET request
# response <- GET(url)
# # Check response status
# if (status_code(response) == 200) {
#   data <- content(response, as = "text", encoding = "UTF-8")
#   data_parsed <- fromJSON(data)
#   print(data_parsed)
# } else {
#   print(paste("Error:", status_code(response)))
# }
# campaings<-data_parsed$results%>%
#   select(tblCampaignID,startDate,endDate)%>%
#   mutate(startDate=as.Date(startDate,format="%Y-%m-%d"),
#          endDate=as.Date(endDate,format="%Y-%m-%d"))
# 
# 
# #make your dataframe
# df<-left_join(position_coord,data_species)%>%
#   left_join(campaings)%>%
#   filter(!is.na(speciesScientificName))
# #rm(data_parsed,data_species,position_coord)
# gc()
# 
# df_shp<- st_as_sf(df, coords=c("longitude","latitude"),crs=4326)
# df_shp<-st_transform(df_shp,3035)
# st_write(df_shp,"../GIS files/ICES_ESAS_seabirds/ICES_ESAS_seabirds.shp",append=F)


library(mgcv)
library(tidyverse)
library(terra)
library(sf)

#raster to use
#base_raster <- rast("../CEA_Skagerrak/study area/Skagerrak.tif")
base_raster <- rast("../CEA_Skagerrak/study area/Skagerrak_v2_filtered_na.tif")
raster_points <- as.data.frame(base_raster, xy = TRUE, na.rm = TRUE)%>%select(-3)
  
  output_csv_folder<- "../CEA_Skagerrak/Ecosystem components/processed/"
  output_plots_folder<- "../CEA_Skagerrak/Ecosystem components/processed/plots/"
  

#read in the data
  df_shp<- st_read("../GIS files/ICES_ESAS_seabirds/ICES_ESAS_seabirds.shp")
  #df_shp<-df_shp%>%filter(startDt>"2000-01-01")
  df_shp$cell_id <- cellFromXY(base_raster, st_coordinates(df_shp))

#species per raster cell

species_count_per_pixel <- df_shp %>%
  st_drop_geometry() %>%
  mutate(spcsScN=ifelse(grepl("Gavia",spcsScN),"Gavia spp.",spcsScN))%>%
  group_by(cell_id,spcsScN) %>%
  summarise(species_count = sum(count),.groups="drop")%>%
  filter(!is.na(cell_id))

 birds_points_raster<-rasterize(df_shp,base_raster,field="spcsScN",fun="sum")
# birds_points_raster[] <- NA
# birds_points_raster[species_count_per_pixel$cell_id] <- species_count_per_pixel$species_count
# plot(birds_points_raster)
# 
# 
# birds_df <- as.data.frame(birds_points_raster, xy = TRUE, na.rm = TRUE)



#predict abundance

coastline<-st_read("C:/Data/Repositories/WORLD/Europe_coastline_poly.shp")
species_count_per_pixel_all <- species_count_per_pixel%>%
  group_by(cell_id)%>%summarise(sum_birds=sum(species_count),.groups = "drop")%>%
  filter(sum_birds<1000)%>%
  filter(sum_birds>10)
df_shpx<-df_shp%>%select(cell_id)%>%distinct()

species<-left_join(df_shpx,species_count_per_pixel_all)%>%filter(!is.na(sum_birds))
species$distance<-st_distance(species,coastline)%>%apply(1,min)
coords<- st_coordinates(species)
species$x<- coords[,1]
species$y<- coords[,2]
rm(coords,df_shpx)

model <- gam(sum_birds ~ s(x, y) + (distance), data = species, family = quasipoisson)

#need to add the variable distance into the input data, to do this shapefile:
  #open the tif Skagerrak in QGIS, transform it to points(Raster pixels to points)
  # and then calculate distance to coastline using "Shortest line between features".
raster_points_distances<- st_read("../CEA_Skagerrak/study area/raster point distances v3.shp")%>%
  select(distance)
raster_points_distances$geometry<-NULL
grid <- raster_points%>%
  cbind(raster_points_distances)

rm(raster_points_distances)

# Predict seabirds abundance
grid$predicted_count <- predict(model, newdata = grid, type = "response")

ggplot(grid, aes(x = x, y = y, fill = predicted_count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Predicted Seabird Abundance in the Skagerrak")

grid<-grid%>%rename("value"="predicted_count")
#normalize it

gridx<-grid%>%
  mutate(value= (value-min(value))/(max(value)-min(value)))%>%
  select(Seabirds=value)
gridx<-cbind(raster_points,gridx)

# Save output CSV
write.csv(gridx, paste0(output_csv_folder, "ESAS_seabirds_predict.csv"), row.names = FALSE)

# Create raster for plotting
r <- rast(gridx,
          type = "xyz",
          crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
# Save PNG
png(filename = paste0(output_plots_folder, "ESAS_seabirds_predicted.png"), width = 700, height = 500)
plot(r, main = paste0("ESAS seabirds 1km resampled: predicted "))
dev.off()

writeRaster(r, filename=paste0(output_plots_folder,"ESAS_seabirds_predicted.tif"), overwrite=T)


#rasterize it per species
# 
# species_count_per_pixel <- df_shp %>%
#   st_drop_geometry() %>%
#   mutate(spcsScN=ifelse(grepl("Gavia",spcsScN),"Gavia spp.",spcsScN))%>%
#   group_by(cell_id,spcsScN) %>%
#   summarise(sum_birds = sum(count),.groups="drop")%>%
#   filter(sum_birds<1000)%>%
#   filter(sum_birds>10)%>%
#   filter(!is.na(cell_id))
# 
# 
# test<-split(species_count_per_pixel,species_count_per_pixel$spcsScN)
# 
# raster_points_distances<- st_read("../CEA_Skagerrak/study area/raster point distances.shp")%>%
#   select(distance)
# raster_points_distances$geometry<-NULL
# 
# sps<-names(test)
# raster_list<-list()
# plot_list<- list()
# for (name in sps){
#   df <- test[[name]]
#   
#   df_shpx<-df_shp%>%select(cell_id)%>%distinct()%>%
#     filter(!is.na(cell_id))
#   
#   species<-left_join(df_shpx,df)%>%filter(!is.na(sum_birds))
#   species$distance<-st_distance(species,coastline)%>%apply(1,min)
#   coords<- st_coordinates(species)
#   species$x<- coords[,1]
#   species$y<- coords[,2]
#   rm(coords,df_shpx)
#   
#   tryCatch({
#     model <- gam(sum_birds ~ s(x, y) + (distance), data = species, family = quasipoisson)
#   #need to add the variable distance into the input data
#     grid <- raster_points%>%
#       cbind(raster_points_distances)
#     
#     
#     # Predict seabirds abundance
#     grid$predicted_count <- predict(model, newdata = grid, type = "response")
#     
#     p<-ggplot(grid, aes(x = x, y = y, fill = predicted_count)) +
#       geom_tile() +
#       scale_fill_viridis_c() +
#       coord_fixed() +
#       labs(title = paste0("Predicted Seabird Abundance in the Skagerrak - ",name))
#     
#     grid<-grid%>%rename("value"="predicted_count")
#     #normalize it
#     grid<-grid%>%
#       mutate(value= value/max(value))%>%
#       select(x,y,Seabirds=value )
#     
#     r <- rast(grid,
#               type = "xyz",
#               crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     
#     raster_list[name]<-r
#     plot_list[name]<-p
#   }, error= function(e){
#     message(paste("Model failed for",name,":",e$message))
#   })
#    
# }
# 
# plot(raster_list$`Somateria mollissima`)
# 
# super_r<-mosaic(raster_list$`Fulmarus glacialis`,raster_list$`Melanitta nigra`,raster_list$`Somateria mollissima`,fun="sum")
# 
# plot(super_r)
