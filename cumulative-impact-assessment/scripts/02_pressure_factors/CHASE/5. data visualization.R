library(sf)
library(lubridate)
library(tidyverse)
library(patchwork)
library(ggspatial)
library(extrafont)
library(extrafontdb)

load("CHASE/data_assessment_for_visualization.RData")

source("CHASE/R scripts/processing.R")
source("CHASE/R scripts/utilities.R")

output_folder<- "CHASE/Figures/"



# Maps - By category----

  df<-df_CHASE_category%>%mutate(GRIDCODE=tolower(GRIDCODE))
  df_integrated<-df_CHASE_integrated%>%mutate(GRIDCODE=tolower(GRIDCODE))
  
  #st_write(df_integrated,"C:/Data/Repositories/AquaSYNC/GIS files/CHASE_2024/CHASE_2024.shp",append=F)
  #st_write(df_integrated,"C:/Data/Repositories/CHASE/ETC_CHASE_2024/march 2025/shapefiles/CHASE_integrated.shp",append=F)
  
  int_map<-grid_map_general(df,df_integrated,palette=color_palette1)
  
  p_a<-int_map[["Water"]]
  p_a<-p_a+
    theme(legend.position="bottom",
          legend.box.background = element_rect(colour = "black")
    )
  p_b<-int_map[["Sediment"]]
  p_b<-p_b+
    theme(legend.position="bottom",
          legend.box.background = element_rect(colour = "black")
    )
  p_c<-int_map[["Biota"]]
  p_c<-p_c+
    theme(legend.position="bottom",
          legend.box.background = element_rect(colour = "black")
    )
  p_d<-int_map[["Bioeffects"]]
  p_d<-p_d+
    theme(legend.position="bottom",
          legend.box.background = element_rect(colour = "black")
    )
  ggsave(filename = paste0(output_folder,"map_water.png"), plot = p_a, width = 20, height = 20, dpi=300)
  ggsave(filename = paste0(output_folder,"map_sed.png"), plot = p_b, width = 20, height = 20, dpi=300)
  ggsave(filename = paste0(output_folder,"map_biota.png"), plot = p_c, width = 20, height = 20, dpi=300)
  ggsave(filename = paste0(output_folder,"map_bieff.png"), plot = p_d, width = 20, height = 20, dpi=300)
  
  p<-(p_a+p_b)/(p_c+p_d)+
    plot_layout(tag_level = "new",guides = "collect",nrow=2)+
    plot_annotation(tag_levels = "A",title="") &
    theme(legend.position="bottom",
          legend.box.background = element_rect(colour = "black")
    )
  ggsave(filename = paste0(output_folder,"map_categories.png"), plot = p, width = 20, height = 20, dpi=300)
  
# Maps - Integrated----

  p_int<-int_map[["Integrated"]]
  p_int<-p_int+
    theme(legend.position="bottom",
          legend.box.background = element_rect(colour = "black")
    )
  ggsave(filename = paste0(output_folder,"map_integrated.png"), plot = p_int, width = 20, height = 20, dpi=300)

  
# Maps - By Substance group-----
  substance_groups<-unique(df_CR$SubstGroup)
  all_maps<-list()
  list_bars1<-list()
  list_bars2<-list()
  for (subst_group in substance_groups) {
    
    df_subst<-df_CR%>%
      filter(SubstGroup==subst_group)
    
    df_cat_subst<-df_subst%>%
      mutate(GridID=tolower(GridID))%>%
      group_by(GridID, Category) %>%
      mutate(n_subs=n()) %>%
      arrange(desc(CR)) %>%
      slice(1) %>%
      ungroup() %>%
      rename(WorstSubst=Substance) 
    
    df_CHASE_subst <- df_cat_subst %>% #keep to plot it afterwards
      group_by(GridID) %>%
      arrange(EQR_cat) %>%
      slice(1) %>%
      ungroup()%>%
      rename(WorstCat=Category)
    
    df_cat_subst<-df_cat_subst%>%
      mutate(Status= case_when(
        EQR_cat > 0.8 ~ "High",
        EQR_cat > 0.6 & EQR_cat <= 0.8 ~ "Good",
        EQR_cat > 0.4 & EQR_cat <= 0.6 ~ "Moderate",
        EQR_cat > 0.2 & EQR_cat <= 0.4 ~ "Poor",
        EQR_cat <= 0.2 ~ "Bad",
        is.na(EQR_cat) ~"No Data",
      ))
    
    df_CHASE_subst<-df_CHASE_subst%>%
      mutate(Status= case_when(
        EQR_cat > 0.8 ~ "High",
        EQR_cat > 0.6 & EQR_cat <= 0.8 ~ "Good",
        EQR_cat > 0.4 & EQR_cat <= 0.6 ~ "Moderate",
        EQR_cat > 0.2 & EQR_cat <= 0.4 ~ "Poor",
        EQR_cat <= 0.2 ~ "Bad",
        is.na(EQR_cat) ~"No Data",
      ))
    tst<-df_CHASE_subst%>%group_by(WorstCat,WorstSubst)%>%arrange(desc(CR))%>%group_by(WorstCat)%>%slice(1)
    tst<-tst%>%select(SubstGroup,WorstSubst,WorstCat)
    print(tst)
    
    
    cat("The number of gridcells assessed for",subst_group,":",nrow(df_CHASE_subst),"assessment units.\n")
    
    #now we will make some summaries to understand the data, but first we need the regions
    
    #merge with the EEA grid
    merged_data_subst <- left_join(grd,df_cat_subst, by=c("GRIDCODE"="GridID"))%>%mutate(Status= ifelse(is.na(Status),"No Data",Status))
    
    df_CHASE_subst_integrated<-left_join(grd,df_CHASE_subst, by=c("GRIDCODE"="GridID"))%>%mutate(Status= ifelse(is.na(Status),"No Data",Status))
    
    maps<-grid_map(df=merged_data_subst,df_integrated = df_CHASE_subst_integrated,palette = color_palette1,subtitle="(2013-2023)")
    
    for (map_name in names(maps)) {
      all_maps[[paste0(map_name)]] <- maps[[map_name]]
    }
    
    # Save each plot
    for (map_name in names(maps)) {
      ggsave(filename = paste0(output_folder,"annex/",map_name, ".png"), plot = maps[[map_name]], width = 20, height = 20, dpi=300)
    }
  }
    
  
# Barplots - Integrated----
  
  df<-merged_data%>%mutate(GRIDCODE=tolower(GRIDCODE))%>%
    mutate(area_km2=1e-6*as.numeric(sf::st_area(.)))%>%filter(Region!="Macaronesia")
  df_integrated<-df_CHASE_integrated%>%mutate(GRIDCODE=tolower(GRIDCODE))%>%
    mutate(area_km2=1e-6*as.numeric(sf::st_area(.)))%>%filter(Region!="Macaronesia")
  
  reg<- c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean","Greater North Sea")
  
  bar_plots<-bar_map1_general(df,df_integrated,reg,palette = color_palette1)
  
  ggsave(filename = paste0(output_folder,"barplot_integrated.png"), plot = bar_plots, width = 10, height = 10, dpi=300)
  

# Data table behind the barplot----
  
  categories<-c("Water","Sediment","Biota","Bioeffects","Integrated")
  reg<- c("Baltic Sea", "Black Sea", "Mediterranean Sea", "North-east Atlantic Ocean","Greater North Sea")
  
  integrated_summaries <- df_integrated %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,WorstCat,Status)%>%
    group_by(Region, Status) %>%
    summarise(count = n(),area=sum(Shape_Area),.groups="drop") 
  integrated_summaries$geometry<-NULL
  
  integrated_summaries<-integrated_summaries%>%
    group_by(Region)%>%
    mutate(tot_area=sum(area))%>%
    ungroup()%>%
    mutate(pct= scales::percent((area/tot_area), accuracy=0.01))
  
  integrated_summaries2<-integrated_summaries%>%
    mutate(state=ifelse(Status %in% c("High","Good"),"Non-P","P"))%>%
    group_by(state)%>%
    summarise(area=sum(area))%>%
    ungroup()%>%
    mutate(tot_area=sum(area))%>%
    ungroup()%>%
    mutate(pct= scales::percent((area/tot_area), accuracy=0.1))
  
  integrated_summaries_wide<-integrated_summaries%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= "Integrated")%>%
    select(-tot_area,-area)%>%
    pivot_wider(names_from = Region,values_from = c(count,pct))%>%
    arrange(Status)
  
  
  category_summaries<-df %>%
    filter(Status !="No Data")%>%
    arrange(GRIDCODE,Category,Status)%>%
    group_by(Region, Category,Status) %>%
    summarise(count = n(),area=sum(Shape_Area),.groups="drop")
  
  category_summaries$geometry<-NULL
  
  category_summaries<-category_summaries%>%
    group_by(Region,Category)%>%
    mutate(tot_area=sum(area))%>%
    ungroup()%>%
    mutate(pct= scales::percent((area/tot_area), accuracy=0.1))
  
  category_summaries_wide<-category_summaries%>%
    select(-tot_area,-area)%>%
    pivot_wider(names_from = Region,values_from = c(count,pct))%>%
    mutate(Status= factor(Status, levels= correct_order),
           Category= factor(Category, levels= categories))%>%
    arrange(Category,Status)%>%
    filter(Status!="No Data")
  
  all_summaries <- bind_rows(category_summaries_wide, integrated_summaries_wide)
  
  all_summaries <- all_summaries%>%
    select(Category, Status,
           Baltic_n = `count_Baltic Sea`,Baltic_pct= `pct_Baltic Sea`,
           Black_n = `count_Black Sea`, Black_pct = `pct_Black Sea`,
           Medit_n = `count_Mediterranean Sea`, Medit_pct = `pct_Mediterranean Sea`,
           GreatN_n = `count_Greater North Sea`, GreatN_pct= `pct_Greater North Sea`,
           Atlantic_n= `count_North-east Atlantic Ocean`,Atlantic_pct= `pct_North-east Atlantic Ocean`)

  table_sum<-integrated_summaries%>%mutate(Total=sum(count,na.rm=T),
                                           Total_area=sum(tot_area,na.rm=T),
                                           State= ifelse(Status %in% c("High","Good"),"No problematic","Problematic"))
  total<-unique(table_sum$Total[!is.na(table_sum$Total)])
  total_area<-unique(table_sum$Total_area[!is.na(table_sum$Total_area)])
  table_sum2<-table_sum%>%
    select(State,count)%>%
    group_by(State)%>%
    summarise(pct=(sum(count,na.rm=T)/total)*100)%>%distinct()
  
  table_sum3<-table_sum%>%
    select(State,tot_area)%>%
    group_by(State)%>%
    summarise(pct=(sum(tot_area,na.rm=T)/total_area)*100)%>%distinct()
  #add it to the excel "barplot_table" in the annex folder so it will calculate the totals

  
