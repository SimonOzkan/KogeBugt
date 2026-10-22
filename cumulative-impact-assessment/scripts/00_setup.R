# Master setup script 
# source("scripts/00_setup.R")


# Load paths configuration
source(here::here("config/paths.R"))
PATHS <- set_project_paths()

target_crs <- 25832
# Install packages hvis de mangler
required_packages <- c("terra","dplyr","sf","ggplot2", "tidyr", "purrr", "readr","readxl","patchwork", "raster","tidyterra",
                       "lubridate", "stringr", "forcats", "scales","gridExtra","grid","lattice","ggpubr","gt","writexl",
                       "ggspatial","httr","ows4R","viridis", "viridisLite","mgcv","gdistance","magick","terrainr")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
# Load packages
lapply(required_packages, library, character.only = TRUE)

grid_sf <- st_read(file.path(PATHS$input_assessment_area, "shp", "250_grid.gpkg")) %>%
  st_transform(., crs = target_crs) %>%
  mutate(area_grid = as.numeric(st_area(.))) 

grid_raster <- terra::rast(file.path(PATHS$input_assessment_area, "geotif","assessment_area_raster.tif"))

assessment_area_dissolved <- grid_sf %>%
  st_union(.)

assessment_area_vect <- terra::vect(assessment_area_dissolved)

### Bagrundskortet
# udsnit med margin (i dit CRS)
# bb <- st_as_sfc(st_bbox(st_buffer(assessment_area_dissolved, 15000)))
# 
# tiles <- maptiles::get_tiles(bb, provider = "Esri.WorldShadedRelief",
#                              crop = TRUE, zoom = 11)
# tiles <- project(tiles, "EPSG:25832")
# 
# writeRaster(tiles, file.path(PATHS$input_assessment_area, "maps","basemap_tiles",
#                              "esri_shaded_relief_25832.tif"), overwrite = TRUE)

#
tiles <- rast(file.path(PATHS$input_assessment_area, "maps","basemap_tiles",
                        "esri_shaded_relief_25832.tif"))


viridis_start_color <- viridis_pal()(1)

color_viridis <- scale_fill_viridis_c(
  name     = NULL,
  limits   = c(0, 1),
  na.value = NA,
  breaks   = c(0, 0.25, 0.5, 0.75, 1.0),
  guide    = guide_colorbar(
    direction       = "vertical",
    barheight       = unit(0.98, "npc"),   
    barwidth        = unit(0.7, "cm"),
    label.position  = "right",
    ticks           = TRUE,
    ticks.linewidth = 0.5,
    frame.colour    = NA
  )
) 
boundary <- coord_sf(
  crs  = 25832,
  xlim = c(696427, 775958),
  ylim = c(6096053, 6179593),
  clip = "off"                         
) 

my_theme <- theme(
  axis.title.x         = element_blank(),
  axis.title.y         = element_blank(),
  axis.text.x          = element_blank(),
  axis.text.y          = element_blank(),
  legend.position = "right",          # <- reservér plads i stedet for npc-overlap
 # legend.position      = c(0.990, 0.5),  
  #legend.justification = c(0, 0.5),     
  legend.background    = element_rect(color = "white"),
  legend.margin        = margin(6, 6, 6, 0),
  legend.box.margin    = margin(0, 0, 0, 0),
  legend.text          = element_text(size = 16),
  legend.text.position = "left",
  axis.ticks           = element_blank(),
  plot.margin = margin(4, 4, 4, 4), 
  #plot.margin          = grid::unit(c(0, 15, 0, 0), units = "mm"),
  axis.ticks.length    = unit(0, "pt")
) 
north_arrow <-  annotation_north_arrow(
  location    = "bl",
  which_north = "true",
  style       = north_arrow_fancy_orienteering(
    fill = c("black","white"),
    text_col = "black"
  ),
  pad_x       = unit(2.5, "cm"),
  pad_y       = unit(0.7, "cm"),
  height      = unit(1.8, "cm"),
  width       = unit(1.8, "cm")
) 

scale_bar <-  annotation_scale(
  location   = "bl",
  width_hint = 0.05,
  height     = unit(0.4, "cm"),
  bar_cols   = c("black", "white"),
  text_col = "black",
  pad_x      = unit(0.2, "cm"),
  pad_y      = unit(1.2, "cm"),
  text_cex   = 1.2
)



fill_na <- function(r, template, w = 3, max_iter = 200) {
  out <- r
  for (i in seq_len(max_iter)) {
    # tæl NA-celler, der ligger INDEN for template-området
    na_tilbage <- global(is.na(out) & !is.na(template), "sum")[1, 1]
    if (na_tilbage == 0) break
    filled <- terra::focal(out, w = w, fun = "mean", na.rm = TRUE)  # nabo-gennemsnit
    out    <- terra::cover(out, filled)                             # behold eksisterende, fyld kun NA
  }
  mask(out, template)   
}



gem_trim <- function(plot, path, dpi = 360, width_cm = 18) {
  # 1) gem RIGELIGT stort i høj opløsning (så resize kun nedskalerer = skarpt)
  ggsave(path, plot, bg = "white", width = 28, height = 28, units = "cm", dpi = dpi)
  
  # 2) beskær hvid kant
  img <- image_trim(image_read(path))
  
  # 3) fastsæt endelig bredde (højden følger aspect) + indlejret dpi
  target_px <- round(width_cm / 2.54 * dpi)          # ønsket bredde i pixels
  img <- image_resize(img, paste0(target_px, "x"))   # "x" tom => bevar formatforhold
  
  image_write(img, path)
  invisible(image_info(img))   # se endelige pixels/dpi
}


cat("Setup complete. Use PATHS object for file paths.\n")
