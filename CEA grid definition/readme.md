Køge Bugt grid definition
================

Read shape file

``` r
file_shp <- "data/raw data/assessment_area/assessment_area.shp"

shp_area <- sf::st_read(paste0(basefolder, "/", file_shp))
```

    ## Reading layer `assessment_area' from data source 
    ##   `C:\Users\CJM\NIVA\Køge Bugt - General\data\raw data\assessment_area\assessment_area.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 4 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 696427.3 ymin: 6096053 xmax: 775958.2 ymax: 6179593
    ## Projected CRS: ETRS89 / UTM zone 32N

Simple plot of polygons

``` r
ggplot() +
  geom_sf(data = shp_area) + 
  theme_minimal()
```

![](readme_files/figure-gfm/plot-shp-1.png)<!-- -->

Get extent of the polygons

``` r
ext <- sf::st_bbox(shp_area)

ext
```

    ##      xmin      ymin      xmax      ymax 
    ##  696427.3 6096052.7  775958.2 6179593.3

Details for the grid resolution and extent

``` r
# grid resolution for the CEA calculations (m)
grid_resolution <- 250 

# coarser grid resolution to give "nice" rounded numbers for the CEA area extent
grid_coarse <- 1000

# get the rounded values and add an additional buffer
x0 <- grid_coarse * floor( (ext$xmin - grid_coarse)/grid_coarse)
x1 <- grid_coarse * ceiling( (ext$xmax + grid_coarse)/grid_coarse)
y0 <- grid_coarse * floor( (ext$ymin - grid_coarse)/grid_coarse)
y1 <- grid_coarse * ceiling( (ext$ymax + grid_coarse)/grid_coarse)

# replace the values in ext with the rounded values

ext[["xmin"]] <- x0
ext[["xmax"]] <- x1
ext[["ymin"]] <- y0
ext[["ymax"]] <- y1

ext
```

    ##    xmin    ymin    xmax    ymax 
    ##  695000 6095000  777000 6181000

create raster layer with the calculated extent and the required CEA
resolution

``` r
# empty raster with no values
r_assessment_area <- terra::rast(xmin=x0, xmax=x1, ymin=y0, ymax=y1, crs= terra::crs(shp_area), resolution=grid_resolution)

# now rasterize the polygon data
r_assessment_area <- terra::rasterize(shp_area, r_assessment_area, field="id")

names(r_assessment_area) <- "area_id"
```

``` r
# convert to xyz for plotting with geom_raster

cells <- terra::cells(r_assessment_area)
dfxyz <- terra::xyFromCell(r_assessment_area, cells) %>%
  as.data.frame()
dfxyz$area_id <- terra::values(r_assessment_area, na.rm=T)

# area_id names
area_names <- shp_area %>%
  sf::st_drop_geometry() %>%
  distinct(id, area) %>%
  arrange(id) %>%
  pull(area)

dfxyz$area_id <- factor(dfxyz$area_id, 
                        labels=area_names)

# create a polygon from the extent

  xy <- data.frame(x = c(x0, x0, x1, x1, x0), 
                   y = c(y0, y1, y1, y0, y0))

  shp_ext <- xy %>%
    st_as_sf(coords = c("x", "y"), crs = sf::st_crs(shp_area)) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  
  # text for extent
  label_offset <- 1000
  
  df_ext <- data.frame(x = c(x0, x0, x1, x1), 
                      y = c(y0, y1, y1, y0))
  
  
  df_text <- data.frame(x = c(x0+label_offset, x0+label_offset, x1-label_offset, x1-label_offset), 
                      y = c(y0+label_offset, y1-label_offset, y1-label_offset, y0+label_offset),
                      justv = c(0,1,1,0),
                      justh = c(0,0,1,1))
  
  df_text <- df_text %>%
    mutate(coords=paste0(x,", ",y))
  # plot the raster and extent
  
  
  

p <-  ggplot() + 
    geom_raster(data=dfxyz, aes(x=x, y=y, fill=area_id), alpha=0.5) +
    geom_sf(data = shp_ext, fill=NA, colour="red") +
    geom_point(data=df_ext, aes(x=x, y=y), size=2) +
    geom_text(data=df_text, aes(x=x, y=y, 
                              label = coords, hjust = justh, vjust=justv,
                               )) +
    coord_sf() + #expand=F) +
    scale_fill_discrete(name = NULL) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.85,0.75))

p
```

![](readme_files/figure-gfm/plot-raster-1.png)<!-- -->

save the raster as (i) tif and (ii) text file

``` r
# output tif file 

file_grid <- paste0(basefolder, "CEA grid definition/assessment_area.tif")

terra::writeRaster(r_assessment_area, file_grid, overwrite=T)

# output tif file 

file_xyz <- paste0(basefolder, "CEA grid definition/assessment_area.csv")

write.table(dfxyz, file=file_xyz, sep=",", col.names = T, row.names = F)

# save the plot as a png 
ggsave(p, filename= paste0(basefolder, "CEA grid definition/assessment_area.png"), 
       height=16, width=16, units="cm", dpi=300)
```
