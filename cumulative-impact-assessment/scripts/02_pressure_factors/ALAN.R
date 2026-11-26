############ Kunstigt lys (ALAN) #############
# Datalag leveret af Paula. Normaliseres i forhold til hele Østersøen (HELCOM).

source("scripts/00_setup.R")
PATHS <- set_project_paths()


## 1. Indlæs ------------------------------------------------------------------

alan <- terra::rast("C:/Users/SIO/Downloads/WCS1891926553409669856 1.nc")   # lon/lat - CRS er korrekt


helcom_v <- terra::vect(file.path(PATHS$input_assessment_area, "HELCOM",
                                  "_ags_HELCOM_1x1km_Baltic_Sea_grid",
                                  "HELCOM_1x1km_Baltic_Sea_grid.shp"))


## 2. Beskær ALAN til Østersøen og saml månederne ----------------------------

# Østersøens bbox omregnet til lon/lat (kun extent, ikke hele griddet)
helcom_ext_ll <- terra::project(terra::ext(helcom_v),
                                from = terra::crs(helcom_v), to = "EPSG:4326")


alan_crop <- terra::crop(alan, helcom_ext_ll)
terra::global(alan_crop[[1]], quantile, probs = c(0.99, 0.999, 1), na.rm = TRUE)
terra::NAflag(alan_crop)   # hvilken værdi filen bruger som NA


alan_fixed <- alan %>%
  terra::crop(helcom_ext_ll) %>%
  terra::classify(cbind(500, Inf, NA))  # fill-værdier / umulige dybder -> NA
terra::global(alan_fixed, median, na.rm = TRUE)


graenser <- c(30, 40, 60, 100, 200)

sapply(graenser, \(x) terra::global(alan_fixed > x, "sum", na.rm = TRUE)[, 1]) %>%
  `colnames<-`(paste0(">", graenser, "m")) %>%
  `rownames<-`(month.abb)

# Hvor ligger de høje værdier i februar?
plot(alan_fixed[[2]] > 40)




alan_med <- alan_fixed %>%
  terra::app(median, na.rm = TRUE)



## 3. Østersø-maske (1 km) og ALAN på samme grid ------------------------------

helcom_r <- terra::rasterize(helcom_v,
                             terra::rast(helcom_v, resolution = 1000),
                             field = 1)

alan_baltic <- alan_med %>%
  terra::project(helcom_r, method = "bilinear") %>%
  terra::mask(helcom_r)



## 4. Normaliseringsreference for Østersøen -----------------------------------

ref_alan <- terra::global(alan_baltic, "max", na.rm = TRUE)[1, 1]
ref_alan_lg <- log10(ref_alan + 1)


## 5. Over på projektgriddet og normalisér ------------------------------------

alan_koge <- alan_mn %>%
  terra::project(grid_raster, method = "bilinear") %>%
  terra::mask(grid_raster)

alan_norm <- alan_koge / ref_alan
alan_log_norm <- log10(alan_koge+1) / ref_alan_lg
par(mfrow = c(1,1))

plot(alan_norm)
plot(alan_log_norm)


ALAN_lg_fill <- fill_na(alan_log_norm, grid_raster, w = 3)

plot(ALAN_lg_fill)


terra::writeRaster(
  ALAN_lg_fill,
  filename  = file.path(PATHS$output_pressure_tif, "fysisk_forstyrrelse","ALAN_lognorm.tif"),
  overwrite = TRUE
)


# lav til sf objekt for plotting
ALAN_sf <- ALAN_lg_fill %>%
  terra::as.polygons(dissolve = FALSE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename("value" = "median")


############### Plotting for bilag ################


map_ALAN<- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = ALAN_sf,
          aes(fill = value), color = NA) +
  color_viridis+
  boundary+
  theme_minimal()+
  my_theme+
  north_arrow+
  scale_bar


map_ALAN

gem_trim(map_ALAN,
         file.path(PATHS$output_pressure_png, "fysisk_forstyrrelse","ALAN.png"),
         dpi = 360, width_cm = 18)



