#---------------------- Marin TN: kernel-interpolation (ArcGIS-ækvivalent) -----------
# Reproducerer ArcGIS 10.3 "Kernel Interpolation with Barriers" i R:
#   Kernel = Gaussian, Power = 1 (lokal lineær), Ridge-stabilisering,
#   Bandwidth = 50 km, valgfri barrierer (cost-distance uden om land),
#   interpolation på log-skala -> tilbagetransformeret til koncentration,
#   + standardfejl-flade som usikkerhed.
#
# Metode = lokal lineær kerneregression (local polynomial / GWR-stil):
#   for hver grid-celle s0: vægt stationer med Gaussian-kernel(afstand/bandwidth),
#   fit en vægtet ridge-regresseret plade value ~ 1 + (x-x0) + (y-y0),
#   forudsig i s0 (= interceptet), og tag prædiktionens SE.
#
# Input: 'samlet' (eller 'dat') med kolonnerne stedID/Station, year, value, x, y
#        - value er den udfyldte (målt + modelleret) stationsværdi.

source("scripts/00_setup.R")
PATHS <- set_project_paths()




## ==================================================================
## 0. Parametre  (ArcGIS-ækvivalenter)
## ==================================================================
bandwidth   <- 10000      # m: kernel-bandbredde (ArcGIS "Bandwidth")
power       <- 1          # 1 = lokal lineær (ArcGIS "Power"); 0 = kerne-vægtet middel
ridge       <- 0.1          # ridge-stabilisering (IKKE Esris 100 - kalibrér, se LOO nederst)
kernel_type <- "gaussian" # "gaussian" | "exponential" | "quartic"
use_barrier <- TRUE      # TRUE = cost-distance uden om land (kræver gdistance)
min_weight  <- 1e-6       # celler hvor alle vægte < dette -> NA (for langt fra data)

# Hvilket data-snit interpoleres? Ét år, eller middel over en periode.
target_year <- NULL       # fx 2024 for ét år; NULL -> middel over 'period_years'
period_years <- 2010:2024 # bruges når target_year er NULL


# log værdi for hver station og beholder koordinator
pts <- read.csv(file.path(PATHS$input_pressure, "MFS_DIN_DIP","GLM_TN.csv"))

pts <- samlet_df_final %>%
  mutate(lvalue = log10(value + 1)) %>%
  group_by(Station, x, y) %>%
  summarise(lvalue = mean(lvalue, na.rm = TRUE), .groups = "drop")


## ==================================================================
## 2. Afstandsmatrix celle -> station  (Euklidisk eller cost/barriere)
## ==================================================================
cell_xy <- terra::xyFromCell(grid_raster, seq_len(terra::ncell(grid_raster)))
st_xy   <- as.matrix(pts[, c("x", "y")])

if (!use_barrier) {
  # Euklidisk: D[j, i] = afstand fra celle j til station i
  D <- outer(cell_xy[, 1], st_xy[, 1], "-")^2 +
    outer(cell_xy[, 2], st_xy[, 2], "-")^2
  D <- sqrt(D)
} else {
  # Vand-maske: 1 inde i området (vand), NA udenfor (land) -> NA = barriere
  water <- terra::rasterize(assessment_area_vect, grid_raster, field = 1, background = NA)
  rl <- raster::raster(water)                     
  
  tr <- gdistance::transition(rl, transitionFunction = function(x) 1, directions = 16)
  tr <- gdistance::geoCorrection(tr, type = "c")    # -> meter
  
  D <- matrix(NA_real_, nrow = nrow(cell_xy), ncol = nrow(st_xy))
  for (i in seq_len(nrow(st_xy))) {
    ac     <- gdistance::accCost(tr, st_xy[i, ])
    D[, i] <- raster::extract(ac, cell_xy)
  }
  D[!is.finite(D)] <- NA                             # celler uden vand-vej -> NA
}


## ==================================================================
## 3. Kerne + lokal lineær ridge-WLS pr. celle
## ==================================================================
kern <- switch(kernel_type,
               gaussian    = function(u) exp(-(u^2)),            # u = d/bandwidth
               exponential = function(u) exp(-u),
               quartic     = function(u) ifelse(u < 1, (1 - u^2)^2, 0),
               stop("ukendt kernel_type"))

n_cell <- nrow(cell_xy)
pred   <- rep(NA_real_, n_cell)   # forudsigelse på log-skala
pse    <- rep(NA_real_, n_cell)   # SE på log-skala
yv     <- pts$lvalue

# penalty: straf ikke interceptet (forudsigelsen), kun hældningerne
P <- if (power == 1) diag(c(0, 1, 1)) else matrix(0, 1, 1)

for (j in seq_len(n_cell)) {
  d <- D[j, ]
  if (anyNA(d)) { ok <- !is.na(d); if (!any(ok)) next } else ok <- rep(TRUE, length(d))
  w <- kern(d[ok] / bandwidth)
  if (max(w, na.rm = TRUE) < min_weight) next        # for langt fra data
  
  if (power == 1) {
    U <- (st_xy[ok, 1] - cell_xy[j, 1]) / bandwidth
    V <- (st_xy[ok, 2] - cell_xy[j, 2]) / bandwidth
    X <- cbind(1, U, V)
  } else {
    X <- matrix(1, nrow = sum(ok), ncol = 1)
  }
  y  <- yv[ok]
  sw <- sqrt(w); Xw <- X * sw
  A  <- crossprod(Xw) + ridge * P
  b  <- crossprod(Xw, y * sw)
  beta <- tryCatch(solve(A, b), error = function(e) NULL)
  if (is.null(beta)) next
  
  pred[j] <- beta[1]                                  # forudsigelse i s0 = intercept
  # lokal varians -> prædiktions-SE (sandwich, ridge-biasen ignoreret)
  res  <- as.numeric(y - X %*% beta)
  sumw <- sum(w); edf <- ncol(X)
  s2   <- sum(w * res^2) / max(sumw - edf, 1)
  M    <- tryCatch(solve(A), error = function(e) NULL)
  if (!is.null(M)) {
    Vbeta  <- s2 * (M %*% crossprod(X * w) %*% M)     # X'W^2X i midten
    pse[j] <- sqrt(max(Vbeta[1, 1], 0))
  }
}


## ==================================================================
## 4. Byg rastre, tilbagetransformér, maskér
## ==================================================================
pred_r <- grid_raster; terra::values(pred_r) <- pred   # log-skala
se_r   <- grid_raster; terra::values(se_r)   <- pse    # log-skala SE

# tilbage til koncentration (µg/l): value = 10^pred - 1
conc_r <- 10^pred_r - 1
# SE på koncentrations-skala via delta-metode: d(10^p)/dp = ln(10)*10^p
conc_se_r<- log(10) * (10^pred_r) * se_r

conc_r   <- terra::mask(conc_r,    assessment_area_vect)
conc_se_r <- terra::mask(conc_se_r, assessment_area_vect)
names(conc_r) <- "TN_ugl"; names(conc_se_r) <- "SE_ugl"

# normaliseret presfaktor-lag 0-1 (efter maskering)
tn_norm <- terra::scale_linear(conc_r); names(tn_norm) <- "value"

terra::writeRaster(conc_r,    file.path(PATHS$output_pressure_tif, "nutrients","total_nitrogen_kernel_ugl.tif"),      overwrite = TRUE)
terra::writeRaster(conc_se_r, file.path(PATHS$output_pressure_tif, "nutrients","total_nitrogen_kernel_se.tif"),       overwrite = TRUE)
terra::writeRaster(tn_norm,   file.path(PATHS$output_pressure_tif, "nutrients","total_nitrogen_kernel_norm.tif"),     overwrite = TRUE)

plot(conc_r)
## ==================================================================
## 5. Bilag-plots (værdi + usikkerhed)
## ==================================================================
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)
pts_sf <- st_as_sf(pts, coords = c("x", "y"), crs = target_crs)


conc_as_sf <- conc_r %>%
  terra::as.polygons(dissolve = FALSE) %>% 
  st_as_sf() %>%
  st_transform(target_crs) %>% 
  st_make_valid() 

conc_norm_as_sf <- tn_norm %>%
  terra::as.polygons(dissolve = FALSE) %>% 
  st_as_sf() %>%
  st_transform(target_crs) %>% 
  st_make_valid() 


maalt_2010_2024mean<- comb_final %>%
  filter(year %in% c(2010:2024)) %>%
  group_by(stedID,x,y) %>%
  summarise(mn_val = mean(value, na.rm =TRUE), .groups = "drop") %>%
  st_as_sf(., coords = c(x= "x", y="y"), crs = target_crs) %>%
  st_transform(target_crs) %>% 
  st_make_valid() 

ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = conc_as_sf, aes(fill = TN_ugl), color = NA) +
  geom_sf(data = maalt_2010_2024mean, aes(fill = mn_val),
          shape = 21, color = "white", size = 3) +
  scale_fill_viridis_c(name = "TN (µg/l)") +      
  labs(title = "Marin total-N (kernel) og målte stationsmidler 2010–2024") +
  theme_minimal() + boundary
  

# normaliseret TN

map_tn_normal <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = tn_norm_as_sf, aes(fill = value), color = NA) +
  #geom_sf(data = pts_sf, color = "red", size = 0.6) +
  color_viridis + boundary +
  theme_minimal() + my_theme + north_arrow + scale_bar
ggsave(plot = map_tn_normal, filename = file.path(PATHS$output_pressure_png,"nutrients" ,"total_nitrogen_kernel_normalized.png"),
       bg = "white", height = 18, width = 18, dpi = 300)

# usikkerhed (SE i µg/l)
se_sf <- to_sf(conc_se_r, "se")
map_se <- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = se_sf, aes(fill = se), color = NA) +
  geom_sf(data = pts_sf, color = "black", size = 0.6) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "SE (µg/l)") +
  boundary +
  labs(title = "Marin total-N - standardfejl (kernel smoothing)") +
  theme_minimal() + my_theme + north_arrow + scale_bar
ggsave(plot = map_se, filename = file.path(PATHS$output_pressure_png, "total_nitrogen_kernel_se.png"),
       bg = "white", height = 18, width = 18, dpi = 300)

# ikke normaliseret TN værdier, er visuelt mere sigende

map_tn<- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = conc_as_sf, aes(fill = TN_ugl), color = NA) +
  #geom_sf(data = pts_sf, color = "red", size = 0.6) +
  scale_fill_viridis_c(name = "TN (µg/l)")+ boundary +
  theme_minimal() + my_theme + north_arrow + scale_bar
ggsave(plot = map_tn, filename = file.path(PATHS$output_pressure_png,"nutrients" ,"total_nitrogen_kernel.png"),
       bg = "white", height = 18, width = 18, dpi = 300)

## ==================================================================
## 6. (valgfrit) LOO-krydsvalidering -> kalibrér bandwidth + ridge
## ==================================================================
# Esris Ridge=100 kan ikke overføres som tal; vælg bandwidth/ridge der
# minimerer LOO-RMSE på log-skala i stedet. Kør over et par kombinationer.

loo_rmse <- function(pts, bandwidth, ridge, power = 1, kernel_type = "gaussian") {
  kern <- switch(kernel_type,
                 gaussian = function(u) exp(-(u^2)),
                 exponential = function(u) exp(-u),
                 quartic = function(u) ifelse(u < 1, (1 - u^2)^2, 0))
  sx <- pts$x; sy <- pts$y; y <- pts$lvalue; n <- nrow(pts)
  Dst <- as.matrix(dist(cbind(sx, sy)))
  P <- if (power == 1) diag(c(0, 1, 1)) else matrix(0, 1, 1)
  err <- rep(NA_real_, n)
  for (k in seq_len(n)) {
    i <- setdiff(seq_len(n), k)
    w <- kern(Dst[k, i] / bandwidth); if (max(w) < 1e-9) next
    X <- if (power == 1) cbind(1, (sx[i]-sx[k])/bandwidth, (sy[i]-sy[k])/bandwidth) else matrix(1, length(i), 1)
    sw <- sqrt(w)
    beta <- tryCatch(solve(crossprod(X*sw) + ridge*P, crossprod(X*sw, y[i]*sw)), error = function(e) NULL)
    if (!is.null(beta)) err[k] <- y[k] - beta[1]
  }
  sqrt(mean(err^2, na.rm = TRUE))
}

# Eksempel - udkommentér og kør for at vælge parametre:
grid_par <- expand.grid(bw = c(10000, 20000, 50000), rg = c(0.1, 1, 10))
grid_par <- expand.grid(bw = c(5000, 10000, 20000, 50000),
                        rg = c(0.1, 1), power = c(0, 1))
grid_par$rmse <- mapply(function(bw, rg) loo_rmse(pts, bw, rg, power, kernel_type),
                        grid_par$bw, grid_par$rg)
print(grid_par[order(grid_par$rmse), ])


## ==================================================================
## NOTER
## ==================================================================
# - Vægtning: Gaussian her er exp(-(d/h)^2) (vægt ~0,37 ved d=bandwidth). Vil du
#   have "mildere" haler, brug exp(-0,5*(d/h)^2) i kern().
# - use_barrier = TRUE kræver gdistance + et vand-raster; for en åben bugt ændrer
#   det typisk lidt. Betyder mest ved vige/landtunger.
# - pred[j] = NA hvor alle vægte < min_weight (ingen data inden for rækkevidde) -
#   de celler bliver hvide; hæv bandwidth hvis for mange.
# - SE-fladen er prædiktionens standardfejl (analog til ArcGIS' "standard error of
#   the results"); den vokser dér hvor stationerne er langt væk / skævt fordelt -
#   altså netop ved de nedlagte kyststrækninger.