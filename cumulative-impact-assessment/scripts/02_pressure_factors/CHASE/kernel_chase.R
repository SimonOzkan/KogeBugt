#### Kernel interpolation af CHASE EQR ###########
source("scripts/00_setup.R")
PATHS <- set_project_paths()

df_CHASE <- read_csv(file.path(PATHS$input_pressure,"kemisk_forurening","CHASE.csv"))
########### Kernel interpolation #####################
pts <- df_CHASE %>%
  filter(!is.na(lon), !is.na(lat), !is.na(EQR_cat)) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(target_crs)
xy <- st_coordinates(pts)
pts <- pts %>% st_drop_geometry() %>%
  mutate(x = xy[,1], y = xy[,2],
         value = EQR_cat) %>%            # <- hvad du interpolerer (se punkt 2)
  dplyr::select(Station = station, x, y, value)

## ==================================================================
## 0. Parametre  (ArcGIS-ækvivalenter)
## ==================================================================
bandwidth   <- 25000      # m: kernel-bandbredde (ArcGIS "Bandwidth") # 25000 er valgt da man dermed får dækket alle celler ud fra stationsværdier og med acceptabel RMSE 
power       <- 1          # 1 = lokal lineær (ArcGIS "Power"); 0 = kerne-vægtet middel
ridge       <- 0.1          # ridge-stabilisering (IKKE Esris 100 - kalibrér, se LOO nederst)
kernel_type <- "gaussian" # "gaussian" | "exponential" | "quartic"
use_barrier <- TRUE      # TRUE = cost-distance uden om land (kræver gdistance)
min_weight  <- 1e-6       # celler hvor alle vægte < dette -> NA (for langt fra data)

# Hvilket data-snit interpoleres? Ét år, eller middel over en periode.
target_year <- NULL       # fx 2024 for ét år; NULL -> middel over 'period_years'
#period_years <- 2014:2024 # bruges når target_year er NULL


###### water_vect er lavet eftersom ICES har nogle punkter der ligger lige udenfor selve assessment grid, men er en del af boundary box
# Disse skal derfor tages med og hjælper til at tildele værdier nede i det syd-østlige hjørne af området

union_extent_area <- function(assessment_area, extent_dir, crs = target_crs) {
  shp_files <- list.files(extent_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
  if (length(shp_files) == 0) stop("Ingen .shp-filer fundet i: ", extent_dir)
  
  # indlæs hver extent -> harmonisér CRS -> valider -> gem geometrien
  geoms <- lapply(shp_files, function(f) {
    st_read(f, quiet = TRUE) %>%
      st_transform(crs) %>%
      st_make_valid() %>%
      st_geometry()
  })
  
  # assessment-området med i samme CRS
  aa <- assessment_area %>% st_transform(crs) %>% st_make_valid() %>% st_geometry()
  
  # saml alt (assessment + alle extents) og union til én flade
  do.call(c, c(list(aa), geoms)) %>%
    st_union() %>%
    st_make_valid()
}


extent_dir <- file.path(PATHS$input_assessment, "CHASE_area_extent")   
water      <- union_extent_area(assessment_area_dissolved, extent_dir)
water_vect <- terra::vect(water)  
## ==================================================================
## 2. Afstandsmatrix celle -> station  (Euklidisk eller cost/barriere)
## ==================================================================
cell_xy <- terra::xyFromCell(grid_raster, seq_len(terra::ncell(grid_raster)))
st_xy   <- as.matrix(pts[, c("x", "y")])

if (!use_barrier) {
  # Euklidisk: D[j, i] = afstand fra celle j til station i
  D <- outer(cell_xy[, 1], st_xy[, 1], "-")^2 + ## outer bruges til at lave en matrix af samtlige afstande mellem gridceller og stationer. ^2 er pythagoras afstand
    outer(cell_xy[, 2], st_xy[, 2], "-")^2
  D <- sqrt(D)       # tilbage til meter afstand fra kvm
} else {
  # Vand-maske: 1 inde i området (vand), NA udenfor (land) -> NA = barriere
  water <- terra::rasterize(water_vect, grid_raster, field = 1, background = NA)
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
               gaussian    = function(u) exp(-(u^2)),            # u = d/bandwidth, sikrer afstands-vægtning
               exponential = function(u) exp(-u),
               quartic     = function(u) ifelse(u < 1, (1 - u^2)^2, 0),
               stop("ukendt kernel_type"))

n_cell <- nrow(cell_xy)
pred   <- rep(NA_real_, n_cell)   # forudsigelse på log-skala
pse    <- rep(NA_real_, n_cell)   # SE på log-skala
yv     <- pts$value

# penalty: straf ikke interceptet (forudsigelsen), kun hældningerne
P <- if (power == 1) diag(c(0, 1, 1)) else matrix(0, 1, 1)

for (j in seq_len(n_cell)) {
  d <- D[j, ]
  if (anyNA(d)) { ok <- !is.na(d); if (!any(ok)) next } else ok <- rep(TRUE, length(d)) #d er afstanden fra celle j til alle stationer. Med barrierer er nogle stationer NA (ingen vand-vej) — ok vælger kun de nåelige. Er ingen nåelige, springes cellen over (bliver NA).
  w <- kern(d[ok] / bandwidth) #Er selv den nærmeste station så langt væk, at alle vægte er ~0, droppes cellen. Det er dét, der laver "huller" i datatomme områder frem for at gætte vildt.
  if (max(w, na.rm = TRUE) < min_weight) next        # for langt fra data
  
  
  
  #Det afgørende trick: koorchaseaterne er centreret på cellen (station minus celle). Så i selve cellen er U=V=0. 
  #tilpasser en hældende plade værdi ≈ β₀ + β₁·U + β₂·V til de vægtede stationer — og fordi den er centreret, bliver β₀ = pladens værdi præcis i cellen.
  #(Med power=0 er X bare en etta-søjle → så er det et rent vægtet gennemsnit, Nadaraya-Watson.)
  if (power == 1) {
    U <- (st_xy[ok, 1] - cell_xy[j, 1]) / bandwidth
    V <- (st_xy[ok, 2] - cell_xy[j, 2]) / bandwidth
    X <- cbind(1, U, V)
  } else {
    X <- matrix(1, nrow = sum(ok), ncol = 1)
  }
  
  
  
  #Standard vægtet mindste-kvadrat: sw = √w vægter rækkerne.
  #A er de penaliserede normalligninger. Ridge-leddet ridge·P lægger en smule til diagonalen for hældningerne (ikke interceptet) — det stabiliserer solve,
  #når stationerne nær cellen er få eller ligger på en linje (så pladen ellers ville vippe vildt), og trækker hældningerne mod 0, dvs. mod en lokal konstant.
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
## ==================================================================
## 4. Byg raster, klem til [0,1], maskér  (EQR interpoleres direkte - ingen tilbagetransform)
## ==================================================================
pred_r_chase <- grid_raster
terra::values(pred_r_chase) <- pred                       # pred er i celle-rækkefølge (xyFromCell)
pred_r_chase <- terra::clamp(pred_r_chase, 0, 1, values = TRUE)   # power=1 kan overshoote
pred_r_chase <- terra::mask(pred_r_chase, grid_raster)    # klip til assessment-området
names(pred_r_chase) <- "value"
## usikkerhedsraster (valgfrit, samme behandling)
pse_r_chase <- grid_raster
terra::values(pse_r_chase) <- pse
pse_r_chase <- terra::mask(pse_r_chase, grid_raster)

plot(pred_r_chase)
plot(pse_r_chase)   # datatæthed/usikkerhed ved siden af



terra::writeRaster(pred_r_chase,    file.path(PATHS$output_pressure_tif, "kemisk_forurening","CHASE.tif"),      overwrite = TRUE)

# Vinter chase
## ==================================================================
## 5. Bilag-plots (værdi + usikkerhed)
## ==================================================================

pts_sf <- pts %>%
  distinct(Station, x,y) %>%
  st_as_sf(., coords = c("x", "y"), crs = target_crs, remove = FALSE)


conc_as_sf_chase <- pred_r_chase %>%
  terra::as.polygons(dissolve = FALSE) %>% 
  st_as_sf() %>%
  st_transform(target_crs) %>% 
  st_make_valid() 


chase_points_sf <- df_CHASE %>%
  st_as_sf(., coords = c(x= "lon", y="lat"), crs = target_crs) %>%
  st_transform(target_crs) %>% 
  st_make_valid() %>%
  rename("value" = "EQR_cat")

kernel_stat_chase <- ggplot() +
  geom_spatraster_rgb(data = tiles, maxcell = 5e6) + #baggrund: relief
  geom_sf(data = conc_as_sf_chase, aes(fill = value), color = NA) +
  geom_point(data = pts, aes(x, y, fill = value),              # stationer som cirkler
             shape = 21, size = 3, stroke = 0.3, colour = "grey20") +
  scale_fill_viridis_c(limits = c(0, 1), name = "EQR", na.value = NA) +  # SAMME skala for begge
  labs(title = "Marin chase (kernel) og modellerede og målte 2014–2024") +
  theme_minimal() + boundary

kernel_stat_chase

gem_trim(kernel_stat_chase,
         file.path(PATHS$output_pressure_png,"kemisk_forurening" ,"kernel_stat_model_chase.png"),
         dpi = 360, width_cm = 18)


# usikkerhed (SE i µg/l)
# se_sf <- to_sf(conc_se_r, "se")
# map_se <- ggplot() +
#   geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
#   geom_sf(data = se_sf, aes(fill = se), color = NA) +
#   geom_sf(data = pts_sf, color = "black", size = 0.6) +
#   scale_fill_viridis_c(option = "magma", direction = -1, name = "SE (µg/l)") +
#   boundary +
#   labs(title = "Marin total-N - standardfejl (kernel smoothing)") +
#   theme_minimal() + my_theme + north_arrow + scale_bar
# ggsave(plot = map_se, filename = file.path(PATHS$output_pressure_png, "total_nitrogen_kernel_se.png"),
#        bg = "white", height = 18, width = 18, dpi = 300)



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

############# Viser hvilke stationer der er brugt til interpolation og hvor lang ud i gridded

## --- vægte pr. celle x station (samme kerne som interpolationen) ---
W <- kern(D / bandwidth)          # D = celle x station (cost-afstande m. barriere)
W[!is.finite(D)] <- 0             # ikke nåelige (land) -> 0
W[W < min_weight] <- 0

## 1) EFFEKTIVT ANTAL bidragende stationer pr. celle (Kish n_eff) -- det bedste "overlap"-mål
sumw  <- rowSums(W,   na.rm = TRUE)
sumw2 <- rowSums(W^2, na.rm = TRUE)
n_eff <- ifelse(sumw2 > 0, sumw^2 / sumw2, NA)

## 2) SIMPELT antal stationer inden for en meningsfuld radius (det du beskriver med ringene)
R     <- bandwidth * sqrt(log(1/0.1))     # radius hvor gaussisk vægt = 0,1  (~91 km ved h=60 km)
count <- rowSums(D <= R, na.rm = TRUE)     # NA (land/ikke nåelig) tæller ikke med

## --- byg rastre ---
neff_r  <- grid_raster; values(neff_r)  <- n_eff
count_r <- grid_raster; values(count_r) <- count
neff_r  <- mask(neff_r,  grid_raster)
count_r <- mask(count_r, grid_raster)

## --- plot: overlap-raster + stationer + ringe ---
stationer <- st_as_sf(pts, coords = c("x","y"), crs = target_crs)
ringe     <- st_buffer(stationer, R)       # euklidiske ringe (vejledende)

ggplot() +
  geom_spatraster(data = count_r) +
  geom_sf(data = ringe,     fill = NA, color = "white", linewidth = 0.25) +
  geom_sf(data = stationer, color = "red", size = 1) +
  scale_fill_viridis_c(na.value = "transparent",
                       name = "Antal stationer\ninden for ~38 km") +
  labs(title = "Datadækning: hvor mange stationer bidrager til hver celle") +
  theme_minimal()




