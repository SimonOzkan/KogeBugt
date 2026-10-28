#====================== DIP: data-prep + GLM (step-by-step) ==========================
# DIP = (NO2+NO3)-N + NH4-N, som N (µg N/l).
#   ODA: komponenter i µg N/l (faktor 1), KUN Prøvefraktion = filtrat, Enkeltprøve.
#   ICES: DIP i µmol/l -> * 14.00672 -> µg N/l.
# Metode antages sammenlignelig (se forbehold) - derfor ingen metode-kovariat.
# Kør chunk for chunk og tjek undervejs.

source("scripts/00_setup.R")
PATHS <- set_project_paths()
library("car")


## ------------------------------------------------------------------
## 0. Parametre
## ------------------------------------------------------------------
#frak            <- "Opløst - filtrat fra filtrering"   # DIP = opløst uorganisk
provetype       <- "Enkeltprøve"
overflade_m     <- 10
out_label       <- "DIP"
ices_value_faktor <- 30.973762  # PO4-P µmol/l -> µg P/l
dup_threshold_m <- 200
min_obs_station <- 2
min_months_year <- 2


## ------------------------------------------------------------------
## 1. Indlæs data
## ------------------------------------------------------------------
DIP_ices <- read_csv2(file.path(PATHS$input_pressure, "MFS_DIN_DIP", "DIP_ices.csv")) 

oda <- read_csv2(file.path(PATHS$input_pressure, "MFS_DIN_DIP",
                           "HentData_Hav_Vandkemi_20261006-110319.csv"),
                 locale = locale(encoding = "latin1")) %>%
  mutate(date_correct = ymd(Startdato),
         year = year(date_correct), month = month(date_correct))


## ------------------------------------------------------------------
## 2. ODA: byg DIP pr. prøve (station + dato + dybde)
## ------------------------------------------------------------------
unique(oda$Parameter)
# DIP 
# Tjekket prøvefraktion og det viser sig at "Total" varierer 25% fra "opløst", derfor er den ikke med foroven,
# Ej oplyst varierer med -2% og kan derfor antages at være tilsvarende opløst
# 
# Orthophosphat er ikke medtaget, da der er meget få Enkeltprøve, hertil er de ikke i vintermånederne
oda_DIP<- oda %>%
  filter(Parameter %in% c("Ortho-phosphat-P"),  #
         GennemsnitsDybde_m <= overflade_m, Prøvefraktion %in% c("Opløst - filtrat fra filtrering", "Ej oplyst"),
         !Resultat > 400) # To ekstreme værdier målt i maj og juni fjernes. Vi kigger alligevel på vinter DIP


# d <- oda_DIP %>%
#   filter(Resultat > 0, !is.na(Resultat)) %>%
#   mutate(value   = as.numeric(Resultat),
#          year    = year(date_correct),
#          month   = month(date_correct),
#          Station = factor(ObservationsStedNr),
#          Prøvefraktion = relevel(factor(Prøvefraktion),
#                                  ref = "Opløst - filtrat fra filtrering"))  # filtrat = reference
# 
# # 1) konfundering med tid
# table(d$Prøvefraktion, d$year)
# 
# # 2) justeret effekt (log-link, kontrollér for station + år + sæson)
# m <- bam(value ~ Prøvefraktion + s(year, k = 10) + s(month, bs = "cc", k = 6) +
#            s(Station, bs = "re"),
#          family = Gamma(link = "log"), data = d, method = "fREML", discrete = TRUE)
# 
# co <- summary(m)$p.table
# data.frame(term = rownames(co),
#            faktor = exp(co[, 1]), pct = (exp(co[, 1]) - 1) * 100, p = co[, 4])
# 



oda_df <- oda_DIP %>%
  transmute(stedID = as.character(ObservationsStedNr),
            Parameter = out_label, source = "ODA",
            date  = as.Date(date_correct),
            year  = as.integer(year(date_correct)), month = as.integer(month(date_correct)),
            depth = as.numeric(GennemsnitsDybde_m),
            value = Resultat, metode = provetype,
            x = as.numeric(X_UTM32), y = as.numeric(Y_UTM32)) 


## ------------------------------------------------------------------
## 3. ICES: DIP -> µg N/l, til UTM, klip til området
## ------------------------------------------------------------------
ices_df <- DIP_ices %>%
  transmute(stedID = as.character(ClusterID),
            Parameter = Parameter, source = "ICES",
            date  = make_date(Year, Month, Day),
            year  = as.integer(Year), month = as.integer(Month),
            depth = as.numeric(Depth),
            value = Value * ices_value_faktor,
            metode = "ICES",
            Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(target_crs) %>%
  mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
  st_intersection(assessment_area_dissolved) %>%
  st_drop_geometry() %>%
  dplyr::select(stedID, Parameter, source, date, year, month, depth, value, metode, x, y)


## ------------------------------------------------------------------
## 4. ICES-dubletter -> omdøb til nærmeste ODA-station
## ------------------------------------------------------------------
oda_sf  <- st_as_sf(oda_df,  coords = c("x", "y"), crs = target_crs, remove = FALSE)
ices_sf <- st_as_sf(ices_df, coords = c("x", "y"), crs = target_crs, remove = FALSE)
oda_stn  <- oda_sf  %>% distinct(stedID, .keep_all = TRUE)
ices_stn <- ices_sf %>% distinct(stedID, .keep_all = TRUE)

nn <- st_nearest_feature(ices_stn, oda_stn)
ices_stn$stedID_oda <- oda_stn$stedID[nn]
ices_stn$dist_oda   <- as.numeric(st_distance(ices_stn, oda_stn[nn, ], by_element = TRUE))

dup_map <- ices_stn %>% st_drop_geometry() %>%
  filter(dist_oda < dup_threshold_m) %>%
  dplyr::select(stedID, stedID_ny = stedID_oda)

omdoeb_dup <- function(df) df %>%
  left_join(dup_map, by = "stedID") %>%
  mutate(stedID = coalesce(stedID_ny, stedID)) %>%
  dplyr::select(-stedID_ny)

stn_pos <- bind_rows(
  oda_stn  %>% st_drop_geometry() %>% dplyr::select(stedID, x, y),
  ices_stn %>% st_drop_geometry() %>% dplyr::select(stedID, x, y) %>% omdoeb_dup()
) %>% group_by(stedID) %>%
  summarise(x = mean(x), y = mean(y), n_stationer = n(), .groups = "drop")


## ------------------------------------------------------------------
## 5. Kombinér + dækningsfiltre
## ------------------------------------------------------------------
df_comb <- bind_rows(oda_df, ices_df) %>%
  omdoeb_dup() %>% dplyr::select(-c(x, y)) %>%
  group_by(stedID, Parameter, year, month) %>% 
  summarise(value = mean(value, na.rm = TRUE),
            source = paste(sort(unique(source)), collapse = "+"), .groups = "drop") %>%
  left_join(stn_pos, by = "stedID")

df_comb_filter <- df_comb %>%
  group_by(stedID) %>% summarise(n_pr_stat = n(), .groups = "drop") %>%
  left_join(df_comb, by = "stedID") %>%
  filter(n_pr_stat >= min_obs_station)

# Fjerner stationsmålinger hvor der kun er målt en måned
comb_final <- df_comb_filter %>%
  group_by(stedID, year) %>% 
  summarise(n_pr_yr = n(), .groups = "drop") %>%
  left_join(df_comb_filter, by = c("stedID", "year")) %>%
  filter(n_pr_yr >= min_months_year) 

## ==================================================================
## 6. GLM (log10) - samme opsætning som TotN
## ==================================================================
dat <- comb_final %>% mutate(log_value = log10(value + 1))

hist(dat$value,     xlab = "DIP (µg N/l)",   col = "green", border = "black", main = "Rå DIP")
hist(dat$log_value, xlab = "log10(DIP+1)",   col = "green", border = "black", main = "Log-transformeret")

glm_df <- dat %>%
  filter(year != 2005) %>%# 2005 har for få målinger og alle centreret omkring KBH Nord, derfor udelades dette år
  mutate(Station = factor(stedID),
         yearf = factor(year),
         monthf = factor(month))

# Additiv model på log10-skala: station + år + sæson
m_glm <- lm(log10(value + 1) ~ Station + yearf + monthf, data = glm_df)
plot(m_glm)                                   # tjek Residuals vs Fitted (heteroskedasticitet?)

vc  <- vcovHC(m_glm, type = "HC3")            # robuste SE (heteroskedasticitet)
sig <- sigma(m_glm)                           # residual-SD til bias-korrektion
print(car::Anova(m_glm, type = 2, vcov. = vc))


## ------------------------------------------------------------------
## 7. Stationsoversigt + målte årsværdier 2010-2024
## ------------------------------------------------------------------
stn_info <- glm_df %>%
  group_by(Station = as.character(Station)) %>%
  summarise(n_maalinger = n(), n_aar = n_distinct(year),
            foerste = min(year), sidste = max(year), .groups = "drop")

aar <- 2010:2024
maalt <- glm_df %>%
  filter(year %in% aar, month %in% c(12, 1, 2)) %>%
  group_by(Station = as.character(Station), year) %>%
  summarise(value_maalt = mean(value), n_maaneder = n_distinct(month), .groups = "drop")

## ------------------------------------------------------------------
## 8. Modelbaserede aritmetiske årsgennemsnit (bias-korrigeret)
## ------------------------------------------------------------------
rg <- emmeans(m_glm, ~ Station + yearf + monthf,
              at = list(yearf = as.character(aar), monthf = c("12","1","2")), vcov. = vc) #Vintermåneder

rg_resp <- regrid(rg, transform = "response", bias.adjust = TRUE, sigma = sig)

em_arit <- emmeans(rg_resp, ~ Station + yearf)

model_df <- as.data.frame(em_arit) %>%
  transmute(Station = as.character(Station),
            year = as.numeric(as.character(yearf)),
            value_model = response, SE, lower.CL, upper.CL)


## ------------------------------------------------------------------
## 9. Samlet tabel: alle stationer × år, udfyldt hvor målinger mangler
## ------------------------------------------------------------------
samlet_df <- expand_grid(Station = levels(glm_df$Station), year = aar) %>%
  left_join(maalt,    by = c("Station", "year")) %>%
  left_join(model_df, by = c("Station", "year")) %>%
  left_join(stn_info, by = "Station") %>%
  mutate(kilde = if_else(is.na(value_maalt), "modelleret", "målt"),
         value = coalesce(value_maalt, value_model),       # Vælger målt værdi for en station hvis tilgængelig, ellers value_model
         aar_udenfor_periode = pmax(foerste - year, year - sidste, 0)) %>%
  arrange(Station, year)

samlet_df_final_DIP <- samlet_df %>%
  left_join(stn_pos %>% mutate(stedID = as.character(stedID)) %>% dplyr::select(stedID, x, y),
            by = c("Station" = "stedID"))

write.csv(samlet_df_final_DIP,file.path(PATHS$input_pressure,"MFS_DIN_DIP","GLM_DIP.csv"), row.names = FALSE) 

# -> samlet_df_final går videre til kernel-interpolationen (som for TotN)


sammenlign <- samlet_df_final_DIP %>%
  filter(!is.na(value_maalt), !is.na(value_model)) %>%
  mutate(diff = value_model - value_maalt,
         pct  = diff / value_maalt * 100)

# nøgletal
sammenlign %>% summarise(
  n        = n(),
  bias     = mean(diff),                               # systematisk over/under
  rmse     = sqrt(mean(diff^2)),
  rmse_pct = sqrt(mean(diff^2)) / mean(value_maalt) * 100,
  r2       = cor(value_maalt, value_model)^2
)

sammenlign %>% summarise(
  n = n(), bias = mean(value_model - value_maalt),
  rmse = sqrt(mean((value_model - value_maalt)^2)),
  r2 = cor(value_maalt, value_model)^2)

# målt vs. modelleret med 1:1-linje
ggplot(sammenlign, aes(value_maalt, value_model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.5, color = "#0072B2") +
  coord_equal() +
  labs(x = "Målt vinter-DIP (µg N/l)", y = "Modelleret vinter-DIP (µg N/l)",
       title = "Model vs. måling (hvor begge findes)") +
  theme_minimal()

## ==================================================================
## FORBEHOLD (skriv i metoden)
## ==================================================================
# - AnalyseMetode antages sammenlignelig. Diagnostikken viste dog signifikante
#  

## ==================================================================
## 0. Parametre  (ArcGIS-ækvivalenter)
## ==================================================================
bandwidth   <- 60000      # m: kernel-bandbredde (ArcGIS "Bandwidth")
power       <- 1          # 1 = lokal lineær (ArcGIS "Power"); 0 = kerne-vægtet middel
ridge       <- 0.1          # ridge-stabilisering (IKKE Esris 100 - kalibrér, se LOO nederst)
kernel_type <- "gaussian" # "gaussian" | "exponential" | "quartic"
use_barrier <- TRUE      # TRUE = cost-distance uden om land (kræver gdistance)
min_weight  <- 1e-6       # celler hvor alle vægte < dette -> NA (for langt fra data)

# Hvilket data-snit interpoleres? Ét år, eller middel over en periode.
target_year <- NULL       # fx 2024 for ét år; NULL -> middel over 'period_years'
period_years <- 2010:2024 # bruges når target_year er NULL


# log værdi for hver station og beholder koorDIPator

DIP_emmeans <- read.csv(file.path(PATHS$input_pressure, "MFS_DIN_DIP","GLM_DIP.csv"))

pts <- DIP_emmeans %>%
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
               gaussian    = function(u) exp(-(u^2)),            # u = d/bandwidth, sikrer afstands-vægtning
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
  if (anyNA(d)) { ok <- !is.na(d); if (!any(ok)) next } else ok <- rep(TRUE, length(d)) #d er afstanden fra celle j til alle stationer. Med barrierer er nogle stationer NA (ingen vand-vej) — ok vælger kun de nåelige. Er ingen nåelige, springes cellen over (bliver NA).
  w <- kern(d[ok] / bandwidth) #Er selv den nærmeste station så langt væk, at alle vægte er ~0, droppes cellen. Det er dét, der laver "huller" i datatomme områder frem for at gætte vildt.
  if (max(w, na.rm = TRUE) < min_weight) next        # for langt fra data
  
  
  
  #Det afgørende trick: koorDIPaterne er centreret på cellen (station minus celle). Så i selve cellen er U=V=0. 
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
pred_r_DIP <- grid_raster
terra::values(pred_r_DIP) <- pred   # log-skala

se_r_DIP   <- grid_raster
terra::values(se_r_DIP)   <- pse    # log-skala SE

# tilbage til koncentration (µg/l): value = 10^pred - 1
conc_r_DIP <- 10^pred_r_DIP - 1
# SE på koncentrations-skala via delta-metode: d(10^p)/dp = ln(10)*10^p
conc_se_r_DIP<- log(10) * (10^pred_r_DIP) * se_r_DIP

conc_r_DIP <- terra::mask(conc_r_DIP,    assessment_area_vect)
conc_se_r_DIP <- terra::mask(conc_se_r_DIP, assessment_area_vect)
names(conc_r_DIP) <- "DIP_ugl"
names(conc_se_r_DIP) <- "SE_DIP_ugl"

# normaliseret presfaktor-lag 0-1 (efter maskering)
DIP_norm <- terra::scale_linear(conc_r_DIP)
names(DIP_norm) <- "value"

terra::writeRaster(conc_r_DIP,    file.path(PATHS$output_pressure_tif, "nutrients","vinter_DIP_kernel_ugl.tif"),      overwrite = TRUE)
terra::writeRaster(conc_se_r_DIP, file.path(PATHS$output_pressure_tif, "nutrients","vinter_DIP_kernel_se.tif"),       overwrite = TRUE)
terra::writeRaster(DIP_norm,   file.path(PATHS$output_pressure_tif, "nutrients","vinter_DIP_kernel_norm.tif"),     overwrite = TRUE)

plot(conc_r_DIP)
## ==================================================================
## 5. Bilag-plots (værdi + usikkerhed)
## ==================================================================
map_eu <- st_read(file.path(PATHS$input_assessment_area, "/maps/Europe/Europe_merged3035.shp")) %>%
  st_transform(crs = target_crs)
pts_sf <- pts %>%
  distinct(Station, x,y) %>%
  st_as_sf(., coords = c("x", "y"), crs = target_crs, remove = FALSE)


conc_as_sf_DIP <- conc_r_DIP %>%
  terra::as.polygons(dissolve = FALSE) %>% 
  st_as_sf() %>%
  st_transform(target_crs) %>% 
  st_make_valid() 

conc_norm_as_sf_DIP <- DIP_norm %>%
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
  geom_sf(data = conc_as_sf_DIP, aes(fill = DIP_ugl), color = NA) +
  geom_sf(data = maalt_2010_2024mean, aes(fill = mn_val),
          shape = 21, color = "white", size = 8) +
  #geom_text(data =pts_sf, aes(x=x, y=y, label = Station))+
  geom_text(data = ices_pos, aes(x = x, y = y, label = stedID))+
  scale_fill_viridis_c(name = "TN (µg/l)") +      
  labs(title = "Marin DIP (kernel) og målte stationsmidler 2010–2024, samt Station navne på samtlige stationer") +
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

map_DIP<- ggplot() +
  geom_sf(data = map_eu, fill = "#c3fbb1", color = NA, alpha = 0.3) +
  geom_sf(data = conc_as_sf_DIP, aes(fill = DIP_ugl), color = NA) +
  #geom_sf(data = pts_sf, color = "red", size = 0.6) +
  scale_fill_viridis_c(name = "DIP (µg/l)")+ boundary +
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
