#---------------------- Marin total nitrogen (GLM) -----------------------
# Alternativ til IDW/regressions-infilling: én spatio-temporal GAM på ALLE
# ODA + ICES TN-målinger samtidig. Modellen "låner styrke" rumligt og tidsligt,
# estimerer et nutidsfelt dér hvor de nedlagte kyststationer lå, og giver
# TROVÆRDIGE usikkerheder (SE/CV) i stedet for et ustabilt punkt-forhold.
#
# Hvorfor GAM frem for infilling:
#   - infilling antog at forholdet mellem to stationer er konstant over tid;
#     valideringen viste R²~0,30 og retningsafhængig bias -> antagelsen holder ikke.
#   - GAM antager i stedet at feltet varierer GLAT i rum og tid. Space-time-
#     interaktionen (ti(x,y,year)) tillader at kysten falder anderledes end
#     midtbugten - præcis det, infilling ikke kunne fange.
#
# Pipeline (samme mønster som de øvrige lag):
#   harmoniser ODA+ICES -> dedup -> overflade/sæson/område-filter -> fit GAM ->
#   diagnostik -> predict til grid for et nutidsår -> rasteriser -> maskér ->
#   scale_linear (0-1) -> gem tif + bilag-png (+ usikkerheds-png).
source("scripts/00_setup.R")
PATHS <- set_project_paths()



#ICES data - har ændret DIN laget i denne fil så det kun er Nitrat og Nitrit, da det ODA data er opdelt i Ammoniak+ammonium og Nitrat-Nitrit

ices_din_kor <- read_csv2(file.path(PATHS$input_pressure,
                               "MFS_DIN_DIP",
                               "nitrat_nitrit.csv"))


ices_alt <- read_csv(file.path(PATHS$input_pressure,
                           "MFS_DIN_DIP",
                           "QV_data_baltic.csv"))


#  Udtræk fra ODA
oda <- read_csv2(file.path(PATHS$input_pressure,
                           "MFS_DIN_DIP",
                           "HentData_Hav_Vandkemi_20261006-110319.csv"),
                 locale = locale(encoding = "latin1")) %>%
  mutate(date_correct = ymd(Startdato)) %>%
  mutate(year= year(date_correct),
         month = month(date_correct)) 


# --- ODA-------------------------------------------------------------
oda_param_navn    <- "Nitrogen,total N"   # ODA: unique(oda$Parameter)
oda_value_faktor  <- 1                     # ODA Resultat er µg/l -> faktor 1
ices_value_faktor <- 14.00672              # ICES µmol/l -> µg/l (molmasse N)
overflade_m       <- 10                    # kun 0-10 m (overflade)
catch_buffer      <- 5000                  # m: medtag punkter så langt uden for området
dup_threshold_m   <- 200                   # m: ICES-station tættere end dette på ODA = dublet


# --- fælles filtre -----------------------------------------------------------
overflade_m   <- 10                    
vinter_mdr    <- NULL #c(12, 1, 2)           # vintermiddel-konvention; NULL = alle mdr.



oda_df <- oda %>%
  filter(Parameter == oda_param_navn,
         GennemsnitsDybde_m <= overflade_m,
         Prøvetype == "Enkeltprøve") %>%        # fravælger Blandingsprøve (~1985-2006)
  transmute(
    stedID = as.character(ObservationsStedNr),
    Parameter = "TotN",
    source = "ODA",
    date   = as.Date(date_correct),
    year   = as.integer(year),
    month  = as.integer(month),
    depth  = as.numeric(GennemsnitsDybde_m),
    value  = as.numeric(Resultat) * oda_value_faktor,
    x      = as.numeric(X_UTM32),
    y      = as.numeric(Y_UTM32),
    metode = "Enkeltprøve"
  ) %>%
  filter(!is.na(value), !is.na(x), !is.na(y))

# ICES: WGS84 -> UTM32 
# og klippes med det samme til assessment-området.
ices_df <- ices_alt %>%
  filter(Parameter == "TotalNitrogen",
         as.numeric(Depth) <= overflade_m) %>%
  transmute(
    stedID = as.character(ClusterID),
    Parameter = "TotN",
    source = "ICES",
    date   = make_date(Year, Month, Day),
    year   = as.integer(Year),
    month  = as.integer(Month),
    depth  = as.numeric(Depth),
    value  = as.numeric(Value) * ices_value_faktor,
    metode = "ICES",
    Longitude, Latitude
  ) %>%
  filter(!is.na(value), !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(target_crs) %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_intersection(assessment_area_dissolved) %>%
  st_drop_geometry() %>%
  dplyr::select(stedID, Parameter, source, date, year, month, depth, value, metode, x, y)


####################
##  håndtering af potientelle ICES dupletter der dækkes af ODA (afstand til nærmeste ODA-punkt)
# Finder de nærmeste punkter med sf pakken og tar så gennemsnitlig position og værdi for de punkter
# På denne måde sikrer vi, at vi ikke smidder brugbare data ud
#


oda_sf   <- st_as_sf(oda_df, coords = c("x", "y"), crs = target_crs, remove = FALSE)
ices_sf  <- st_as_sf(ices_df, coords = c("x", "y"), crs = target_crs, remove = FALSE)
oda_stn <- oda_sf %>%distinct(stedID, .keep_all = TRUE)
ices_stn <- ices_sf %>% distinct(stedID, .keep_all = TRUE)

nn <- st_nearest_feature(ices_stn, oda_stn)
ices_stn$stedID_oda <- oda_stn$stedID[nn]
ices_stn$dist_oda   <- as.numeric(st_distance(ices_stn, oda_stn[nn, ], by_element = TRUE))

dup_map <- ices_stn %>%
  st_drop_geometry() %>%
  filter(dist_oda < dup_threshold_m) %>%
  dplyr::select(stedID, stedID_ny = stedID_oda)

omdoeb_dup <- function(df) {
  df %>%
    left_join(dup_map, by = "stedID") %>%
    mutate(stedID = coalesce(stedID_ny, stedID)) %>%
    dplyr::select(-stedID_ny)
}

stn_pos <- bind_rows(
  oda_stn  %>% st_drop_geometry() %>% dplyr::select(stedID, x, y),
  ices_stn %>% st_drop_geometry() %>% dplyr::select(stedID, x, y) %>% omdoeb_dup()
) %>%
  group_by(stedID) %>%
  summarise(x = mean(x), y = mean(y), n_stationer = n(), .groups = "drop")
  
# Samlet datasæt
df_comb <- bind_rows(oda_df, ices_df) %>%
  omdoeb_dup() %>%
  dplyr::select(-c(x,y)) %>%
  group_by(stedID, Parameter, year, month) %>%
  summarise(value = mean(value, na.rm = TRUE),
            source =paste(sort(unique(source)), collapse = "+"),
            .groups = "drop") %>%
  left_join(stn_pos, by = "stedID")

comb_sf <- st_as_sf(df_comb, coords = c("x", "y"), crs = target_crs, remove = FALSE)

# Fjerner stationer hvor med en enkelt måling og år hvor der kun er målt for en enkelt måned
df_comb_filter <- df_comb %>%
  group_by(stedID) %>%
  summarise(n_pr_stat = n(), .groups = "drop") %>%
  left_join(df_comb) %>%
  filter(!n_pr_stat < 2) %>%
  dplyr::select(-n_pr_stat)

comb_final <- df_comb_filter %>%
  group_by(stedID, year) %>%
  summarise(n_pr_yr = n(), .groups = "drop") %>%
  left_join(df_comb_filter) %>%
  filter(!n_pr_yr < 2) %>%
  dplyr::select(-c(n_pr_yr))
  

tst_yr_mean_avg <- comb_final %>%
  group_by(stedID, year) %>%
  summarise(mn_val = mean(value, na.rm =TRUE), .groups = "drop")
## Aggregér pr. station/år/måned (overflade-middel pr. udtag)
# Præstø skal med i sidste ende, så data beholdes her
# ##  Rumlig afgrænsning: Køge Bugt (+ buffer), uden Præstø Fjord
# 
# fjord_box <- st_bbox(c(xmin = 11.85, xmax = 12.10, ymin = 55.00, ymax = 55.21),
#                      crs = 4326) %>%
#   st_as_sfc() %>%
#   st_transform(target_crs)
# 
# cast_sf <- cast %>%
#   st_as_sf(coords = c("x", "y"), crs = target_crs, remove = FALSE) %>%
#   st_filter(st_buffer(assessment_area_dissolved, catch_buffer))
# 
# cast_sf$i_fjord <- lengths(st_intersects(cast_sf, fjord_box)) > 0
# cast_sf <- cast_sf[!cast_sf$i_fjord, ]
# 
# dat <- st_drop_geometry(cast_sf)
# message("Målinger til GLM: ", nrow(dat),
#         "  |  stationer: ", dplyr::n_distinct(dat$stedID),
#         "  |  år: ", paste(range(dat$year), collapse = "-"))


## konverter til log data
library(emmeans)

dat <- comb_final %>% mutate(log_value = log10(value + 1))

hist(dat$value,     xlab = "TotN (µg/l)",   col = "green", border = "black", main = "Rå TotN")
hist(dat$log_value, xlab = "log10(TotN+1)", col = "green", border = "black", main = "Log-transformeret")

glm_df <- dat %>%
  mutate(Station = factor(stedID),
         yearf   = factor(year),
         monthf  = factor(month))

#### Model ####
# Additiv lineær model på log10-skala: stationsniveau + årseffekt + sæson
m_glm <- lm(log10(value + 1) ~ Station + yearf + monthf, data = glm_df)

plot(m_glm)                                   # diagnostik

# Robuste standardfejl (heteroskedasticitet og tunge haler)
vc  <- vcovHC(m_glm, type = "HC3") # Dette gøres på baggrund af Residuals vs fitted hvor der tydeligt ikke er nogen homoskedasticitet, deraf heteroskedasticitet
# Her beregnes "Meat" i sandwich-estimator
# Dette fortæller R at den skal droppe antagelsen om konstant varians og i stedet estimere en ny varians-kovarians-matrix baseret på dine faktiske residualer.
# type = "HC3" tager højde for små stikprøvestørrelser og datapunkter med høj "leverage" (ekstreme x-værdier), hvilket gør den utrolig robust.

sig <- sigma(m_glm) # bruges til bias-korrektion, altså gå fra log-transformerede tilbage til normale værdier (arimetisk vs geometrisk gennemsnit)
#residualstandardafvigelse (sig)
car::Anova(m_glm, type = 2, vcov. = vc)
# ANOVA resultatet fortolket
#Pr(>F) 2.2e-16 *** for station, year og month. De har derfor alle betydning for test værdien (value)
# Station (Df = 31, F = 73.459): Der er gigantiske forskelle på tværs af de 32 stationer (31 frihedsgrader). Geografien eller lokaliteten spiller en massiv rolle for værdierne
# yearf (Df = 43, F = 17.405): Der er en meget klar og signifikant udvikling eller variation fra år til år. Tidsfaktoren er vigtig.
# monthf (Df = 11, F = 37.867): Der er stærk sæsonvariation. Værdierne ændrer sig markant alt efter, hvilken måned på året der måles i.


#### Stationsoversigt ####
stn_info <- glm_df %>%
  group_by(Station = as.character(Station)) %>%
  summarise(n_maalinger = n(),
            n_aar       = n_distinct(year),
            foerste     = min(year),
            sidste      = max(year),
            .groups = "drop")

#### 4. Målte årsværdier 2010–2024, så hvor skiftet fra mange til få stationer sker ####
aar <- 2010:2024

maalt <- glm_df %>%
  filter(year %in% aar) %>%
  group_by(Station = as.character(Station), year) %>%
  summarise(value_maalt = mean(value),          # aritmetisk gennemsnit
            n_maaneder  = n_distinct(month),
            .groups = "drop")

####  Modelbaserede aritmetiske årsgennemsnit ####
# Månedsestimater på log-skala -> tilbagetransformeret med bias-korrektion
# -> aritmetisk gennemsnit over måneder på den oprindelige skala
rg <- emmeans(m_glm, ~ Station + yearf + monthf,
              at = list(yearf = as.character(aar)), vcov. = vc) # vcov. = vc, sikre at deres tages højde for heteroskedasticitet

rg_resp <- regrid(rg, transform = "response", bias.adjust = TRUE, sigma = sig) 
em_arit <- emmeans(rg_resp, ~ Station + yearf) # her findes årsgennemsnit pr station hvor der tages højde for sæsonvariation

model_df <- as.data.frame(em_arit) %>%
  transmute(Station     = as.character(Station),
            year        = as.numeric(as.character(yearf)),
            value_model = response,
            SE, lower.CL, upper.CL)

#### Samlet tabel: alle stationer × år, udfyldt hvor der mangler målinger ####
samlet_df <- expand_grid(Station = levels(glm_df$Station), year = aar) %>%
  left_join(maalt,    by = c("Station", "year")) %>%
  left_join(model_df, by = c("Station", "year")) %>%
  left_join(stn_info, by = "Station") %>%
  mutate(kilde = if_else(is.na(value_maalt), "modelleret", "målt"),
         value = coalesce(value_maalt, value_model),
         aar_udenfor_periode = pmax(foerste - year, year - sidste, 0)) %>%
  arrange(Station, year)

# Koordinater på
samlet_df_final <- samlet_df %>%
  left_join(stn_pos %>% mutate(stedID = as.character(stedID)) %>% select(stedID, x, y),
            by = c("Station" = "stedID"))

write.csv(samlet_df_final,file.path(PATHS$input_pressure,"MFS_DIN_DIP","GLM_TN.csv"), row.names = FALSE) 

samlet_sf <- st_as_sf(samlet_df_final %>% distinct(Station, x,y), coords = c(x = "x", y = "y"), crs = target_crs, remove = FALSE)


ggplot() +
  geom_sf(data = assessment_area_dissolved, fill = "lightblue", color = NA, alpha = 0.6) +
  geom_sf(data = samlet_sf, aes(geometry = geometry, color = Station), fill = NA) +
  geom_text(data = samlet_sf, aes(x = x , y=y, label = Station))+
  theme_minimal()

# check

stat_new_yr <- samlet %>%
  filter(Station == 97120011) %>%
  dplyr::select(stedID = Station, value = value_model, year) %>%
  mutate(value_type = "model")


comb_final_new_yr <- comb_final  %>%
  filter(stedID == "97120011") %>%
  dplyr::select(stedID, value, year) %>%
  mutate(value_type = "målt")


tst_97120011 <- bind_rows(stat_new_yr,comb_final_new_yr)


ggplot()+
  geom_point(data = tst_97120011, aes(x = year, y = value))+
  theme_minimal()


