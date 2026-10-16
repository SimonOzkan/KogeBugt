
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


 
# Vi starter med at finde den station der ligger midt ude i Køge Bugt, da den er den længste tidserie
oda_historisk_TOTN <- oda %>%
  filter(Parameter == "Nitrogen,total N")

df_sted_ny <- oda_historisk_TOTN%>%
  filter(year >= 2011) %>%
  distinct(ObservationsStedNr , X_UTM32, Y_UTM32 ) %>%
  st_as_sf(., coords = c("X_UTM32","Y_UTM32"), crs = target_crs)  

ggplot() +
  geom_sf(data = assessment_area_dissolved, fill = "lightblue", color = NA, alpha = 0.6) +
  geom_sf(data = df_sted_ny, aes(geometry = geometry, color = NA), fill = NA) +
  geom_sf_text(data = df_sted_ny, aes(geometry = geometry, label = ObservationsStedNr ), size = 3, check_overlap = TRUE) +
  theme_minimal()

# 

oda_koge <- oda_historisk_TOTN %>% 
  filter(ObservationsStedNr  %in% c("97120010","97000007", "99000051", "97100006")) %>%
  group_by(year, ObservationsStedNr ) %>%
  summarise(antal_obs = n(), .groups = "drop")

ggplot(data = oda_koge, aes(x =year, y = antal_obs)) +
  geom_point()+
  facet_wrap(~ ObservationsStedNr , scales = "free_y")

### Først finder vi de stationer der har historisk lang tids målinger. 
# Første plot viser at de stationer med nyere data (efter 2011 er)# 97120010, som også er dette der ligger midt i Køge Bugt og er interressant i forhold til de kystnære. Herudover er det
# 97000007, 99000051 og 97100006, men af scatterplot ser vi at station 97000007, kun har målinger fra to af, og 99000051 fra 2011 og frem
# Så reelt kun 97100006 og 97120010 der har lange tidsserier

# Nu filtrerer vi så kun de stationer helt langs kysten ved Køge Bugt, som vi vil modellere data for 

df_sted_gammel <- oda_historisk_TOTN%>%
  filter(year <= 2011,
         !ObservationsStedNr  %in% c("97120010","97000007", "99000051", "97100006")) %>%
  distinct(ObservationsStedNr , xutm = `x-koordinat`, yutm = `y-koordinat`) %>%
  st_as_sf(., coords = c("xutm","yutm"), crs = target_crs)  

ggplot() +
  geom_sf(data = assessment_area_dissolved, fill = "lightblue", color = NA, alpha = 0.6) +
  geom_sf(data = df_sted_gammel, aes(geometry = geometry, color = NA), fill = NA) +
  geom_sf_text(data = df_sted_gammel, aes(geometry = geometry, label = ObservationsStedNr ), size = 3, check_overlap = TRUE) +
  theme_minimal()

### Tjekker deres tidsserier

df_gammel_obs <- oda_historisk_TOTN %>% 
  filter(!ObservationsStedNr  %in% c("97120010","97000007", "99000051", "97100006")) %>%
  group_by(year, ObservationsStedNr ) %>%
  summarise(antal_obs = n(), .groups = "drop")

ggplot(data = df_gammel_obs, aes(x =year, y = antal_obs)) +
  geom_point()+
  facet_wrap(~ ObservationsStedNr , scales = "free_y")



###### Tjekker om prøvetype har påvirkning på værdien

tn_aar_test <- oda_historisk_TOTN %>%
  filter(Parameter == param_navn,
         ObservationsStedNr  %in% c(test_station),
         GennemsnitsDybde_m < dybde)


tn_aar_ref <- oda_historisk_TOTN %>%
  filter(Parameter == param_navn,
         ObservationsStedNr  %in% c(ref_station),
         GennemsnitsDybde_m < dybde)
ggplot()+
  geom_point(data = tn_aar_test, aes(x = date_correct, y = Resultat, color = Prøvetype),  shape = 2)+
  geom_point(data = tn_aar_ref, aes(x = date_correct, y = Resultat, color = Prøvetype), shape = 1)+
  theme_minimal()





df <- oda_historisk_TOTN %>%
  filter(Parameter == param_navn,          # <- dit faktiske kolonnenavn
         GennemsnitsDybde_m < dybde)

table(df$Prøvetype, df$year)                    # skifter typen over årene?
table(df$Prøvetype, df$ObservationsStedNr)      # bruges forskellige typer på forskellige stationer?


library(mgcv)

# log-link fordi koncentrationer virker multiplikativt
m <- gam(Resultat ~ Prøvetype + s(year) + s(GennemsnitsDybde_m) +
           s(ObservationsStedNr, bs = "re"),   # station som random effect
         family = Gamma(link = "log"), data = df)

summary(m)   # kig på den parametriske Prøvetype-række


parret <- df %>%
  group_by(ObservationsStedNr, date_correct) %>%
  filter(n_distinct(Prøvetype) > 1) %>%
  ungroup()

nrow(parret)   # typisk 0 i marine data - men værd at tjekke



plot_df <- df %>% filter(ObservationsStedNr %in% c(test_station, ref_station))

# a) tidsserie, farve = prøvetype, facet = station
ggplot(plot_df, aes(date_correct, Resultat, color = Prøvetype)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ ObservationsStedNr, ncol = 1) +
  theme_minimal()

# b) fordeling pr. prøvetype (det "rå" blik - husk det er ujusteret)
ggplot(plot_df, aes(Prøvetype, Resultat, fill = Prøvetype)) +
  geom_boxplot(alpha = 0.5, outlier.size = 0.6) +
  facet_wrap(~ ObservationsStedNr) +
  theme_minimal() + theme(legend.position = "none")



#---------------------- Validering: regressions-infilling af TN -----------------
# Tester om man kan rekonstruere en stations nutidige TN ud fra en anden
# fortsættende station, ved at lære forholdet i overlaps-perioden (før 2011).
#
# Princip (leave-one-out): behandl en FORTSÆTTENDE station som "nedlagt".
# Fit forhold til reference-station på data FØR split-året, forudsig EFTER,
# og sammenlign med stationens FAKTISKE efter-målinger.


## ------------------------------------------------------------------
test_station <- "97100006"      
ref_station  <- "97120010"       # 97120010
param_navn   <- "Nitrogen,total N"   
dybde <- 10 
split_year   <- 2011            # før = fit, >= = validering
vinter_mdr   <- NULL #c(12,1,3) #for vintermiddel; NULL = alle måneder, kan ved DIN og DIP
provetype <- "Enkeltprøve" #to typer prøver, enkelt og blandings. Blandingsprøve stoppede omkring 2006 med at blive anvendt


#Aggregér TN pr. station og år

tn_aar <- oda_historisk_TOTN %>%
  filter(Parameter == param_navn,
         ObservationsStedNr  %in% c(test_station, ref_station),
         GennemsnitsDybde_m < dybde,
         Prøvetype == provetype) %>%
  { if (!is.null(vinter_mdr)) filter(., month %in% vinter_mdr) else . } %>%
  group_by(ObservationsStedNr , year) %>%
  summarise(TN = mean(Resultat, na.rm = TRUE), .groups = "drop")



# Bred form: én kolonne pr. station, kun år hvor BEGGE har data


tn_wide <- tn_aar %>%
  tidyr::pivot_wider(names_from = ObservationsStedNr , values_from = TN)

# Robust udtræk (kolonnenavne = station-id'er som strenge)
tn_wide$test <- tn_wide[[test_station]]
tn_wide$ref  <- tn_wide[[ref_station]]

tn_wide <- tn_wide %>%
  dplyr::filter(!is.na(test), !is.na(ref))


# Split, fit og forudsig

fit_df <- tn_wide %>% dplyr::filter(year <  split_year)
val_df <- tn_wide %>% dplyr::filter(year >= split_year)

message("Fit-år (overlap): ", nrow(fit_df), "  |  Validerings-år: ", nrow(val_df))

mod <- lm(test ~ ref, data = fit_df)
print(summary(mod))

val_df$pred <- predict(mod, newdata = val_df)


## ------------------------------------------------------------------
## 4. Fejlmål
## ------------------------------------------------------------------

resid   <- val_df$test - val_df$pred
rmse    <- sqrt(mean(resid^2))
bias    <- mean(resid)
r2_val  <- if (nrow(val_df) > 1) cor(val_df$test, val_df$pred)^2 else NA
rel_err <- rmse / mean(val_df$test) * 100

cat("\n--- Validering (", test_station, " forudsagt fra ", ref_station, ") ---\n", sep = "")
cat(sprintf("  n validerings-år : %d\n", nrow(val_df)))
cat(sprintf("  RMSE             : %.1f µg/l  (%.0f %% af middel)\n", rmse, rel_err))
cat(sprintf("  Bias (middelfejl): %.1f µg/l\n", bias))
cat(sprintf("  R² (forudsagt vs faktisk): %.2f\n", r2_val))


## ------------------------------------------------------------------
## 5. Plots
## ------------------------------------------------------------------

# a) Forudsagt vs. faktisk (med 1:1-linje)
p_scatter <- ggplot(val_df, aes(x = test, y = pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 2, color = "#D55E00") +
  labs(title = paste0("Validering: ", test_station, " forudsagt fra ", ref_station),
       subtitle = sprintf("RMSE = %.1f µg/l (%.0f %%), bias = %.1f, R² = %.2f",
                          rmse, rel_err, bias, r2_val),
       x = "Faktisk TN (µg/l)", y = "Forudsagt TN (µg/l)") +
  coord_equal() +
  theme_minimal()
print(p_scatter)

# b) Tidsserie: faktisk test, forudsagt test, reference
ts_df <- dplyr::bind_rows(
  tn_wide %>% transmute(year, TN = test, serie = paste0(test_station, " (faktisk)")),
  val_df  %>% transmute(year, TN = pred, serie = paste0(test_station, " (forudsagt)")),
  tn_wide %>% transmute(year, TN = ref,  serie = paste0(ref_station, " (reference)"))
)

p_ts <- ggplot(ts_df, aes(x = year, y = TN, color = serie)) +
  geom_vline(xintercept = split_year - 0.5, linetype = "dotted", color = "grey50") +
  geom_point() +
  geom_line(alpha = 0.6) +
  labs(title = "Tidsserier: faktisk vs. forudsagt", x = "År", y = "TN (µg/l)", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_ts)




