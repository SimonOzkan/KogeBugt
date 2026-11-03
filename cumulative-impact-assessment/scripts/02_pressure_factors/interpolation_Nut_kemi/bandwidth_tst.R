## ==================================================================
## Valg af båndbredde via krydsvalidering (leave-one-out)
## ------------------------------------------------------------------
## Idé: Hver station udelades én ad gangen, dens værdi forudsiges ud fra
## de øvrige stationer med præcis samme metode som i hovedscriptet
## (kernel + lokal lineær ridge-WLS), og forudsigelsen sammenlignes med
## den målte værdi. Det gentages for en række båndbredder, og den
## båndbredde med mindst forudsigelsesfejl vælges.
##
## Forudsætter at disse objekter allerede findes fra hovedscriptet:
##   pts          (Station, x, y, lvalue)   - stationsgennemsnit på log10(x+1)
##   use_barrier  TRUE/FALSE
##   water_vect   (kun hvis use_barrier = TRUE)
##   grid_raster
##   kernel_type  "gaussian" / "exponential" / "quartic"
##   power        1 = lokal lineær, 0 = lokalt vægtet gennemsnit
##   ridge
##   min_weight
## ==================================================================

library(dplyr)
library(ggplot2)

## ---- Indstillinger ---------------------------------------------------
bw_grid_km <- c(5, 10, 15, 20, 25, 30, 40, 50, 60, 75, 100)  # båndbredder der testes

# Stationer inden for denne radius af den udeladte station udelades også.
# 0 = almindelig leave-one-out. Sæt fx 2-5 km hvis ODA og ICES har stationer
# næsten oven i hinanden, ellers "snyder" testen og favoriserer små båndbredder.
exclude_radius_km <- 0

# Mindste andel af stationer der skal kunne forudsiges, før en båndbredde
# kan vælges som den bedste (små båndbredder efterlader huller).
min_coverage <- 0.9

out_dir <- "C:\\Users\\SIO\\NIVA\\Køge Bugt - General\\data\\Pressure\\MFS_DIN_DIP\\bandwidt_tst"   # hvor tabel og figur gemmes

## ---- Stationer --------------------------------------------------------
st_xy <- as.matrix(pts[, c("x", "y")])
yv    <- pts$lvalue
n_st  <- nrow(st_xy)

# Lige-linje-afstand bruges altid til eksklusionsradius
D_euc <- as.matrix(dist(st_xy))

# Afstand mellem stationer, beregnet på samme måde som i hovedscriptet
if (!use_barrier) {
  D_ss <- D_euc
} else {
  water <- terra::rasterize(water_vect, grid_raster, field = 1, background = NA)
  rl    <- raster::raster(water)
  tr    <- gdistance::transition(rl, transitionFunction = function(x) 1, directions = 16)
  tr    <- gdistance::geoCorrection(tr, type = "c")
  D_ss  <- as.matrix(gdistance::costDistance(tr, st_xy))
  D_ss[!is.finite(D_ss)] <- NA   # ingen vand-vej mellem stationerne
}

## ---- Kernel og lokal model (samme som hovedscriptet) -----------------
kern <- switch(kernel_type,
               gaussian    = function(u) exp(-(u^2)),
               exponential = function(u) exp(-u),
               quartic     = function(u) ifelse(u < 1, (1 - u^2)^2, 0),
               stop("ukendt kernel_type"))

local_fit <- function(d, dx, dy, y, bw) {
  ok <- !is.na(d)
  if (!any(ok)) return(NA_real_)
  w <- kern(d[ok] / bw)
  if (max(w) < min_weight) return(NA_real_)
  
  if (power == 1) {
    X <- cbind(1, dx[ok] / bw, dy[ok] / bw)
    P <- diag(c(0, 1, 1))
  } else {
    X <- matrix(1, nrow = sum(ok), ncol = 1)
    P <- matrix(0, 1, 1)
  }
  
  sw   <- sqrt(w)
  Xw   <- X * sw
  A    <- crossprod(Xw) + ridge * P
  b    <- crossprod(Xw, y[ok] * sw)
  beta <- tryCatch(solve(A, b), error = function(e) NULL)
  if (is.null(beta)) NA_real_ else beta[1]
}

## ---- Krydsvalidering --------------------------------------------------
bw_m <- bw_grid_km * 1000
pred_mat <- matrix(NA_real_, nrow = n_st, ncol = length(bw_m),
                   dimnames = list(pts$Station, paste0("bw_", bw_grid_km, "km")))

for (k in seq_along(bw_m)) {
  for (i in seq_len(n_st)) {
    keep    <- D_euc[i, ] > exclude_radius_km * 1000
    keep[i] <- FALSE
    if (!any(keep)) next
    pred_mat[i, k] <- local_fit(d  = D_ss[i, keep],
                                dx = st_xy[keep, 1] - st_xy[i, 1],
                                dy = st_xy[keep, 2] - st_xy[i, 2],
                                y  = yv[keep],
                                bw = bw_m[k])
  }
  message("Færdig: båndbredde ", bw_grid_km[k], " km")
}

## ---- Fejlmål ----------------------------------------------------------
obs_c <- 10^yv - 1

# Fælles sæt: stationer som ALLE båndbredder kan forudsige.
# Det gør sammenligningen fair, fordi små båndbredder ellers kun bliver
# testet på de "lette" stationer i tætte områder.
common <- rowSums(is.na(pred_mat)) == 0

score <- function(p, idx) {
  p_c <- 10^p - 1
  c(rmse_log = sqrt(mean((p[idx] - yv[idx])^2)),
    rmse_ugl = sqrt(mean((p_c[idx] - obs_c[idx])^2)),
    mae_ugl  = mean(abs(p_c[idx] - obs_c[idx])),
    bias_ugl = mean(p_c[idx] - obs_c[idx]))
}

cv_res <- bind_rows(lapply(seq_along(bw_m), function(k) {
  p   <- pred_mat[, k]
  ok  <- !is.na(p)
  all <- score(p, ok)
  com <- if (any(common)) score(p, common) else rep(NA_real_, 4)
  data.frame(bandwidth_km     = bw_grid_km[k],
             andel_forudsagt  = mean(ok),
             rmse_log_alle    = all[["rmse_log"]],
             rmse_ugl_alle    = all[["rmse_ugl"]],
             rmse_log_faelles = com[["rmse_log"]],
             rmse_ugl_faelles = com[["rmse_ugl"]],
             mae_ugl_faelles  = com[["mae_ugl"]],
             bias_ugl_faelles = com[["bias_ugl"]])
}))

message("\nStationer i fælles sæt: ", sum(common), " af ", n_st)
print(cv_res, digits = 3)

## ---- Vælg bedste båndbredde ------------------------------------------
# Vælges på log-skala (samme skala som modellen fitter på), blandt de
# båndbredder der dækker mindst min_coverage af stationerne.
kandidater <- cv_res %>% filter(andel_forudsagt >= max(andel_forudsagt) - 0.001)
if (nrow(kandidater) == 0) {
  warning("Ingen båndbredde dækker ", min_coverage * 100,
          " % af stationerne. Sænk min_coverage eller min_weight.")
  kandidater <- cv_res
}
best_bw_km <- kandidater$bandwidth_km[which.min(kandidater$rmse_log_alle)]
message("\nBedste båndbredde: ", best_bw_km, " km")

## ---- Figur ------------------------------------------------------------
p_cv <- ggplot(cv_res, aes(bandwidth_km, rmse_log_faelles)) +
  geom_line(colour = "grey40") +
  geom_point(aes(colour = andel_forudsagt >= min_coverage), size = 3) +
  geom_vline(xintercept = best_bw_km, linetype = "dashed", colour = "red") +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey70"),
                      labels = c(`TRUE` = "Ja", `FALSE` = "Nej"),
                      name = paste0("Dækker ≥ ", min_coverage * 100, " %")) +
  labs(title = "Krydsvalidering af båndbredde (DIP)",
       subtitle = paste0("Kernel: ", kernel_type,
                         " · bedste båndbredde: ", best_bw_km, " km"),
       x = "Båndbredde (km)",
       y = "RMSE, log10(DIP + 1)") +
  theme_minimal()
print(p_cv)

## ---- Gem --------------------------------------------------------------
write.csv(cv_res, file.path(out_dir, "bandwidth_cv_DIP.csv"), row.names = FALSE)
ggsave(file.path(out_dir, "bandwidth_cv_DIP.png"), p_cv, width = 7, height = 4.5, dpi = 200)

# Brug resultatet i hovedscriptet:
# bandwidth <- best_bw_km * 1000