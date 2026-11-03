# =============================================================================
# 04b_identification.R -- what the impulse-response targets identify
#
#   I1  Profile of the matching distance over theta, kappa and eta: fix one
#       parameter on a grid and re-estimate the other six.
#   I2  Sensitivity of the model's rent and price responses to kappa and eta
#       at the estimates (which horizons move each parameter).
#   I3  Profile of the distance over omega, re-estimating the other seven.
#
# The joint behaviour of kappa and eta across the bootstrap draws is in
# 05b_bootstrap_summary.R, so this script does not depend on the bootstrap.
#
# Reads:  everything 04_model_matching.R reads (it is sourced first).
# Writes: output/model_profile.csv, output/model_profile_omega.csv,
#         output/model_sensitivity.csv, output/model_ident.txt,
#         output/fig_model_profile.png
# =============================================================================

source("04_model_matching.R")   # model, targets, estimates and fit_best()

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

sink(file.path(OUTDIR, "model_ident.txt"), split = TRUE)

# ---- I1. Profiles ---------------------------------------------------------------
# Each grid point starts from the estimates and the other basins (basins() in
# 04_model_matching.R) with the parameter fixed at the grid value, and then from its
# neighbours' solutions, up and down the grid. Nothing is random, so the profile
# does not depend on the seed or on the number of cores.
better <- function(a, b) if (b$value < a$value) b else a
profile_grid <- function(nm) {
  k <- match(nm, pn); g <- grids[[nm]]; n <- length(g); free <- setdiff(1:7, k)
  res <- lapply(g, function(v) fit_best(lapply(basins(th), replace, k, v), J0, free = free))
  for (pass in 1:2) {
    for (i in 2:n)       res[[i]] <- better(res[[i]], fit_best(list(replace(res[[i - 1]]$par, k, g[i])), J0, free = free))
    for (i in (n - 1):1) res[[i]] <- better(res[[i]], fit_best(list(replace(res[[i + 1]]$par, k, g[i])), J0, free = free))
  }
  data.frame(param = nm, value = g, distance = sapply(res, `[[`, "value"),
             eps = sapply(res, function(r) r$par["eps"]),
             kappa = sapply(res, function(r) r$par["kappa"]), eta = sapply(res, function(r) r$par["eta"]),
             theta = sapply(res, function(r) r$par["theta"]),
             kappa_eta = sapply(res, function(r) r$par["kappa"] * r$par["eta"]))
}
grids <- list(theta = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6, 0.7, 0.8, 1),
              kappa = c(0.02, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1),
              eta   = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# Windows cannot fork, so the three grids run one after the other there.
P <- bind_rows(parallel::mclapply(names(grids), profile_grid, mc.preschedule = FALSE,
                                  mc.cores = if (.Platform$OS.type == "windows") 1L
                                             else min(3, parallel::detectCores())))
# theta = 1 was also fitted in 04_model_matching.R; keep the better optimum.
i1 <- P$param == "theta" & P$value == 1
P$distance[i1] <- min(P$distance[i1], f_t1$value)
P$excess <- P$distance - f0$value
rownames(P) <- NULL
cat("== I1 profiles (distance at the estimates:", round(f0$value, 2), ") ==\n")
print(P, digits = 3)
if (min(P$excess) < -1e-3) cat("NOTE: a profile point lies below the estimates' distance\n")
write_csv(P, file.path(OUTDIR, "model_profile.csv"))

# ---- I2. Sensitivity of the responses to kappa and eta -------------------------
sens <- function(j) {
  h <- 1e-4; a <- th; b <- th; a[j] <- th[j] + h; b[j] <- max(lo[j], th[j] - h)
  ma <- model_irf(a, J0); mb <- model_irf(b, J0)
  data.frame(param = pn[j], h = 0:H, d_rent = (ma$r - mb$r) / (a[j] - b[j]) * th[j],
             d_price = (ma$p - mb$p) / (a[j] - b[j]) * th[j])
}
S <- bind_rows(lapply(match(c("kappa", "eta"), pn), sens))
cat("\n== I2 elasticity-scaled sensitivity: d response / d log parameter ==\n")
print(S |> filter(h %in% c(1, 2, 4, 8, 12)), digits = 3)
write_csv(S, file.path(OUTDIR, "model_sensitivity.csv"))
mf <- model_irf(th, J0)
cat(sprintf("Rent response h12 / price h12 in the model: %.3f; h1 rent / h0 price: %.3f\n",
            mf$r[13] / mf$p[13], mf$r[2] / mf$p[1]))
# Rent response at h when kappa*eta is held fixed and kappa varies
ke <- th["kappa"] * th["eta"]
KE <- bind_rows(lapply(c(0.15, unname(th["kappa"]), 0.4, 1), function(k) {
  z <- th; z["kappa"] <- k; z["eta"] <- ke / k; m <- model_irf(z, J0)
  data.frame(kappa = k, eta = ke / k, r1 = m$r[2], r4 = m$r[5], r8 = m$r[9], r12 = m$r[13],
             distance = obj(z, J0))
}))
cat("\n== Holding kappa*eta at its estimate ==\n"); print(KE, digits = 3)

# ---- I3. Profile over omega ------------------------------------------------------
# The impulse responses say little about omega, which is why it is fixed at the
# estimate from the dynamic rent equation. Same warm starts as in I1.
gw <- sort(unique(c(0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, OMEGA, 0.4, 0.5, 0.65, 0.8, 1)))
nw <- length(gw)
resw <- lapply(gw, function(w) fit_best(basins(th), J0, omega = w))
for (pass in 1:2) {
  for (i in 2:nw)       resw[[i]] <- better(resw[[i]], fit_best(list(resw[[i - 1]]$par), J0, omega = gw[i]))
  for (i in (nw - 1):1) resw[[i]] <- better(resw[[i]], fit_best(list(resw[[i + 1]]$par), J0, omega = gw[i]))
}
PW <- data.frame(omega = gw, distance = sapply(resw, `[[`, "value"),
                 theta = sapply(resw, function(r) r$par["theta"]),
                 kappa = sapply(resw, function(r) r$par["kappa"]), eta = sapply(resw, function(r) r$par["eta"]))
cat("\n== I3 profile over omega ==\n"); print(PW, digits = 3, row.names = FALSE)
write_csv(PW, file.path(OUTDIR, "model_profile_omega.csv"))
sink()

# ---- Figure: profiles -------------------------------------------------------------
lab <- c(theta = "Speed of price adjustment, \u03b8", kappa = "Speed of tenure switching, \u03ba",
         eta = "Tenure response to prices, \u03b7")
P$panel <- factor(lab[P$param], levels = lab)
est_pts <- data.frame(panel = factor(lab, levels = lab), value = th[c("theta", "kappa", "eta")])
g <- ggplot(P, aes(value, excess)) +
  geom_line(colour = "#1f4e79") + geom_point(colour = "#1f4e79", size = 1.4) +
  geom_vline(data = est_pts, aes(xintercept = value), linetype = "dashed", colour = "grey40") +
  facet_wrap(~panel, scales = "free_x") +
  labs(x = "Parameter value (held fixed; other six re-estimated)",
       y = "Increase in distance over the minimum") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.spacing.x = grid::unit(1.4, "lines"))
ggsave(file.path(OUTDIR, "fig_model_profile.png"), g, width = 9, height = 3.4, dpi = 300)
