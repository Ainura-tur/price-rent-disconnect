# =============================================================================
# 05c_small_sample.R -- small-sample behaviour of the impulse-response matching
#
# A recentred-bootstrap Monte Carlo. The estimated model is taken as the truth.
# Each pseudo-sample adds to the model's impulse responses the sampling error of
# one block-bootstrap draw (that draw's local projections less the full-sample
# projections), so the pseudo-samples carry the size and the cross-horizon
# correlation of the sampling error in this 74-quarter, five-city panel. Each
# draw's estimate of omega from the dynamic rent equation is used as is. The
# model is then re-estimated as in 05_bootstrap.R (bounded Gauss-Newton search
# from the true values and nine other starting points), and the estimates are
# compared with the truth.
#
# Reads:  output/model_estimates.csv, output/sa_C_lp.csv, output/sa_B_omega.csv,
#         output/boot_b8_*.rds (05_bootstrap.R)
# Writes: output/model_small_sample.txt, output/model_small_sample.csv,
#         output/mc_small_*.rds (chunks of 25 pseudo-samples)
# =============================================================================

source("04_model_matching.R")   # model, targets (m_data, W), estimates (th, J0, lo, hi)
suppressPackageStartupMessages(library(parallel))

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

f <- list.files(OUTDIR, pattern = "^boot_b8_\\d+_\\d+\\.rds$", full.names = TRUE)
f <- f[order(as.integer(sub(".*_b8_(\\d+)_.*", "\\1", f)))]   # by draw number, not as text
B <- do.call(rbind, lapply(f, readRDS))
dev <- B[, paste0("data", seq_along(m_data))] - matrix(m_data, nrow(B), length(m_data), byrow = TRUE)
om  <- B[, "omega.dr_l1"]

truth <- th
mt <- model_irf(truth, J0)
m_true <- c(mt$p, mt$r, mt$c)
wedge <- function(z, omega, kappa = z["kappa"]) {
  m <- model_irf(z, J0, omega = omega, kappa_override = kappa); m$p[13] - m$r[13]
}
share <- function(z, omega) {
  w0 <- wedge(z, omega)
  c(tenure = 1 - wedge(z, omega, kappa = 1) / w0,
    lease  = 1 - { m1 <- model_irf(z, J0, omega = 1); m1$p[13] - m1$r[13] } / w0)
}
true_sh <- share(truth, OMEGA)

M_FULL <- m_data
one <- function(i) {
  # fit_ls() in 04_model_matching.R matches the global m_data; point it at this
  # pseudo-sample and restore it on exit, because on Windows every pseudo-sample
  # runs in this one process.
  on.exit(m_data <<- M_FULL)
  m_data <<- m_true + dev[i, ]
  best <- fit_best(basins(truth), J0, omega = om[i])
  z <- best$par; names(z) <- pn
  mi <- model_irf(z, J0, omega = om[i])
  c(z, omega = om[i], objective = best$value, share(z, om[i]),
    rent12 = mi$r[13], wedge12 = mi$p[13] - mi$r[13])
}
# The first N_MC draws, in chunks of 25 saved as they finish, so an interrupted
# run resumes where it stopped.
N_MC <- min(200, nrow(B)); CH <- 25
for (a in seq(1, N_MC, by = CH)) {
  z <- min(a + CH - 1, N_MC)
  fch <- file.path(OUTDIR, sprintf("mc_small_%d_%d.rds", a, z))
  if (file.exists(fch)) next
  t0 <- Sys.time()
  r <- mclapply(a:z, function(i) tryCatch(one(i), error = function(e) NULL),
                mc.cores = if (.Platform$OS.type == "windows") 1L else detectCores())
  saveRDS(do.call(rbind, r[!sapply(r, is.null)]), fch)
  cat(sprintf("pseudo-samples %d-%d done in %.1f min\n", a, z,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
R <- as.data.frame(do.call(rbind, lapply(file.path(OUTDIR, sprintf("mc_small_%d_%d.rds",
       seq(1, N_MC, by = CH), pmin(seq(1, N_MC, by = CH) + CH - 1, N_MC))), readRDS)))
write_csv(R, file.path(OUTDIR, "model_small_sample.csv"))

sink(file.path(OUTDIR, "model_small_sample.txt"), split = TRUE)
cat("== Recentred-bootstrap Monte Carlo:", nrow(R), "pseudo-samples (blocks of eight) ==\n")
cat(sprintf("Truth: theta %.3f kappa %.3f eta %.3f; tenure share %.2f, lease share %.2f\n",
            truth["theta"], truth["kappa"], truth["eta"], true_sh["tenure"], true_sh["lease"]))
summ <- function(x, t) c(truth = t, median = median(x), bias_median = median(x) - t,
                         lo5 = unname(quantile(x, .05)), hi95 = unname(quantile(x, .95)),
                         rmse = sqrt(mean((x - t)^2)))
tab <- rbind(theta = summ(R$theta, truth["theta"]), kappa = summ(R$kappa, truth["kappa"]),
             eta = summ(R$eta, truth["eta"]), kappa_eta = summ(R$kappa * R$eta, truth["kappa"] * truth["eta"]),
             tenure = summ(R$tenure, true_sh["tenure"]), lease = summ(R$lease, true_sh["lease"]))
print(round(tab, 3))
cat(sprintf("Share of pseudo-samples with |theta - truth| < 0.10: %.2f\n", mean(abs(R$theta - truth["theta"]) < 0.10)))
cat(sprintf("Share with theta above 0.5: %.3f; max theta %.3f\n", mean(R$theta > 0.5), max(R$theta)))
cat(sprintf("Share with tenure share above lease share (true: %s): %.2f\n",
            true_sh["tenure"] > true_sh["lease"], mean(R$tenure > R$lease)))
cat(sprintf("Share with kappa at a bound: %.2f; with chi at zero: %.2f\n",
            mean(R$kappa < 0.021 | R$kappa > 0.999), mean(R$chi < 1e-6)))
sink()
