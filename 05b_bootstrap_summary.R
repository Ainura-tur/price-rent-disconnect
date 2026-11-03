# =============================================================================
# 05b_bootstrap_summary.R -- combine bootstrap draws; intervals, bands, figures
#
# Reads output/boot_b<block>_*.rds from 05_bootstrap.R and writes
#   output/boot_summary.csv          parameter and derived-quantity intervals
#   output/boot_shares.csv           shares of draws and intervals by block length
#   output/fig_model_fit.png         model IRFs with bootstrap bands
#   output/fig_model_counterfactual.png  counterfactual with bootstrap bands
# =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({ library(ggplot2) })

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

load_block <- function(bl) {
  f <- list.files(OUTDIR, pattern = sprintf("^boot_b%d_\\d+_\\d+\\.rds$", bl), full.names = TRUE)
  if (!length(f)) return(NULL)
  do.call(rbind, lapply(f, readRDS))
}
q90 <- function(x) quantile(x, c(0.05, 0.95), na.rm = TRUE)

est   <- read_csv(file.path(OUTDIR, "model_estimates.csv"), show_col_types = FALSE)
fit   <- read_csv(file.path(OUTDIR, "model_fit.csv"), show_col_types = FALSE)
cfact <- read_csv(file.path(OUTDIR, "model_counterfactual.csv"), show_col_types = FALSE)
dec   <- read_csv(file.path(OUTDIR, "model_decomposition.csv"), show_col_types = FALSE)

pn <- c("eps", "rho_g", "rho_d", "theta", "kappa", "eta", "chi")
derived <- c("omega", "share_tenure", "share_lease", "wedge12", "rent_price_12", "mr4", "mr8", "mr12")
rent_at <- function(h) fit$model[fit$outcome == "Rent" & fit$h == h]
point <- c(setNames(est$estimate, pn),
           omega = read_csv(file.path(OUTDIR, "sa_B_omega.csv"), show_col_types = FALSE)$omega,
           share_tenure = 1 - dec$wedge12[grepl("Tenure", dec$case)] / dec$wedge12[dec$case == "Estimated model"],
           share_lease  = 1 - dec$wedge12[grepl("Leases", dec$case)] / dec$wedge12[dec$case == "Estimated model"],
           wedge12 = dec$wedge12[dec$case == "Estimated model"],
           rent_price_12 = rent_at(12) / fit$model[fit$outcome == "Price" & fit$h == 12],
           mr4 = rent_at(4), mr8 = rent_at(8), mr12 = rent_at(12))

summ <- bind_rows(lapply(c(4, 8, 12), function(bl) {
  B <- load_block(bl); if (is.null(B)) return(NULL)
  colnames(B)[colnames(B) == "omega.dr_l1"] <- "omega"
  bind_rows(lapply(c(pn, derived), function(v) {
    x <- B[, v]; qq <- q90(x)
    data.frame(block = bl, draws = nrow(B), quantity = v, point = unname(point[v]),
               lo90 = unname(qq[1]), hi90 = unname(qq[2]), sd = sd(x, na.rm = TRUE),
               share_theta_lt_1 = if (v == "theta") mean(x < 0.95) else NA)
  }))
}))
print(summ, digits = 3, row.names = FALSE)
write_csv(summ, file.path(OUTDIR, "boot_summary.csv"))

# ---- Bands on the model IRFs and counterfactual (block 8) ----------------------
B <- load_block(8)
band <- function(prefix, n) t(apply(B[, paste0(prefix, 0:(n - 1)), drop = FALSE], 2, q90))
bp <- band("mp", 13); br <- band("mr", 13)
fit2 <- fit |> arrange(outcome, h) |>
  mutate(mlo = ifelse(outcome == "Price", bp[h + 1, 1], ifelse(outcome == "Rent", br[h + 1, 1], NA)),
         mhi = ifelse(outcome == "Price", bp[h + 1, 2], ifelse(outcome == "Rent", br[h + 1, 2], NA))) |>
  filter(outcome != "Credit")
pal <- c(Price = "#c0392b", Rent = "#1b6f7a")
ggsave(file.path(OUTDIR, "fig_model_fit.png"),
  ggplot(fit2, aes(h, colour = outcome, fill = outcome)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_ribbon(aes(ymin = data - 1.645 * se, ymax = data + 1.645 * se), alpha = 0.12, colour = NA) +
    geom_errorbar(aes(ymin = mlo, ymax = mhi), width = 0.25, linewidth = 0.5) +
    geom_line(aes(y = data), linewidth = 0.9) +
    geom_line(aes(y = model), linewidth = 0.9, linetype = "dashed") +
    geom_point(aes(y = model), size = 1.4) +
    scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
    scale_x_continuous(breaks = 0:12) +
    labs(x = "Quarters after the innovation", y = "Log points", colour = NULL, fill = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom"),
  width = 8, height = 4.6, dpi = 300)

cp <- band("cfp", 29); cr <- band("cfr", 29)
cf2 <- bind_rows(
  data.frame(h = 0:28, outcome = "Price", value = cfact$price, lo = cp[, 1], hi = cp[, 2]),
  data.frame(h = 0:28, outcome = "Rent",  value = cfact$rent,  lo = cr[, 1], hi = cr[, 2]))
ggsave(file.path(OUTDIR, "fig_model_counterfactual.png"),
  ggplot(cf2, aes(h, value, colour = outcome, fill = outcome)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    annotate("rect", xmin = 0, xmax = 15, ymin = -Inf, ymax = Inf, alpha = 0.06) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
    scale_x_continuous(breaks = seq(0, 28, 4)) +
    labs(x = "Quarters after the tightening begins", y = "Log points", colour = NULL, fill = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom"),
  width = 8, height = 4.2, dpi = 300)

# Counterfactual summary statistics by draw
cf_stats <- function(B) {
  cfs <- data.frame(
    price_low = apply(B[, paste0("cfp", 0:28)], 1, min),
    rent_low  = apply(B[, paste0("cfr", 0:28)], 1, min),
    price_low_q = apply(B[, paste0("cfp", 0:28)], 1, which.min) - 1,
    rent_low_q  = apply(B[, paste0("cfr", 0:28)], 1, which.min) - 1)
  cfs$ratio <- cfs$rent_low / cfs$price_low
  cfs
}
cfs <- cf_stats(B)
cat("\nCounterfactual, 90% intervals across draws:\n")
print(sapply(cfs, q90), digits = 3)
cat("Share of draws with the rent low point later than the price low point:",
    mean(cfs$rent_low_q > cfs$price_low_q), "\n")
cat("Share of draws in which removing tenure lowers the wedge more than removing leases:",
    mean(B[, "share_tenure"] > B[, "share_lease"]), "\n")
write_csv(cfs, file.path(OUTDIR, "boot_counterfactual_draws.csv"))

# Shares of draws and intervals by block length (the numbers quoted in Section 6)
shares <- bind_rows(lapply(c(4, 8, 12), function(bl) {
  Bb <- load_block(bl); if (is.null(Bb)) return(NULL)
  cc <- cf_stats(Bb); th <- Bb[, "theta"]; k <- Bb[, "kappa"]; e <- Bb[, "eta"]; ke <- k * e
  half <- log(0.5) / log(1 - th)
  data.frame(block = bl, draws = nrow(Bb),
             theta_lo = q90(th)[1], theta_hi = q90(th)[2], theta_max = max(th),
             halflife_lo = q90(half)[1], halflife_hi = q90(half)[2],
             kappa_eta_lo = q90(ke)[1], kappa_eta_median = median(ke), kappa_eta_hi = q90(ke)[2],
             kappa_median = median(k), eta_median = median(e),
             corr_log_kappa_eta = cor(log(k), log(pmax(e, 1e-4))), corr_kappa_eta = cor(k, e),
             kappa_at_upper = mean(k > 0.999), kappa_at_lower = mean(k < 0.021),
             tenure_above_lease = mean(Bb[, "share_tenure"] > Bb[, "share_lease"]),
             rent_low_later = mean(cc$rent_low_q > cc$price_low_q),
             chi_at_zero = mean(Bb[, "chi"] < 1e-4),
             price_low_lo = q90(cc$price_low)[1], price_low_hi = q90(cc$price_low)[2],
             rent_low_lo = q90(cc$rent_low)[1], rent_low_hi = q90(cc$rent_low)[2],
             price_low_q_lo = q90(cc$price_low_q)[1], price_low_q_hi = q90(cc$price_low_q)[2],
             rent_low_q_lo = q90(cc$rent_low_q)[1], rent_low_q_hi = q90(cc$rent_low_q)[2],
             row.names = NULL)
}))
print(shares, digits = 3, row.names = FALSE)
write_csv(shares, file.path(OUTDIR, "boot_shares.csv"))
