# =============================================================================
# 03_figures.R   (merges old 05_plot_divergence + 07_local_projections,
#                 and runs the Figure 1 transmission diagram if present)
#
# Reads:  clean/panel_combined.rds, output/rolling_financial_r2.csv
# Writes: output/fig_*.png, output/local_projection_irf.{csv,png},
#         and (via figure1_transmission.R) the transmission diagram.
# =============================================================================

source("00_setup.R")
library(ggplot2); library(plm); library(lmtest); library(sandwich)

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
                  legend.position = "bottom", plot.title = element_text(face = "bold")))

panel <- readRDS(file.path(CLEAN, "panel_combined.rds")) |>
  mutate(qn = as.numeric(quarter), city = factor(city, levels = CITIES))

# ---- Divergence figures (old 05) -------------------------------------------
roll <- read_csv(file.path(OUTDIR, "rolling_financial_r2.csv"), show_col_types = FALSE) |>
  pivot_longer(c(price_invR2, rent_invR2), names_to = "equation", values_to = "invR2") |>
  mutate(equation = recode(equation, price_invR2 = "Price equation", rent_invR2 = "Rent equation"))
ggsave(file.path(OUTDIR, "fig_divergence.png"),
       ggplot(roll, aes(qend, invR2, colour = equation)) +
         geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
         geom_line(linewidth = 0.9) +
         scale_colour_manual(values = c("Price equation"="#c0392b","Rent equation"="#2c3e50")) +
         labs(title = "The decoupling, with a mechanism",
              subtitle = "Partial R-squared of investor demand: price vs rent equation, 20-quarter rolling window",
              x = NULL, y = "Partial R-squared of investor lending"),
       width = 9, height = 5, dpi = 150)

ggsave(file.path(OUTDIR, "fig_price_rent_gap.png"),
       ggplot(panel, aes(qn, price_rent, colour = city)) +
         geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) +
         geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
         geom_line(linewidth = 0.7) + scale_colour_manual(values = CITY_COLS) +
         labs(title = "Price-rent gap: prices pulling away from rents",
              subtitle = "prop_price / rents (both 2011-12 = 100); rising = prices outran rents",
              x = NULL, y = "Price / rent"),
       width = 9, height = 5, dpi = 150)

ggsave(file.path(OUTDIR, "fig_real_rent.png"),
       ggplot(panel, aes(qn, real_rent, colour = city)) +
         geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
         geom_line(linewidth = 0.7) + scale_colour_manual(values = CITY_COLS) +
         labs(title = "Wage-adjusted rents (rents relative to wages)",
              subtitle = "rent / WPI; the affordability angle tied to the policy's stated aim",
              x = NULL, y = "Rent / WPI"),
       width = 9, height = 5, dpi = 150)

ggsave(file.path(OUTDIR, "fig_rental_trajectories.png"),
       ggplot(panel, aes(qn, rents, colour = city)) +
         geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
         geom_line(linewidth = 0.7) + scale_colour_manual(values = CITY_COLS) +
         labs(title = "Rental price index by capital city",
              subtitle = "ABS 6401.0; 2011-12 = 100; dashed line marks 2017:Q1",
              x = NULL, y = "Rental price index (2011-12 = 100)"),
       width = 9, height = 5, dpi = 150)
message("Wrote divergence figures to ", OUTDIR, "/")

# ---- Rolling rent-coefficient evolution (main-text evidence for Sec 4.2) ----
# Visualises the time-variation the Chow test detects: the employment coefficient
# strengthens while wage pass-through is noisier. Reads the CSV written by
# 02_estimate.R section 8.1; skips gracefully if absent.
roll_coef_path <- file.path(OUTDIR, "alt_rent_rolling_coef.csv")
if (file.exists(roll_coef_path)) {
  rc <- read_csv(roll_coef_path, show_col_types = FALSE) |>
    select(qend, wpi_coef, emp_coef) |>
    pivot_longer(c(wpi_coef, emp_coef), names_to = "coef", values_to = "value") |>
    mutate(coef = recode(coef, wpi_coef = "Wage growth", emp_coef = "Employment growth"))
  ggsave(file.path(OUTDIR, "fig_rent_rolling_coef.png"),
         ggplot(rc, aes(qend, value, colour = coef)) +
           geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
           geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
           geom_line(linewidth = 0.9) +
           scale_colour_manual(values = c("Wage growth" = "#2c3e50",
                                          "Employment growth" = "#c0392b")) +
           labs(title = "Rent-equation coefficients over time",
                subtitle = "20-quarter rolling within-city estimates; dashed line marks 2017:Q1",
                x = NULL, y = "Coefficient on rent growth"),
         width = 9, height = 5, dpi = 150)
  message("Wrote fig_rent_rolling_coef.png to ", OUTDIR, "/")
} else {
  message("alt_rent_rolling_coef.csv not found; run 02_estimate.R section 8.1 first.")
}


# =============================================================================
# LOCAL PROJECTIONS (old 07): cumulative response of price/rent levels to a
# one-SD investor-credit innovation. pdata.frame built ONCE with all leads as
# columns; the horizon loop is then a pure regression sweep (optimization #6).
# =============================================================================
H <- 12; n_lag <- 4; z90 <- 1.645

dlp <- panel |> arrange(city, quarter) |>
  mutate(l_price = log(prop_price), l_rent = log(rents), l_inv = log(inv_commit)) |>
  group_by(city) |>
  mutate(d_lprice = l_price - dplyr::lag(l_price), d_lrent = l_rent - dplyr::lag(l_rent),
         d_linv = l_inv - dplyr::lag(l_inv), d_cash = cash_rate - dplyr::lag(cash_rate),
         tight_l1 = dplyr::lag(tightness), d_cash_l1 = dplyr::lag(d_cash)) |>
  ungroup()
# lags of shock and outcomes
for (j in 1:n_lag) dlp <- dlp |> group_by(city) |>
  mutate("d_linv_l{j}"   := dplyr::lag(d_linv,   j),
         "d_lprice_l{j}" := dplyr::lag(d_lprice, j),
         "d_lrent_l{j}"  := dplyr::lag(d_lrent,  j)) |> ungroup()
# precompute all leads of the LEVELS for cumulative response y_{t+h} - y_{t-1}
for (h in 0:H) dlp <- dlp |> group_by(city) |>
  mutate("lead_lprice_{h}" := dplyr::lead(l_price, h) - dplyr::lag(l_price, 1),
         "lead_lrent_{h}"  := dplyr::lead(l_rent,  h) - dplyr::lag(l_rent,  1)) |> ungroup()

dlp$qid <- as.integer(factor(dlp$quarter))
sd_shock <- sd(dlp$d_linv, na.rm = TRUE)
ctrl_common <- c(paste0("d_linv_l", 1:n_lag), "tight_l1", "d_cash_l1")

run_lp <- function(prefix, growth, label) {
  ctrls <- c(ctrl_common, paste0(growth, "_l", 1:n_lag))
  do.call(rbind, lapply(0:H, function(h) {
    fml <- as.formula(paste0("`lead_", prefix, "_", h, "` ~ d_linv + ",
                             paste(ctrls, collapse = " + ")))
    P <- pdata.frame(dlp, index = c("city","qid"))
    m <- plm(fml, data = P, model = "within")
    data.frame(outcome = label, h = h,
               beta = coef(m)["d_linv"] * sd_shock,
               se   = dk_se(m)["d_linv"] * sd_shock)
  }))
}
irf <- rbind(run_lp("lprice", "d_lprice", "Price"),
             run_lp("lrent",  "d_lrent",  "Rent"))
irf$lo <- irf$beta - z90 * irf$se; irf$hi <- irf$beta + z90 * irf$se
write_csv(irf, file.path(OUTDIR, "local_projection_irf.csv"))
print(irf, digits = 3, row.names = FALSE)

irf$outcome <- factor(irf$outcome, levels = c("Price","Rent"))
pal <- c(Price = "#c0392b", Rent = "#1b6f7a")
ggsave(file.path(OUTDIR, "local_projection_irf.png"),
       ggplot(irf, aes(h, beta, colour = outcome, fill = outcome)) +
         geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.22, colour = NA) +
         geom_line(linewidth = 1.0) +
         scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
         scale_x_continuous(breaks = 0:H) +
         labs(title = "Response to a one-SD investor-credit innovation",
              subtitle = "Cumulative response of the log level; 90% Driscoll-Kraay bands",
              x = "Quarters after the innovation", y = "Log points"),
       width = 8, height = 5, dpi = 150)
message("Wrote local_projection_irf.{csv,png} to ", OUTDIR, "/")

# ---- Appendix: LP lag-length sensitivity of the RENT response ---------------
# Overlays the rent IRF estimated with 2, 4, and 6 control lags, showing the
# shape and h=12 magnitude are robust even though the first-significant horizon
# is not. Reuses the lead columns already built on dlp.
rent_irf_nlag <- function(nlag) {
  ctrls <- c(paste0("d_linv_l", 1:nlag), "tight_l1", "d_cash_l1",
             paste0("d_lrent_l", 1:nlag))
  # Lags beyond those built in the main LP section (4) may not exist; build any
  # missing d_linv AND d_lrent lag columns now (both are needed as controls).
  for (j in 1:nlag) {
    if (!paste0("d_linv_l", j) %in% names(dlp))
      dlp[[paste0("d_linv_l", j)]] <<- ave(dlp$d_linv, dlp$city,
                                           FUN = function(v) dplyr::lag(v, j))
    if (!paste0("d_lrent_l", j) %in% names(dlp))
      dlp[[paste0("d_lrent_l", j)]] <<- ave(dlp$d_lrent, dlp$city,
                                            FUN = function(v) dplyr::lag(v, j))
  }
  P <- pdata.frame(dlp, index = c("city","qid"))
  out <- do.call(rbind, lapply(0:H, function(h) {
    fml <- as.formula(paste0("`lead_lrent_", h, "` ~ d_linv + ",
                             paste(ctrls, collapse = " + ")))
    m <- try(plm(fml, data = P, model = "within"), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)
    data.frame(nlag = paste0(nlag, " lags"), h = h,
               beta = coef(m)["d_linv"] * sd_shock)
  }))
  if (is.null(out) || nrow(out) == 0)
    message("  LP lag-sensitivity: ", nlag, "-lag spec produced no estimates.")
  out
}
rent_lag_irf <- do.call(rbind, lapply(c(2, 4, 6), rent_irf_nlag))
if (!is.null(rent_lag_irf) && nrow(rent_lag_irf) > 0) {
  write_csv(rent_lag_irf, file.path(OUTDIR, "lp_rent_lag_sensitivity.csv"))
  ggsave(file.path(OUTDIR, "fig_lp_rent_lag_sensitivity.png"),
         ggplot(rent_lag_irf, aes(h, beta, colour = nlag)) +
           geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
           geom_line(linewidth = 0.9) +
           scale_colour_manual(values = c("2 lags" = "#8e44ad", "4 lags" = "#1b6f7a",
                                          "6 lags" = "#e67e22")) +
           scale_x_continuous(breaks = 0:H) +
           labs(title = "Rent response under alternative LP control-lag counts",
                subtitle = "Cumulative rent response to a one-SD credit innovation, log points",
                x = "Quarters after the innovation", y = "Log points"),
         width = 8, height = 5, dpi = 150)
  message("Wrote fig_lp_rent_lag_sensitivity.png to ", OUTDIR, "/")
}


# =============================================================================
# FIGURE 1 transmission diagram: delegate to its self-contained script if present.
# =============================================================================
if (file.exists("figure1_transmission.R")) {
  source("figure1_transmission.R")
  message("Ran figure1_transmission.R")
} else {
  message("figure1_transmission.R not found; skipping the transmission diagram.")
}