# =============================================================================
# 03c_lp_iv_apra.R  --  local projections with APRA measures as instruments
#
# Instruments investor credit in the local projections of 03b_local_projections.R
# with APRA's two investor measures: the investor-lending benchmark (in force
# 2015 Q1 to 2018 Q2) and the interest-only cap (in force 2017 Q2 to 2018 Q4).
#
# Two designs, each with three instruments, used one at a time:
#   national  an indicator that the measure is in force, with city fixed
#             effects. Identification comes from timing alone, so any other
#             national change in those years that moved prices biases it.
#   exposure  each city's 2014 investor share of new lending (before the
#             benchmark) times that indicator, with city and quarter fixed
#             effects. National shocks drop out; identification comes from more
#             exposed cities' credit falling more. With five cities, Sydney's
#             high share carries most of it; the no_sydney sample shows how much.
#   Instruments: any (either measure in force, 2015 Q1 to 2018 Q4), benchmark,
#   and io_cap. One instrument at a time keeps each model exactly identified,
#   so the Anderson-Rubin test is a test of the credit effect alone. With both
#   indicators together, that test also rejects when the two measures imply
#   different effects, which they do.
#
# Why cumulative: the indicators stay on for years, so they move credit in
# many later quarters, not only in quarter t. Each model therefore relates the
# cumulative change in log price or rent from t-1 to t+h to the cumulative
# change in credit over the same quarters (Ramey and Zubairy 2018). The
# coefficient is the change in the outcome per unit of credit change; OLS is
# run in the same form, on the same sample, for comparison.
#
# For each horizon h = 0..12, outcome, design, instrument, credit measure and
# sample (full; without the 2015-16 reclassification quarters; without Sydney):
#   OLS and IV (two-stage least squares) estimates, 90 percent bands
#   first-stage F  Driscoll-Kraay Wald statistic of the instrument; below about
#                  10 the instrument is weak and the IV estimate unreliable
#   AR p-value     Anderson-Rubin test of no credit effect: the instrument's
#                  significance in the reduced form, valid even when it is weak
# Controls, as in 03b: four lags of the credit measure and of the outcome's
# growth, lagged tightness, and (national design only) the lagged cash-rate
# change, which quarter effects absorb in the exposure design.
#
# Reads clean/panel_tests (.rds or .csv), written by 01a_build_test_panel.R.
# Run from the folder that holds clean/ (the repository root):
#   Rscript 03c_lp_iv_apra.R
# It does not need 00_setup.R, and it takes under a minute.
#
# Writes to output/:
#   lpiv_results.csv     every estimate
#   lpiv_summary.txt     first-stage strength and the main responses
#   fig_lpiv_main.png    IV and OLS responses, seasonally adjusted credit growth
#   fig_lpiv_share.png   the same for the change in the investor share
#
# The two-stage least squares and the Driscoll-Kraay covariance (4 lags,
# Bartlett weights) are coded here. For OLS, the covariance matches
# plm::vcovSCC(type = "HC0", maxlag = 4) to rounding.
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("03c_lp_iv_apra")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(zoo)
})
# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- Settings ----------------------------------------------------------------
CLEAN  <- "clean"
OUTDIR <- "output"
dir.create(OUTDIR, showWarnings = FALSE)

H        <- 12
NLAG     <- 4
DK_LAG   <- 4
Z90      <- 1.645
MEASURES <- c(main = "dl_inv_sa", share = "d_inv_share_sa")
LEVELS   <- c(price = "prop_price", rent = "rents")
GROWTH   <- c(price = "dl_price",   rent = "dl_rent")
INSTRUMENTS <- list(national = c(any = "apra_any", benchmark = "apra_inv_on", io_cap = "apra_io_on"),
                    exposure = c(any = "z_any",    benchmark = "z_inv",       io_cap = "z_io"))
EXPOSURE_YEAR <- 2014
SAMPLES <- list(full       = function(d) d,
                no_reclass = function(d) filter(d, flag_reclass == 0),
                no_sydney  = function(d) filter(d, city != "Sydney"))
FIG_SPECS <- c("national / any", "exposure / io_cap")   # design / instrument in the figures

COL_EST <- c(IV = "#4a3aa7", OLS = "#808080")   # OLS is the grey dashed reference

# ---- Data ----------------------------------------------------------------------
read_panel <- function(name) {
  f_rds <- file.path(CLEAN, paste0(name, ".rds"))
  f_csv <- file.path(CLEAN, paste0(name, ".csv"))
  if (file.exists(f_rds)) return(readRDS(f_rds))
  if (file.exists(f_csv))
    return(read_csv(f_csv, show_col_types = FALSE) |> mutate(quarter = as.yearqtr(quarter)))
  stop("Neither ", f_rds, " nor ", f_csv, " was found. Run 01a_build_test_panel.R first.")
}
tp <- read_panel("panel_tests") |> arrange(city, quarter)

need <- c(LEVELS, GROWTH, MEASURES, "apra_inv_on", "apra_io_on", "inv_share_sa",
          "tight_l1", "d_cash_l1", "city", "qid", "year", "flag_reclass",
          paste0(rep(c(MEASURES, GROWTH), each = NLAG), "_l", seq_len(NLAG)))
missing_cols <- setdiff(need, names(tp))
if (length(missing_cols))
  stop("panel_tests lacks: ", paste(missing_cols, collapse = ", "),
       ". Rerun 01a_build_test_panel.R.")

# Exposure: each city's average seasonally adjusted investor share in 2014,
# centred on the five-city mean, times each regime indicator.
expo <- tp |>
  filter(year == EXPOSURE_YEAR) |>
  group_by(city) |>
  summarise(exposure = mean(inv_share_sa), .groups = "drop")
tp <- tp |>
  left_join(expo |> mutate(exposure = exposure - mean(exposure)), by = "city") |>
  mutate(apra_any = as.integer(apra_inv_on == 1 | apra_io_on == 1),
         z_any = exposure * apra_any,
         z_inv = exposure * apra_inv_on,
         z_io  = exposure * apra_io_on)

# Cumulative outcome changes y(t+h) - y(t-1) and cumulative credit changes
# (the sum of the measure from t to t+h), all within city.
for (o in names(LEVELS)) for (h in 0:H) {
  tp <- tp |>
    group_by(city) |>
    mutate("cum_{o}_{h}" := dplyr::lead(log(.data[[LEVELS[[o]]]]), h) -
                            dplyr::lag(log(.data[[LEVELS[[o]]]]), 1)) |>
    ungroup()
}
for (m in names(MEASURES)) {
  x <- MEASURES[[m]]
  tp <- tp |> group_by(city) |> mutate("cx_{m}_0" := .data[[x]]) |> ungroup()
  for (h in seq_len(H)) {
    tp <- tp |>
      group_by(city) |>
      mutate("cx_{m}_{h}" := .data[[paste0("cx_", m, "_", h - 1)]] + dplyr::lead(.data[[x]], h)) |>
      ungroup()
  }
}

# ---- Estimation helpers ----------------------------------------------------------
# Remove city effects (or city and quarter effects) from every column.
resid_fe <- function(M, city, time, two_way) {
  if (!two_way) return(M - apply(M, 2, function(v) ave(v, city)))
  qr.resid(qr(model.matrix(~ factor(city) + factor(time))), M)
}

# Driscoll-Kraay "meat": quarterly sums of score contributions, Bartlett weights.
dk_meat <- function(Xs, u, time, L = DK_LAG) {
  Hs <- rowsum(Xs * as.vector(u), time, reorder = TRUE)
  S  <- crossprod(Hs)
  for (l in seq_len(min(L, nrow(Hs) - 1))) {
    G <- crossprod(Hs[(l + 1):nrow(Hs), , drop = FALSE], Hs[1:(nrow(Hs) - l), , drop = FALSE])
    S <- S + (1 - l / (L + 1)) * (G + t(G))
  }
  S
}

ols <- function(y, X, time) {
  XtXi <- solve(crossprod(X))
  b <- drop(XtXi %*% crossprod(X, y)); u <- y - X %*% b
  list(b = b, V = XtXi %*% dk_meat(X, u, time) %*% XtXi)
}

tsls <- function(y, X, Zfull, time) {
  Xh   <- Zfull %*% solve(crossprod(Zfull), crossprod(Zfull, X))   # fitted regressors
  XhXi <- solve(crossprod(Xh))
  b <- drop(XhXi %*% crossprod(Xh, y)); u <- y - X %*% b          # residuals use actual X
  list(b = b, V = XhXi %*% dk_meat(Xh, u, time) %*% XhXi)
}

t_stat <- function(fit, term) fit$b[[term]] / sqrt(fit$V[term, term])

fit_spec <- function(d, design, inst, measure, outcome, h) {
  x_now <- MEASURES[[measure]]
  endo  <- paste0("cx_", measure, "_", h)
  y     <- paste0("cum_", outcome, "_", h)
  ctrl  <- c(paste0(x_now, "_l", seq_len(NLAG)),
             paste0(GROWTH[[outcome]], "_l", seq_len(NLAG)), "tight_l1")
  if (design == "national") ctrl <- c(ctrl, "d_cash_l1")
  cols <- unique(c(y, endo, ctrl, inst))
  d <- d[stats::complete.cases(d[, cols]), ]
  R <- resid_fe(as.matrix(d[, cols]), d$city, d$qid, two_way = design == "exposure")
  time <- d$qid
  yv <- R[, y]
  X  <- R[, c(endo, ctrl), drop = FALSE]
  Zf <- R[, c(inst, ctrl), drop = FALSE]

  f_ols <- ols(yv, X, time)
  f_fs  <- ols(R[, endo], Zf, time)    # first stage
  f_rf  <- ols(yv, Zf, time)           # reduced form
  f_iv  <- tsls(yv, X, Zf, time)
  tibble(n = nrow(d), quarters = length(unique(time)),
         ols_b = f_ols$b[[endo]], ols_se = sqrt(f_ols$V[endo, endo]),
         iv_b  = f_iv$b[[endo]],  iv_se  = sqrt(f_iv$V[endo, endo]),
         fs_coef = f_fs$b[[inst]],
         fs_F  = t_stat(f_fs, inst)^2,
         ar_p  = pchisq(t_stat(f_rf, inst)^2, 1, lower.tail = FALSE))
}

# ---- Run ---------------------------------------------------------------------------
message("Local projections with APRA instruments ...")
grid <- bind_rows(lapply(names(INSTRUMENTS), function(des)
  expand_grid(design = des, instrument = names(INSTRUMENTS[[des]]), measure = names(MEASURES),
              outcome = names(LEVELS), sample = names(SAMPLES), h = 0:H)))
res <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  out <- tryCatch(fit_spec(SAMPLES[[g$sample]](tp), g$design,
                           INSTRUMENTS[[g$design]][[g$instrument]],
                           g$measure, g$outcome, g$h),
                  error = function(e) {
                    message("  skipped (", paste(g, collapse = "/"), "): ", conditionMessage(e))
                    NULL
                  })
  if (is.null(out)) return(NULL)
  bind_cols(g, out)
})) |>
  mutate(iv_lo = iv_b - Z90 * iv_se, iv_hi = iv_b + Z90 * iv_se,
         ols_lo = ols_b - Z90 * ols_se, ols_hi = ols_b + Z90 * ols_se,
         weak = fs_F < 10)
write_csv(res, file.path(OUTDIR, "lpiv_results.csv"))

# ---- Summary -----------------------------------------------------------------------
cell <- function(b, lo, hi, p, weak)
  sprintf("%.3f [%.3f, %.3f] p %.3f%s", b, lo, hi, p, ifelse(weak, " weak", ""))

# The sink starts inside tryCatch, so source(echo = TRUE) does not copy the
# code into the summary.
con <- file(file.path(OUTDIR, "lpiv_summary.txt"), open = "wt")
old_width <- options(width = 250)
tryCatch({
  sink(con, split = TRUE)
  cat("Local projections with APRA instruments,", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("Cumulative change in log price or rent from t-1 to t+h per unit of cumulative credit change\n",
      "over the same quarters (main = log change in SA investor commitments; share = change in the\n",
      "SA investor share, as a fraction). 90 percent Driscoll-Kraay bands; p = Anderson-Rubin p-value\n",
      "for no credit effect; 'weak' = first-stage F below 10.\n",
      "Exposure, the 2014 investor share before centring: ",
      paste(sprintf("%s %.3f", expo$city, expo$exposure), collapse = ", "), "\n", sep = "")

  cat("\n1. First-stage F by horizon (full sample, price equation)\n")
  t1 <- res |>
    filter(sample == "full", outcome == "price", h %in% c(0, 4, 8, 12)) |>
    transmute(design, instrument, measure, h = paste0("F_h", h),
              cell = sprintf("%.1f", fs_F)) |>
    pivot_wider(names_from = h, values_from = cell)
  print(as.data.frame(t1), row.names = FALSE)

  cat("\n2. IV responses (full sample)\n")
  t2 <- res |>
    filter(sample == "full", h %in% c(4, 8, 12)) |>
    transmute(design, instrument, measure, outcome, h = paste0("h", h),
              cell = cell(iv_b, iv_lo, iv_hi, ar_p, weak)) |>
    pivot_wider(names_from = h, values_from = cell)
  print(as.data.frame(t2), row.names = FALSE)

  cat("\n3. OLS in the same form (full sample, national design's controls)\n")
  t3 <- res |>
    filter(sample == "full", design == "national", instrument == "any", h %in% c(4, 8, 12)) |>
    transmute(measure, outcome, h = paste0("h", h),
              cell = sprintf("%.3f [%.3f, %.3f]", ols_b, ols_lo, ols_hi)) |>
    pivot_wider(names_from = h, values_from = cell)
  print(as.data.frame(t3), row.names = FALSE)

  cat("\n4. Checks on the figure's designs, main measure: IV and first-stage F by sample\n")
  t4 <- res |>
    filter(paste(design, "/", instrument) %in% FIG_SPECS, measure == "main", h %in% c(4, 8, 12)) |>
    transmute(spec = paste(design, "/", instrument), outcome, sample, h = paste0("h", h),
              cell = sprintf("%.3f (p %.3f, F %.1f)", iv_b, ar_p, fs_F)) |>
    pivot_wider(names_from = h, values_from = cell) |>
    arrange(spec, outcome, sample)
  print(as.data.frame(t4), row.names = FALSE)
}, finally = {
  sink()
  close(con)
  options(old_width)
})

# ---- Figures ---------------------------------------------------------------------
# One figure per measure: the IV response (line and 90 percent band) and OLS
# (dashed), full sample, for the two designs in FIG_SPECS. No title: the
# caption carries it. Suggested caption: "Cumulative change in the log price
# and log rent levels per unit of cumulative change in investor credit,
# instrumented with APRA's investor measures. Left: indicator that either
# measure is in force, city fixed effects. Right: 2014 investor share times
# the interest-only cap indicator, city and quarter fixed effects. 90 percent
# Driscoll-Kraay bands; dashed lines show OLS."
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("ggplot2 is not installed; skipping the figures.")
} else {
  library(ggplot2)
  for (m in names(MEASURES)) {
    d <- res |>
      filter(measure == m, sample == "full", paste(design, "/", instrument) %in% FIG_SPECS) |>
      mutate(panel = paste0(ifelse(outcome == "price", "Prices", "Rents"), ": ",
                            ifelse(design == "national", "national timing, either measure",
                                   "exposure \u00d7 interest-only cap")))
    d$panel <- factor(d$panel, levels = unique(d$panel[order(d$outcome != "price", d$design != "national")]))
    long <- bind_rows(d |> transmute(panel, h, est = "IV", beta = iv_b),
                      d |> transmute(panel, h, est = "OLS", beta = ols_b))
    p <- ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
      geom_ribbon(data = d, aes(h, ymin = iv_lo, ymax = iv_hi), fill = COL_EST[["IV"]], alpha = 0.15) +
      geom_line(data = long, aes(h, beta, colour = est, linetype = est), linewidth = 0.8) +
      facet_wrap(~ panel, ncol = 2, scales = "free_y") +
      scale_colour_manual(values = COL_EST) +
      scale_linetype_manual(values = c(IV = "solid", OLS = "dashed")) +
      scale_x_continuous(breaks = seq(0, H, 2)) +
      labs(x = expression("Horizon " * italic(h) * " (quarters)"),
           y = "Change per unit of cumulative credit growth",
           colour = NULL, linetype = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
            strip.text = element_text(colour = "grey20", hjust = 0),
            legend.position = "bottom",
            axis.title = element_text(colour = "grey25"), axis.text = element_text(colour = "grey35"))
    ggsave(file.path(OUTDIR, paste0("fig_lpiv_", m, ".png")), p, width = 8, height = 6, dpi = 300)
  }
  message("Wrote fig_lpiv_main.png and fig_lpiv_share.png to ", OUTDIR, "/")
}

end_run_log()
