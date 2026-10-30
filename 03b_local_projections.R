# =============================================================================
# 03b_local_projections.R  --  local projections with the adjusted credit series
#
# Re-estimates the paper's panel local projections (the LP section of
# 03_figures.R) with the seasonally adjusted investor-commitment series as the
# main credit measure, as agreed on 8 October 2026. The original series, which
# reproduces the earlier version's figure exactly, and the change in the
# adjusted investor share run alongside.
#
# Reads clean/panel_tests (.rds or .csv), written by 01a_build_test_panel.R.
# Run from the folder that holds clean/ (the repository root):
#   Rscript 03b_local_projections.R
# It does not need 00_setup.R, and it takes under a minute.
#
# Writes to output/:
#   lp_irf_revision.csv            every response: measure, outcome, lags, horizon
#   lp_summary.txt                 responses at 0, 4, 8 and 12 quarters, and when
#                                  the rent response turns significant
#   fig_lp_irf_adjusted.png        price and rent responses, adjusted series;
#                                  replaces local_projection_irf.png
#   fig_lp_rent_lags_adjusted.png  rent response with 2, 4 and 6 control lags;
#                                  replaces fig_lp_rent_lag_sensitivity.png
#
# Specification, as in 03_figures.R, for h = 0, ..., 12:
#   y(i,t+h) - y(i,t-1) = a(i) + b(h) x(i,t)
#                         + sum over j = 1..L of [ c(j) x(i,t-j) + d(j) dy(i,t-j) ]
#                         + e tightness(i,t-1) + f dcash(t-1) + u(i,t+h)
# y is the log price or log rent level, dy its quarterly change, x the credit
# measure, and L = 4 control lags (2 and 6 as checks). City fixed effects and
# Driscoll-Kraay errors with 4 lags. b(h) is scaled to a rise in x of one
# standard deviation of that measure; bands are 90 percent.
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("03b_local_projections")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(zoo); library(plm)
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

H         <- 12               # longest horizon, in quarters
LAGS      <- c(2, 4, 6)       # control lags; 4 is the paper's choice
BASE_LAGS <- 4
Z90       <- 1.645            # 90 percent bands, as in the paper
DK_LAG    <- 4

MEASURES <- c(main = "dl_inv_sa", original = "dl_inv", share = "d_inv_share_sa")
LEVELS   <- c(price = "prop_price", rent = "rents")   # outcome levels
GROWTH   <- c(price = "dl_price",   rent = "dl_rent") # their quarterly changes

# Colours checked for colour-blind separation and contrast on white.
COL_OUTCOME <- c(Price = "#c0392b", Rent = "#2a78d6")
COL_LAGS    <- c(`4 lags` = "#2a78d6", `2 lags` = "#eb6834", `6 lags` = "#1baf7a")

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

need <- c(LEVELS, GROWTH, MEASURES, "tight_l1", "d_cash_l1", "city", "qid",
          paste0(rep(c(MEASURES, GROWTH), each = max(LAGS)), "_l", seq_len(max(LAGS))))
missing_cols <- setdiff(need, names(tp))
if (length(missing_cols))
  stop("panel_tests lacks: ", paste(missing_cols, collapse = ", "),
       ". Rerun 01a_build_test_panel.R.")

# Cumulative changes y(t+h) - y(t-1), built within city.
for (o in names(LEVELS)) for (h in 0:H) {
  tp <- tp |>
    group_by(city) |>
    mutate("cum_{o}_{h}" := dplyr::lead(log(.data[[LEVELS[[o]]]]), h) -
                            dplyr::lag(log(.data[[LEVELS[[o]]]]), 1)) |>
    ungroup()
}

# One standard deviation of each measure, over every quarter it is observed
# (as 03_figures.R does for the original series).
SD_SHOCK <- vapply(MEASURES, function(v) sd(tp[[v]], na.rm = TRUE), numeric(1))

P <- pdata.frame(as.data.frame(tp), index = c("city", "qid"))

# ---- Estimation ----------------------------------------------------------------
lp_fit <- function(outcome, measure, nlag, h) {
  x <- MEASURES[[measure]]; g <- GROWTH[[outcome]]
  ctrls <- c(paste0(x, "_l", seq_len(nlag)), "tight_l1", "d_cash_l1",
             paste0(g, "_l", seq_len(nlag)))
  f <- as.formula(paste0("cum_", outcome, "_", h, " ~ ", x, " + ",
                         paste(ctrls, collapse = " + ")))
  m <- plm(f, data = P, model = "within")
  V <- plm::vcovSCC(m, type = "HC0", maxlag = DK_LAG)
  tibble(beta_raw = coef(m)[[x]], se_raw = sqrt(V[x, x]), n = nobs(m))
}

message("Local projections ...")
grid <- expand_grid(measure = names(MEASURES), outcome = names(LEVELS),
                    lags = LAGS, h = 0:H)
irf <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  bind_cols(g, lp_fit(g$outcome, g$measure, g$lags, g$h))
})) |>
  mutate(sd_shock = SD_SHOCK[measure],
         beta = beta_raw * sd_shock, se = se_raw * sd_shock,
         lo = beta - Z90 * se, hi = beta + Z90 * se,
         significant = lo > 0 | hi < 0)
write_csv(irf, file.path(OUTDIR, "lp_irf_revision.csv"))

# ---- Summary -------------------------------------------------------------------
# first_sig: first horizon from 1 on whose band excludes zero (as in 02_estimate.R
# section 7.5); sig_from: first horizon from which every later band excludes zero.
timing <- irf |>
  filter(h >= 1) |>
  group_by(measure, outcome, lags) |>
  summarise(first_sig = if (any(significant)) min(h[significant]) else NA_integer_,
            sig_from  = {
              s <- significant[order(h)]; hh <- sort(h)
              k <- which(rev(cumprod(rev(s))) == 1)
              if (length(k)) hh[min(k)] else NA_integer_
            },
            .groups = "drop")

cell <- function(b, lo, hi) sprintf("%.4f [%.4f, %.4f]", b, lo, hi)

# The sink starts inside tryCatch, so source(echo = TRUE) does not copy the
# code into the summary.
con <- file(file.path(OUTDIR, "lp_summary.txt"), open = "wt")
old_width <- options(width = 250)
tryCatch({
  sink(con, split = TRUE)
  cat("Local projections,", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("Cumulative response of log price and log rent (log points) to a one-standard-deviation\n",
      "rise in the credit measure; 90 percent Driscoll-Kraay bands in brackets.\n", sep = "")
  cat(sprintf("One standard deviation: main %.4f, original %.4f, share %.4f (0.01 = one point)\n",
              SD_SHOCK[["main"]], SD_SHOCK[["original"]], SD_SHOCK[["share"]]))

  cat("\n1. Responses with", BASE_LAGS, "control lags\n")
  t1 <- irf |>
    filter(lags == BASE_LAGS, h %in% c(0, 4, 8, 12)) |>
    transmute(measure, outcome, h = paste0("h", h), cell = cell(beta, lo, hi)) |>
    pivot_wider(names_from = h, values_from = cell)
  print(as.data.frame(t1), row.names = FALSE)

  cat("\n2. When the response turns significant (horizons from 1)\n")
  cat("   first_sig = first horizon whose band excludes zero; sig_from = from then on, every band does\n")
  t2 <- timing |>
    mutate(lags = paste(lags, "lags")) |>
    transmute(measure, outcome, lags,
              cell = paste0(first_sig, " / ", sig_from)) |>
    pivot_wider(names_from = lags, values_from = cell)
  print(as.data.frame(t2), row.names = FALSE)

  cat("\n3. Rent response at 12 quarters, by control lags\n")
  t3 <- irf |>
    filter(outcome == "rent", h == 12) |>
    transmute(measure, lags = paste(lags, "lags"), cell = cell(beta, lo, hi)) |>
    pivot_wider(names_from = lags, values_from = cell)
  print(as.data.frame(t3), row.names = FALSE)

  cat("\n4. Price minus rent at 12 quarters,", BASE_LAGS, "lags (bands overlap if the gap is not clear)\n")
  t4 <- irf |>
    filter(lags == BASE_LAGS, h == 12) |>
    select(measure, outcome, beta, lo, hi) |>
    pivot_wider(names_from = outcome, values_from = c(beta, lo, hi)) |>
    transmute(measure, gap = sprintf("%.4f", beta_price - beta_rent),
              bands_overlap = ifelse(lo_price <= hi_rent & lo_rent <= hi_price, "yes", "no"))
  print(as.data.frame(t4), row.names = FALSE)
}, finally = {
  sink()
  close(con)
  options(old_width)
})

# ---- Figures ---------------------------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("ggplot2 is not installed; skipping the figures.")
} else {
  library(ggplot2)

  # Spread end-of-line labels so they do not overlap.
  spread_labels <- function(y, min_gap) {
    o <- order(y); ys <- y[o]
    for (k in seq_along(ys)[-1]) ys[k] <- max(ys[k], ys[k - 1] + min_gap)
    out <- y; out[o] <- ys; out
  }

  # The figures carry no title: Elsevier's guides put the title in the caption.
  base_theme <- theme_minimal(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
          legend.title = element_blank(), legend.position = "bottom",
          axis.title = element_text(colour = "grey25"),
          axis.text  = element_text(colour = "grey35"))

  # Figure: price and rent, adjusted series, 4 lags. Suggested caption:
  # "Cumulative response of the log price and log rent levels to a rise of one
  # standard deviation in seasonally adjusted investor commitments, over twelve
  # quarters. Panel local projections with city fixed effects and four control
  # lags; 90 percent Driscoll-Kraay bands."
  d1 <- irf |>
    filter(measure == "main", lags == BASE_LAGS) |>
    mutate(series = factor(ifelse(outcome == "price", "Price", "Rent"), levels = c("Price", "Rent")))
  lab1 <- d1 |> filter(h == H) |>
    mutate(y_lab = spread_labels(beta, 0.06 * diff(range(c(d1$lo, d1$hi)))))
  p1 <- ggplot(d1, aes(h, beta, colour = series, fill = series)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_text(data = lab1, aes(x = H + 0.3, y = y_lab, label = series),
              colour = "grey20", hjust = 0, size = 3.5, inherit.aes = FALSE) +
    scale_colour_manual(values = COL_OUTCOME) + scale_fill_manual(values = COL_OUTCOME) +
    scale_x_continuous(breaks = 0:H, expand = expansion(add = c(0.3, 1.4))) +
    labs(x = "Quarters after the rise", y = "Log points") +
    base_theme
  ggsave(file.path(OUTDIR, "fig_lp_irf_adjusted.png"), p1, width = 8, height = 4.5, dpi = 300)

  # Figure: rent response with 2, 4 and 6 control lags, adjusted series.
  # Suggested caption: "Cumulative response of the log rent level to a rise of
  # one standard deviation in seasonally adjusted investor commitments, with
  # two, four and six control lags in the local projections."
  d2 <- irf |>
    filter(measure == "main", outcome == "rent") |>
    mutate(spec = factor(paste(lags, "lags"), levels = paste(sort(LAGS), "lags")))
  lab2 <- d2 |> filter(h == H) |>
    mutate(y_lab = spread_labels(beta, 0.06 * diff(range(d2$beta))))
  p2 <- ggplot(d2, aes(h, beta, colour = spec)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_line(linewidth = 0.8) +
    geom_text(data = lab2, aes(x = H + 0.3, y = y_lab, label = spec),
              colour = "grey20", hjust = 0, size = 3.5, inherit.aes = FALSE) +
    scale_colour_manual(values = COL_LAGS) +
    scale_x_continuous(breaks = 0:H, expand = expansion(add = c(0.3, 1.8))) +
    labs(x = "Quarters after the rise", y = "Log points") +
    base_theme
  ggsave(file.path(OUTDIR, "fig_lp_rent_lags_adjusted.png"), p2, width = 8, height = 4.5, dpi = 300)
  message("Wrote fig_lp_irf_adjusted.png and fig_lp_rent_lags_adjusted.png to ", OUTDIR, "/")
}

end_run_log()
