# =============================================================================
# 03_figures.R   (merges old 05_plot_divergence + 07_local_projections,
#                 and draws the Figure 1 transmission diagram)
#
# Reads:  clean/panel_combined.rds, output/rolling_financial_r2.csv
# Writes: output/fig_*.png, output/local_projection_irf.{csv,png},
#         output/fig_transmission.{pdf,png} (Figure 1; needs no data).
# =============================================================================

source("00_setup.R")
library(ggplot2); library(plm); library(lmtest); library(sandwich)

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
                  legend.position = "bottom",
                  # Titles belong in the paper's captions, not on the figures.
                  plot.title = element_blank(), plot.subtitle = element_blank()))

panel <- readRDS(file.path(CLEAN, "panel_combined.rds")) |>
  mutate(qn = as.numeric(quarter), city = factor(city, levels = CITIES))

# ---- Divergence figures (old 05) -------------------------------------------
# Figures 2-4 and E.2. The paper shows Figures 3, 4 and E.2 about 5 in wide
# (0.8\textwidth) and Figure 2 about 6 in wide, so saving them 6.5 in wide prints
# axis and legend text at 7 to 8 pt; 600 dpi keeps lines sharp at that size.
div_w <- 6.5; div_h <- 3.6; div_dpi <- 600

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
              x = NULL, y = "Partial R-squared of investor credit"),
       width = div_w, height = div_h, dpi = div_dpi)

ggsave(file.path(OUTDIR, "fig_price_rent_gap.png"),
       ggplot(panel, aes(qn, price_rent, colour = city)) +
         geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) +
         geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
         geom_line(linewidth = 0.7) + scale_colour_manual(values = CITY_COLS) +
         labs(title = "Price-rent gap: prices pulling away from rents",
              subtitle = "prop_price / rents (both 2011-12 = 100); rising = prices outran rents",
              x = NULL, y = "Price / rent"),
       width = div_w, height = div_h, dpi = div_dpi)

ggsave(file.path(OUTDIR, "fig_real_rent.png"),
       ggplot(panel, aes(qn, real_rent, colour = city)) +
         geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
         geom_line(linewidth = 0.7) + scale_colour_manual(values = CITY_COLS) +
         labs(title = "Wage-adjusted rents (rents relative to wages)",
              subtitle = "rent / WPI; the affordability angle tied to the policy's stated aim",
              x = NULL, y = "Rent / WPI"),
       width = div_w, height = div_h, dpi = div_dpi)

ggsave(file.path(OUTDIR, "fig_rental_trajectories.png"),
       ggplot(panel, aes(qn, rents, colour = city)) +
         geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
         geom_line(linewidth = 0.7) + scale_colour_manual(values = CITY_COLS) +
         labs(title = "Rental price index by capital city",
              subtitle = "ABS 6401.0; 2011-12 = 100; dashed line marks 2017:Q1",
              x = NULL, y = "Rent index (2011\u201312 = 100)"),
       width = div_w, height = div_h, dpi = div_dpi)
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
         width = 9, height = 5, dpi = 300)
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
  mutate(l_price = log(prop_price), l_rent = log(rents), l_inv = log(inv_commit_sa)) |>   # seasonally adjusted credit
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
       width = 8, height = 5, dpi = 300)
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
         width = 8, height = 5, dpi = 300)
  message("Wrote fig_lp_rent_lag_sensitivity.png to ", OUTDIR, "/")
}


# =============================================================================
# FIGURE 1: transmission channels in the model (Section 3). Drawn with ggplot2
# so that every figure in the paper comes from the pipeline; it needs no data.
# Writes output/fig_transmission.pdf, which the paper includes, and a .png
# preview. Labels are plain ASCII, so any PDF device renders them.
# Edge styles: main, secondary, dotted for links that act with a lag, and
# dashed grey for measurement only;
# (+) and (-) give the direction of an effect.
# =============================================================================
local({
  fig_w <- 6.6                                   # inches; the paper scales it to the text width
  xlim  <- c(-2.8, 25.1); ylim <- c(-11.8, 6.8)
  ipu   <- fig_w / diff(xlim)                    # inches per plot unit
  fig_h <- ipu * diff(ylim)
  pt    <- function(points) points / .pt         # font size in points -> ggplot size

  col <- c(price = "#8C2D3A", price_fill = "#F3E3E5", price_node = "#E8B4BB",
           rent  = "#2A7F77", rent_fill  = "#E2F0EE", rent_node  = "#A8D5CE",
           slate = "#5B6770", slate_fill = "#EEF1F2", term = "#2C3E50",
           ink   = "#1F2A30", meas = "#7F8C8D")

  # Rounded rectangle as a polygon, centred on (x, y)
  rrect <- function(x, y, w, h, r = 0.25, n = 8) {
    a  <- seq(0, pi / 2, length.out = n)
    cx <- x + c(1, -1, -1, 1) * (w / 2 - r)
    cy <- y + c(1, 1, -1, -1) * (h / 2 - r)
    do.call(rbind, lapply(1:4, function(k)
      data.frame(x = cx[k] + r * cos((k - 1) * pi / 2 + a),
                 y = cy[k] + r * sin((k - 1) * pi / 2 + a))))
  }
  # cairo_pdf embeds the fonts and prints hyphens as hyphens; pdf() is the fallback.
  use_cairo <- isTRUE(capabilities("cairo"))
  # Width and height of labels in inches, measured with the output device's fonts
  text_in <- function(labels, points, face = "plain") {
    tmp <- tempfile(fileext = ".pdf")
    if (use_cairo) grDevices::cairo_pdf(tmp) else grDevices::pdf(tmp)
    on.exit({ grDevices::dev.off(); unlink(tmp) })
    points <- rep_len(points, length(labels)); face <- rep_len(face, length(labels))
    t(vapply(seq_along(labels), function(i) {
      g <- grid::textGrob(labels[i], gp = grid::gpar(fontsize = points[i], fontface = face[i],
                                                       lineheight = 0.95))
      c(grid::convertWidth(grid::grobWidth(g), "in", TRUE),
        grid::convertHeight(grid::grobHeight(g), "in", TRUE))
    }, numeric(2)))
  }

  # ---- Nodes ------------------------------------------------------------------
  nd <- data.frame(
    id    = c("icredit", "cond", "asset", "prices", "tenure", "rents", "income",
              "invest", "pipe", "compl", "tight", "migr"),
    x     = c(0, 0, 6.0, 11.4, 6.0, 11.4, 11.4, 17.6, 17.6, 17.6, 17.6, 23.2),
    y     = c(4.6, 1.0, 1.0, 1.0, -3.2, -3.2, -6.6, 1.0, -1.3, -3.6, -6.6, -6.6),
    w     = c(4.2, 4.2, 3.4, 3.6, 2.6, 3.6, 4.2, 3.4, 3.4, 3.4, 4.4, 2.6),
    h     = c(1.5, 2.2, 1.6, 1.8, 1.6, 1.8, 1.6, 1.5, 1.5, 1.5, 1.6, 1.5),
    title = c("Investor credit", "Credit conditions", "Asset value", "PRICES",
              "Tenure\nswitching", "RENTS", "Renter income", "Housing\ninvestment",
              "Construction\npipeline", "Dwelling\ncompletions", "Market tightness",
              "Net\nmigration"),
    sub   = c("(measured)", "Loan-to-value limit\nPolicy rate",
              "(investors' valuation)", "(gradual adjustment)", "",
              "(staggered leases)", "(wages, employment)", "", "", "",
              "(migration/completions)", ""),
    border = unname(col[c("meas", "price", "price", "price", "rent", "rent", "rent",
                          "slate", "slate", "slate", "rent", "slate")]),
    fill   = c("white", unname(col[c("price_fill", "price_fill", "price_node",
                                     "rent_fill", "rent_node", "rent_fill",
                                     "slate_fill", "slate_fill", "slate_fill",
                                     "rent_fill", "slate_fill")])),
    lw    = c(0.4, 0.7, 0.45, 0.9, 0.45, 0.9, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45),
    lty   = c("dashed", rep("solid", 11)),
    tsize = c(7.5, 7.5, 7.5, 9.5, 7.5, 9.5, rep(7.5, 6)),
    tface = c("plain", "bold", "plain", "bold", "plain", "bold", rep("plain", 6)),
    stringsAsFactors = FALSE)
  text_w <- pmax(text_in(nd$title, nd$tsize, nd$tface)[, 1],
                  ifelse(nd$sub == "", 0, text_in(nd$sub, 6.5)[, 1])) / ipu
  nd$w <- pmax(nd$w, text_w + 0.45)              # boxes grow to fit wider fonts
  n_sub <- ifelse(nd$sub == "", 0, lengths(strsplit(nd$sub, "\n")))
  nd$ty <- nd$y + ifelse(n_sub == 0, 0, ifelse(n_sub == 2, 0.55, 0.27))   # title
  nd$sy <- nd$y - ifelse(n_sub == 2, 0.28, 0.30)                          # sub-label
  boxes <- do.call(rbind, lapply(seq_len(nrow(nd)), function(i) {
    b <- rrect(nd$x[i], nd$y[i], nd$w[i], nd$h[i])
    cbind(b, nd[i, c("id", "border", "fill", "lw", "lty")], row.names = NULL)
  }))

  # ---- Edges --------------------------------------------------------------------
  at <- function(id, side, f = 0.5) {            # point on a node's border
    n <- nd[nd$id == id, ]
    switch(side,
           l = c(n$x - n$w / 2, n$y - n$h / 2 + f * n$h),
           r = c(n$x + n$w / 2, n$y - n$h / 2 + f * n$h),
           t = c(n$x - n$w / 2 + f * n$w, n$y + n$h / 2),
           b = c(n$x - n$w / 2 + f * n$w, n$y - n$h / 2))
  }
  bez <- function(p0, p1, bend = 0, gap = 0.07, n = 80) {   # quadratic Bezier
    u  <- (p1 - p0) / sqrt(sum((p1 - p0)^2))
    p0 <- p0 + gap * u; p1 <- p1 - gap * u      # stop just short of the boxes
    d  <- p1 - p0; k <- p0 + d / 2 + bend * c(-d[2], d[1])
    t  <- seq(0, 1, length.out = n)
    data.frame(x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * k[1] + t^2 * p1[1],
               y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * k[2] + t^2 * p1[2])
  }
  sty <- data.frame(lw = c(0.95, 0.5, 0.65, 0.45), lty = c("solid", "solid", "13", "44"),
                    row.names = c("main", "sec", "slow", "meas"))
  e <- function(from, fs, to, ts, style, colour, ff = 0.5, tf = 0.5, bend = 0,
                lab = "", lx = NA, ly = NA, hj = 0.5, vj = 0.5, lt = 0.5) {
    p0 <- at(from, fs, ff); p1 <- at(to, ts, tf)
    data.frame(x0 = p0[1], y0 = p0[2], x1 = p1[1], y1 = p1[2], bend, style,
               colour = col[[colour]], lab, lx, ly, hj, vj, lt, stringsAsFactors = FALSE)
  }
  E <- rbind(
    # Credit side: credit conditions move the asset value at once; transaction
    # prices follow gradually (theta); measured credit also contains the price.
    e("cond",   "r", "asset",   "l", "main", "price", lab = "collateral\nchannel",
      lx = 3.2,  ly = 1.2, vj = 0),
    e("asset",  "r", "prices",  "l", "main", "price"),
    e("cond",   "t", "icredit", "b", "meas", "meas"),
    e("prices", "t", "icredit", "r", "meas", "meas", ff = 0.35, bend = -0.18,
      lab = "current price"),
    # Tenure route: switching responds with a lag to prices relative to wages
    # (Equation 4), and the switchers raise renter demand at once.
    e("prices", "b", "tenure",  "t", "slow", "price", ff = 0.2, tf = 0.8,
      lab = "affordability", lx = 8.3, ly = -0.95, hj = 1),
    e("income", "l", "tenure",  "b", "slow", "rent", bend = 0.25, lab = "wages (-)"),
    e("tenure", "r", "rents",   "l", "sec",  "rent", lab = "renter\ndemand (+)",
      lx = 8.45, ly = -3.0, vj = 0),
    e("income", "t", "rents",   "b", "main", "rent", lab = "income",
      lx = 11.6, ly = -4.95, hj = 0),
    # Building route: investment responds at once (Tobin's q), completions arrive
    # after the construction lag, and the route works against the tenure route.
    e("prices", "r", "invest",  "l", "sec",  "price", lab = "Tobin's q",
      lx = 14.55, ly = 1.2, vj = 0),
    e("invest", "b", "pipe",    "t", "sec",  "rent"),
    e("pipe",   "b", "compl",   "t", "slow", "rent", lab = "time to build",
      lx = 17.8, ly = -2.45, hj = 0),
    e("compl",  "b", "tight",   "t", "sec",  "rent", lab = "denominator (-)",
      lx = 17.8, ly = -5.05, hj = 0),
    e("migr",   "l", "tight",   "r", "sec",  "rent", lab = "numerator\n(+)",
      lx = 20.85, ly = -6.45, vj = 0),
    e("tight",  "l", "rents",   "b", "main", "rent", tf = 0.85, bend = 0.3,
      lab = "in levels", lx = 13.4, ly = -4.8, hj = 0)
  )
  paths <- do.call(rbind, lapply(seq_len(nrow(E)), function(i) {
    b <- bez(c(E$x0[i], E$y0[i]), c(E$x1[i], E$y1[i]), E$bend[i])
    cbind(b, edge = i, colour = E$colour[i], sty[E$style[i], ], row.names = NULL)
  }))
  heads <- do.call(rbind, lapply(split(paths, paths$edge), function(p) {
    n <- nrow(p); v <- c(p$x[n] - p$x[n - 1], p$y[n] - p$y[n - 1])
    v <- v / sqrt(sum(v^2))                      # solid stub, so dotted lines get clean heads
    data.frame(x = p$x[n] - 0.02 * v[1], y = p$y[n] - 0.02 * v[2],
               xend = p$x[n], yend = p$y[n], colour = p$colour[1], lw = p$lw[1])
  }))
  for (i in which(E$lab != "" & is.na(E$lx))) {  # unplaced labels sit on their curve
    q <- paths[paths$edge == i, ]; j <- max(1, round(E$lt[i] * nrow(q)))
    E$lx[i] <- q$x[j]; E$ly[i] <- q$y[j]
  }
  labs <- E[E$lab != "", c("lab", "lx", "ly", "hj", "vj", "colour")]
  wh   <- text_in(labs$lab, 6.5) / ipu
  pad  <- 0.06
  labs$xmin <- labs$lx - labs$hj * wh[, 1] - pad; labs$xmax <- labs$xmin + wh[, 1] + 2 * pad
  labs$ymin <- labs$ly - labs$vj * wh[, 2] - pad; labs$ymax <- labs$ymin + wh[, 2] + 2 * pad

  # ---- Bands, bottom box and key ------------------------------------------------
  bands <- data.frame(label  = c("CREDIT", "TRANSMISSION CORE", "SUPPLY & MIGRATION"),
                      x0 = c(-2.5, 3.8, 15.0), x1 = c(2.5, 13.6, 24.8),
                      y0 = c(-0.5, -8.0, -8.0), y1 = c(5.9, 2.4, 2.4),
                      colour = unname(col[c("price", "rent", "slate")]))
  band_poly <- do.call(rbind, lapply(seq_len(nrow(bands)), function(i) {
    b <- bands[i, ]
    cbind(rrect((b$x0 + b$x1) / 2, (b$y0 + b$y1) / 2, b$x1 - b$x0, b$y1 - b$y0, r = 0.35),
          band = i, colour = b$colour, row.names = NULL)
  }))
  disc <- rrect(8.7, -9.6, 14.0, 1.5)
  key  <- data.frame(x = c(-2.3, 3.0, 7.6, 12.2),
                     lab = c("main channel", "secondary", "with a lag", "measurement only"),
                     colour = unname(col[c("ink", "ink", "ink", "meas")]),
                     sty[c("main", "sec", "slow", "meas"), ], row.names = NULL)
  y_key <- -11.1

  p <- ggplot() +
    geom_polygon(data = band_poly, aes(x, y, group = band, colour = colour),
                 fill = NA, linewidth = 0.35, linetype = "33") +
    geom_text(data = bands, aes(x0 + 0.1, y1 + 0.12, label = label, colour = colour),
              hjust = 0, vjust = 0, size = pt(6.5)) +
    geom_polygon(data = boxes, aes(x, y, group = id, fill = fill, colour = border,
                                   linewidth = lw, linetype = lty)) +
    geom_text(data = nd, aes(x, ty, label = title, fontface = tface, size = pt(tsize)),
              colour = col[["ink"]], lineheight = 0.95) +
    geom_text(data = nd[nd$sub != "", ], aes(x, sy, label = sub),
              colour = col[["ink"]], size = pt(6.5), lineheight = 0.95) +
    geom_path(data = paths, aes(x, y, group = edge, colour = colour, linewidth = lw,
                                linetype = lty), lineend = "round") +
    geom_segment(data = heads, aes(x, y, xend = xend, yend = yend, colour = colour,
                                   linewidth = lw),
                 arrow = arrow(angle = 22, length = unit(1.7, "mm"), type = "closed"),
                 linejoin = "mitre") +
    geom_rect(data = labs, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "white", colour = NA) +
    geom_text(data = labs, aes(lx, ly, label = lab, colour = colour, hjust = hj,
                               vjust = vj), size = pt(6.5), lineheight = 0.95) +
    # Bottom box: what the channels imply
    annotate("segment", x = 8.7, y = -8.0, xend = 8.7, yend = -8.8, colour = col[["ink"]],
             linewidth = 0.5, arrow = arrow(angle = 22, length = unit(1.7, "mm"),
                                            type = "closed"), linejoin = "mitre") +
    geom_polygon(data = disc, aes(x, y), fill = col[["term"]], colour = col[["term"]]) +
    annotate("text", x = 8.7, y = -9.32, label = "Price-rent disconnect",
             colour = "white", fontface = "bold", size = pt(7.5)) +
    annotate("text", x = 8.7, y = -9.88, colour = "white", size = pt(6.5),
             label = "Prices move first and build up over several quarters; rents respond later") +
    # Key
    geom_segment(data = key, aes(x = x, y = y_key, xend = x + 1.3, yend = y_key,
                                 colour = colour, linewidth = lw, linetype = lty),
                 lineend = "round") +
    geom_text(data = key, aes(x + 1.5, y_key, label = lab), hjust = 0,
              colour = col[["ink"]], size = pt(6.5)) +
    annotate("text", x = 17.5, y = y_key, hjust = 0, colour = col[["ink"]],
             size = pt(6.5), label = "(+) / (-): direction of the effect") +
    scale_colour_identity() + scale_fill_identity() + scale_size_identity() +
    scale_linewidth_identity() + scale_linetype_identity() +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))

  ggsave(file.path(OUTDIR, "fig_transmission.pdf"), p, width = fig_w, height = fig_h,
         device = if (use_cairo) grDevices::cairo_pdf else "pdf")
  ggsave(file.path(OUTDIR, "fig_transmission.png"), p, width = fig_w, height = fig_h,
         dpi = 300, bg = "white")
})
message("Wrote fig_transmission.{pdf,png} (Figure 1) to ", OUTDIR, "/")