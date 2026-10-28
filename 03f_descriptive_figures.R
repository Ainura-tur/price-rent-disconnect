# =============================================================================
# 03f_descriptive_figures.R  --  Figures 2, 3, 4 and C.1, redrawn without titles
#
# 03_figures.R drew these four figures with a title and a working subtitle
# inside each image. Elsevier's guides put the title in the caption, so the
# redrawn figures carry none, and their axis labels use the paper's terms:
#   fig_rental_trajectories.png  Figure 2    rent index by city
#   fig_price_rent_gap.png       Figure 3    price-rent ratio by city
#   fig_real_rent.png            Figure 4    rent index over the wage price index
#   fig_divergence.png           Figure C.1  rolling partial R-squared of
#                                            investor credit, price and rent
#                                            equations (Appendix C)
# The file names match the \includegraphics calls in Rental_disconnect.tex, so
# copying the four PNGs into the paper folder replaces the old figures.
#
# Figure C.1 now uses seasonally adjusted investor credit, the paper's main
# measure; 03_figures.R used the original series. The statistic is otherwise
# that of 02_estimate.R, section 6: in each 20-quarter window, the rise in R^2
# when credit growth joins a within-city regression of price (or rent) growth
# on the cash-rate change, lagged tightness and wage growth, as a share of the
# outcome's total variation in the window. As in 02_estimate.R, growth rates
# are taken within the window, so each city adds 19 quarters to each window.
# rolling_partial_r2.csv holds the values for both credit series.
#
# Reads clean/panel_tests (.rds or .csv), written by 01a_build_test_panel.R.
# Run from the folder that holds clean/ (the repository root):
#   Rscript 03f_descriptive_figures.R
# It does not need 00_setup.R and runs in a few seconds.
#
# Writes to output/:
#   the four PNGs above (8 x 4.5 inches, 300 dpi, as Figure 5)
#   rolling_partial_r2.csv           Figure C.1 values, both credit series
#   descriptive_figures_summary.txt  end values and peaks, for checking the
#                                    text against the figures
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("03f_descriptive_figures")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(zoo); library(ggplot2)
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

CITIES      <- c("Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth")
BREAK_Q     <- as.yearqtr("2017 Q1")     # dashed line in every figure
WIN         <- 20                        # rolling window for Figure C.1, quarters
CREDIT      <- c(main = "dl_inv_sa", original = "dl_inv")
FIG8_CREDIT <- "main"                    # the credit series Figure C.1 plots
                                         # (named when it was Figure 8)
CONTROLS    <- c("d_cash", "tight_l1", "dl_wpi")   # as in 02_estimate.R, section 6
OUTCOMES    <- c("Price equation" = "dl_price", "Rent equation" = "dl_rent")
FIG_W <- 8; FIG_H <- 4.5; FIG_DPI <- 300

# City colours: the 03_figures.R palette with Melbourne moved from slate
# (#2c3e50, which reads as grey and fails the chroma check) to blue. Checked
# with the dataviz palette validator on white: lightness, chroma, colour-blind
# and normal-vision separation pass for neighbouring pairs. Across all pairs,
# violet and blue sit close for colour-blind readers and Perth's orange has
# 2.9:1 contrast, so every line also carries a direct label.
CITY_COLS <- c(Sydney = "#c0392b", Melbourne = "#2a78d6", Brisbane = "#16a085",
               Adelaide = "#8e44ad", Perth = "#e67e22")
# Price and rent take the colours they have in Figure 5.
COL_EQ <- c("Price equation" = "#c0392b", "Rent equation" = "#2a78d6")

# ---- Data ----------------------------------------------------------------------
read_panel <- function(name) {
  f_rds <- file.path(CLEAN, paste0(name, ".rds"))
  f_csv <- file.path(CLEAN, paste0(name, ".csv"))
  if (file.exists(f_rds)) return(readRDS(f_rds))
  if (file.exists(f_csv))
    return(read_csv(f_csv, show_col_types = FALSE) |> mutate(quarter = as.yearqtr(quarter)))
  stop("Neither ", f_rds, " nor ", f_csv, " was found. Run 01a_build_test_panel.R first.")
}
tp <- read_panel("panel_tests")

need <- c("city", "quarter", "rents", "prop_price", "wpi",
          unname(CREDIT), CONTROLS, unname(OUTCOMES))
missing_cols <- setdiff(need, names(tp))
if (length(missing_cols))
  stop("panel_tests lacks: ", paste(missing_cols, collapse = ", "),
       ". Rerun 01a_build_test_panel.R.")

tp <- tp |>
  mutate(quarter    = as.yearqtr(quarter),
         city       = factor(city, levels = CITIES),
         t          = as.numeric(quarter),
         price_rent = prop_price / rents,   # both indices 2011-12 = 100
         rent_wage  = rents / wpi) |>
  arrange(city, quarter)

# ---- Figure C.1 statistic: rolling partial R-squared of investor credit ---------
window_r2 <- function(w, y, x) {
  m1 <- lm(reformulate(c(x, CONTROLS, "city"), y), data = w)
  m0 <- lm(reformulate(c(CONTROLS, "city"), y), data = w)
  tss <- sum((w[[y]] - mean(w[[y]]))^2)
  max((sum(resid(m0)^2) - sum(resid(m1)^2)) / tss, 0)
}

qs <- sort(unique(tp$quarter))
roll <- bind_rows(lapply(qs[qs >= qs[WIN]], function(end) {
  wq <- qs[qs > end - WIN / 4 & qs <= end]
  if (length(wq) < WIN) return(NULL)
  w_all <- tp |> filter(quarter %in% wq[-1])   # growth within the window
  bind_rows(lapply(names(CREDIT), function(m) {
    bind_rows(lapply(names(OUTCOMES), function(eq) {
      vars <- c(OUTCOMES[[eq]], CREDIT[[m]], CONTROLS)
      w <- w_all[complete.cases(w_all[, vars]), ]
      tibble(quarter_end = end, measure = m, equation = eq, n_obs = nrow(w),
             partial_r2 = window_r2(w, OUTCOMES[[eq]], CREDIT[[m]]))
    }))
  }))
}))
write_csv(roll |> mutate(quarter_end = format(quarter_end, "%Y Q%q")),
          file.path(OUTDIR, "rolling_partial_r2.csv"))

# ---- Summary for checking the text --------------------------------------------
# The sink starts inside tryCatch, so source(echo = TRUE) does not copy the
# code into the summary.
con <- file(file.path(OUTDIR, "descriptive_figures_summary.txt"), open = "wt")
tryCatch({
  sink(con)
  cat("Descriptive figures: values for checking the text\n")
  cat("Panel:", format(min(tp$quarter)), "to", format(max(tp$quarter)), "\n\n")

  cat("1. Levels in the last quarter and peaks (Figures 2 to 4)\n")
  t1 <- tp |>
    group_by(city) |>
    summarise(rent_end        = rents[which.max(t)],
              ratio_end       = price_rent[which.max(t)],
              ratio_peak      = max(price_rent, na.rm = TRUE),
              ratio_peak_q    = format(quarter[which.max(price_rent)]),
              ratio_min_10_13 = min(price_rent[quarter >= as.yearqtr("2010 Q1") &
                                               quarter <= as.yearqtr("2013 Q4")]),
              rent_wage_end   = rent_wage[which.max(t)],
              rent_wage_min   = min(rent_wage, na.rm = TRUE),
              rent_wage_min_q = format(quarter[which.min(rent_wage)]),
              .groups = "drop")
  print(as.data.frame(t1), digits = 3, row.names = FALSE)

  cat("\n2. Figure C.1: rolling partial R-squared of investor credit\n")
  t2 <- roll |>
    group_by(measure, equation) |>
    summarise(first_window_end = format(min(quarter_end)),
              min_obs          = min(n_obs),
              mean_to_2016q4   = mean(partial_r2[quarter_end <  BREAK_Q]),
              mean_from_2017q1 = mean(partial_r2[quarter_end >= BREAK_Q]),
              peak             = max(partial_r2),
              peak_q           = format(quarter_end[which.max(partial_r2)]),
              last             = partial_r2[which.max(quarter_end)],
              .groups = "drop")
  print(as.data.frame(t2), digits = 3, row.names = FALSE)

  cat("\n3. Figure C.1: windows in which the price equation's value exceeds the rent equation's\n")
  t3 <- roll |>
    select(quarter_end, measure, equation, partial_r2) |>
    pivot_wider(names_from = equation, values_from = partial_r2) |>
    group_by(measure) |>
    summarise(share_price_above = mean(`Price equation` > `Rent equation`),
              rent_max          = max(`Rent equation`),
              rent_max_q        = format(quarter_end[which.max(`Rent equation`)]),
              .groups = "drop")
  print(as.data.frame(t3), digits = 3, row.names = FALSE)
}, finally = {
  sink()
  close(con)
})

# ---- Figures ---------------------------------------------------------------------
# Spread end-of-line labels so they do not overlap. Labels that crowd together
# are spaced evenly around the mean of their lines' end values, so each label
# stays close to its line.
spread_labels <- function(y, min_gap) {
  o <- order(y); y0 <- y[o]; ys <- y0
  for (iter in 1:100) {
    # Labels closer than min_gap form one group; groups only merge, so this ends.
    grp <- cumsum(c(TRUE, diff(ys) > min_gap * (1 + 1e-9)))
    moved <- FALSE
    for (g in unique(grp)) {
      idx <- which(grp == g)
      if (length(idx) < 2) next
      new <- mean(y0[idx]) + (seq_along(idx) - (length(idx) + 1) / 2) * min_gap
      if (any(abs(new - ys[idx]) > 1e-9 * min_gap)) { ys[idx] <- new; moved <- TRUE }
    }
    if (!moved) break
  }
  out <- y; out[o] <- ys; out
}

# The figures carry no title: Elsevier's guides put the title in the caption.
base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        legend.title = element_blank(), legend.position = "bottom",
        axis.title = element_text(colour = "grey25"),
        axis.text  = element_text(colour = "grey35"),
        plot.margin = margin(16, 6, 4, 6))   # room for the 2017:Q1 label

# Dashed line at the break, labelled just above the plotting area so the label
# never sits on a line.
break_marker <- list(
  geom_vline(xintercept = as.numeric(BREAK_Q), linetype = "dashed",
             linewidth = 0.35, colour = "grey55"),
  annotate("text", x = as.numeric(BREAK_Q), y = Inf, label = "2017:Q1",
           vjust = -0.6, size = 3, colour = "grey40"),
  coord_cartesian(clip = "off"))

# floor: the lowest position a label may take, so that a line ending on a
# reference line (Figure C.1's zero line) does not put its label on that line.
end_labels <- function(d, group, value, gap_share, floor = -Inf) {
  e <- d |> group_by(.data[[group]]) |> filter(t == max(t)) |> ungroup()
  e$y_lab <- spread_labels(pmax(e[[value]], floor),
                           gap_share * diff(range(d[[value]], na.rm = TRUE)))
  e
}

plot_cities <- function(var, ylab, ref = NULL) {
  d <- tp |> filter(!is.na(.data[[var]])) |> transmute(city, t, y = .data[[var]])
  e <- end_labels(d, "city", "y", 0.05)
  ggplot(d, aes(t, y, colour = city)) +
    { if (!is.null(ref)) geom_hline(yintercept = ref, linewidth = 0.3, colour = "grey60") } +
    break_marker +
    geom_line(linewidth = 0.7) +
    geom_text(data = e, aes(x = max(d$t) + 0.35, y = y_lab, label = city),
              colour = "grey20", hjust = 0, size = 3.3, inherit.aes = FALSE) +
    scale_colour_manual(values = CITY_COLS, breaks = CITIES) +
    scale_x_continuous(breaks = c(2010, 2015, 2020, 2025),
                       expand = expansion(add = c(0.4, 2.4))) +
    labs(x = NULL, y = ylab) +
    base_theme
}

# Figure 2. Caption in the paper: "Rent indices, five Australian capital
# cities, 2006:Q3--2025:Q4 (ABS 6401.0; base: 2011--12 = 100) ..."
p2 <- plot_cities("rents", "Rent index (2011\u201312 = 100)")
ggsave(file.path(OUTDIR, "fig_rental_trajectories.png"), p2,
       width = FIG_W, height = FIG_H, dpi = FIG_DPI, bg = "white")

# Figure 3. The grey line at 1 marks prices and rents at their 2011-12 relation.
p3 <- plot_cities("price_rent", "Price\u2013rent ratio (2011\u201312 = 1)", ref = 1)
ggsave(file.path(OUTDIR, "fig_price_rent_gap.png"), p3,
       width = FIG_W, height = FIG_H, dpi = FIG_DPI, bg = "white")

# Figure 4. Wage-adjusted rents: the rent index over the state wage price index.
p4 <- plot_cities("rent_wage", "Rent index / wage price index")
ggsave(file.path(OUTDIR, "fig_real_rent.png"), p4,
       width = FIG_W, height = FIG_H, dpi = FIG_DPI, bg = "white")

# Figure C.1. Rolling partial R-squared of investor credit.
d8 <- roll |>
  filter(measure == FIG8_CREDIT) |>
  mutate(t = as.numeric(quarter_end),
         equation = factor(equation, levels = names(OUTCOMES)))
# The rent series ends at about zero; lift its label clear of the zero line.
e8 <- end_labels(d8, "equation", "partial_r2", 0.06,
                 floor = 0.05 * max(d8$partial_r2, na.rm = TRUE))
p8 <- ggplot(d8, aes(t, partial_r2, colour = equation)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  break_marker +
  geom_line(linewidth = 0.8) +
  geom_text(data = e8, aes(x = max(d8$t) + 0.3, y = y_lab, label = equation),
            colour = "grey20", hjust = 0, size = 3.3, inherit.aes = FALSE) +
  scale_colour_manual(values = COL_EQ) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024),
                     expand = expansion(add = c(0.3, 2.9))) +
  labs(x = "Last quarter of the 20-quarter window",
       y = expression("Partial " * italic(R)^2 * " of investor credit")) +
  base_theme
ggsave(file.path(OUTDIR, "fig_divergence.png"), p8,
       width = FIG_W, height = FIG_H, dpi = FIG_DPI, bg = "white")

message("Wrote fig_rental_trajectories.png, fig_price_rent_gap.png, fig_real_rent.png,\n",
        "fig_divergence.png, rolling_partial_r2.csv and descriptive_figures_summary.txt to ",
        OUTDIR, "/")

end_run_log()
