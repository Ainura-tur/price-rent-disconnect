# =============================================================================
# 06_paper_figures.R -- the paper's model and robustness figures in one style
#
# The earlier scripts save these figures 8 to 9 inches wide at 300 dpi, and the
# paper shrinks them to fit, so their text prints at about 6 pt, their lines
# thin out and their resolution falls below 500 dpi. This script redraws them
# from the saved results in the style of Figures 2, 9 and F.1: theme_minimal
# with base_size = 11, plain panel titles, main lines 0.9 wide, 600 dpi, and
# each saved at its share of a 6.5-inch line. The paper prints a full line at
# \textwidth = 6.3 inches, so every figure shrinks by the same 3 percent and
# its text prints at about 8.5 pt, as in Figures 2, 9 and F.1.
#
#   Figure 5    fig_lp_irf_adjusted.png       0.8 \textwidth in the paper
#   Figure 6    fig_model_fit.png             0.8
#   Figure 7    fig_model_profile.png         1.0   lines now as thick as elsewhere
#   Figure 8    fig_model_counterfactual.png  0.8
#   Figure D.1  fig_lpiv_main.png             0.9
#   Figure D.2  fig_cr_valid_range.png        1.0
#   Figure E.1  fig_lp_postimpact.png         0.85  panel titles in words
#   Figure E.2  fig_divergence.png            0.8   rents in teal, as elsewhere
#
# It estimates nothing. Run it last. 02c, 03_figures, 03f, 04, 04b, 05c and
# 05b still write their own versions of these files, so rerun this script
# after any of them.
#
# Reads:  output/sa_C_lp.csv (02c_sa_additions.R)
#         output/rolling_financial_r2.csv (02_estimate.R)
#         output/lpiv_apra.csv, output/cr_valid_ranges.csv (03f_missing_figures.R)
#         output/model_estimates.csv, output/model_fit.csv,
#         output/model_counterfactual.csv (04_model_matching.R)
#         output/model_profile.csv (04b_identification.R)
#         output/boot_b8_*.rds (05_bootstrap.R), for the bands in Figures 6 and 8
# Writes: the eight figure files above, in output/
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

# ---- Common style -------------------------------------------------------------------
LINE <- 6.5                    # inches; Figures 2, 9 and F.1 are saved this wide
DPI  <- 600
LW   <- 0.9                    # main lines, as in Figures 9 and F.1
PAL  <- c(Price = "#c0392b", Rent = "#1b6f7a")
NAVY <- "#2c3e50"
theme_paper <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
TXT <- 0.8 * 11 / .pt          # geom_text size that matches the axis numbers

# share: the figure's width in the paper as a share of \textwidth.
save_fig <- function(g, file, share, height) {
  ggplot2::ggsave(file.path(OUTDIR, file), g, width = share * LINE, height = height,
                  dpi = DPI, bg = "white")
  cat(sprintf("Wrote %-30s %.2f x %.2f in, %d dpi\n", file, share * LINE, height, DPI))
}
rd <- function(f) read_csv(file.path(OUTDIR, f), show_col_types = FALSE)

# Bootstrap draws. boot_b8_FIRST_LAST.rds holds draws FIRST to LAST; the files
# must cover 1 to N once each, because a leftover test chunk (boot_b8_1_2.rds
# beside boot_b8_1_30.rds) would count draws twice.
load_boot <- function(bl) {
  f <- list.files(OUTDIR, pattern = sprintf("^boot_b%d_\\d+_\\d+\\.rds$", bl), full.names = TRUE)
  if (!length(f)) return(NULL)
  rg <- do.call(rbind, lapply(regmatches(basename(f), regexec("_(\\d+)_(\\d+)\\.rds$", basename(f))),
                              function(m) as.integer(m[2:3])))
  o <- order(rg[, 1], rg[, 2]); rg <- rg[o, , drop = FALSE]; f <- f[o]
  if (rg[1, 1] != 1 || any(rg[-1, 1] != rg[-nrow(rg), 2] + 1))
    stop("boot_b", bl, " files in ", OUTDIR, " overlap or leave a gap: ",
         paste(basename(f), collapse = ", "),
         ". Move any test chunk out of the folder, or rerun a missing one.")
  B <- do.call(rbind, lapply(f, readRDS))
  if (nrow(B) != rg[nrow(rg), 2])
    warning("boot_b", bl, ": ", nrow(B), " draws, not ", rg[nrow(rg), 2], " (some draws failed)")
  cat(sprintf("Bootstrap, blocks of %d quarters: %d draws in %d files\n", bl, nrow(B), length(f)))
  B
}
B8 <- load_boot(8)
band <- function(prefix, n)
  t(apply(B8[, paste0(prefix, 0:(n - 1)), drop = FALSE], 2, quantile, c(0.05, 0.95), na.rm = TRUE))

# ---- Figures 5 and E.1: local projections (02c) -------------------------------------
C <- rd("sa_C_lp.csv")

f5 <- C |> filter(spec %in% c("Price, baseline", "Rent, baseline")) |>
  mutate(outcome = factor(sub(",.*", "", spec), levels = c("Price", "Rent")))
g5 <- ggplot(f5, aes(h, beta, colour = outcome, fill = outcome)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.22, colour = NA) +
  geom_line(linewidth = LW) +
  scale_colour_manual(values = PAL) + scale_fill_manual(values = PAL) +
  scale_x_continuous(breaks = 0:12) +
  labs(x = "Quarters after the innovation", y = "Log points", colour = NULL, fill = NULL) +
  theme_paper
save_fig(g5, "fig_lp_irf_adjusted.png", 0.8, 3.0)

# Panel titles in words; the caption gives the formulas.
PA <- "(a) Cumulative response, with labour-market controls"
PB <- "(b) Post-impact response, with the current price change held fixed"
fE1 <- C |> filter(spec %in% c("Price, labour controls", "Rent, labour controls",
                               "Price, post-impact", "Rent, post-impact")) |>
  mutate(outcome = factor(ifelse(grepl("^Price", spec), "Price", "Rent"), levels = c("Price", "Rent")),
         panel = factor(ifelse(grepl("post-impact", spec), PB, PA), levels = c(PA, PB)))
gE1 <- ggplot(fE1, aes(h, beta, colour = outcome, fill = outcome)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.22, colour = NA) +
  geom_line(linewidth = LW) +
  facet_wrap(~panel, ncol = 1) +
  scale_colour_manual(values = PAL) + scale_fill_manual(values = PAL) +
  scale_x_continuous(breaks = 0:12) +
  labs(x = "Quarters after the innovation", y = "Log points", colour = NULL, fill = NULL) +
  theme_paper
save_fig(gE1, "fig_lp_postimpact.png", 0.85, 5.15)

# ---- Figures 6 and 8: the model, with bootstrap intervals (04, 05) -------------------
if (is.null(B8)) {
  cat("No boot_b8 files in output/, so Figures 6 and 8 are left as they are.\n")
} else {
  bp <- band("mp", 13); br <- band("mr", 13)
  fit <- rd("model_fit.csv") |> filter(outcome %in% c("Price", "Rent")) |> arrange(outcome, h) |>
    mutate(mlo = ifelse(outcome == "Price", bp[h + 1, 1], br[h + 1, 1]),
           mhi = ifelse(outcome == "Price", bp[h + 1, 2], br[h + 1, 2]),
           outcome = factor(outcome, levels = c("Price", "Rent")))
  g6 <- ggplot(fit, aes(h, colour = outcome, fill = outcome)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_ribbon(aes(ymin = data - 1.645 * se, ymax = data + 1.645 * se), alpha = 0.12, colour = NA) +
    geom_errorbar(aes(ymin = mlo, ymax = mhi), width = 0.25, linewidth = 0.5) +
    geom_line(aes(y = data), linewidth = LW) +
    geom_line(aes(y = model), linewidth = LW, linetype = "dashed") +
    geom_point(aes(y = model), size = 1.4) +
    scale_colour_manual(values = PAL) + scale_fill_manual(values = PAL) +
    scale_x_continuous(breaks = 0:12) +
    labs(x = "Quarters after the innovation", y = "Log points", colour = NULL, fill = NULL) +
    theme_paper
  save_fig(g6, "fig_model_fit.png", 0.8, 3.0)

  cf <- rd("model_counterfactual.csv") |> arrange(h)
  cp <- band("cfp", 29); cr <- band("cfr", 29)
  f8 <- bind_rows(
    data.frame(h = 0:28, outcome = "Price", value = cf$price[1:29], lo = cp[, 1], hi = cp[, 2]),
    data.frame(h = 0:28, outcome = "Rent",  value = cf$rent[1:29],  lo = cr[, 1], hi = cr[, 2])) |>
    mutate(outcome = factor(outcome, levels = c("Price", "Rent")))
  g8 <- ggplot(f8, aes(h, value, colour = outcome, fill = outcome)) +
    annotate("rect", xmin = 0, xmax = 15, ymin = -Inf, ymax = Inf, alpha = 0.06) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
    geom_line(linewidth = LW) +
    scale_colour_manual(values = PAL) + scale_fill_manual(values = PAL) +
    scale_x_continuous(breaks = seq(0, 28, 4)) +
    labs(x = "Quarters after the tightening begins", y = "Log points", colour = NULL, fill = NULL) +
    theme_paper
  save_fig(g8, "fig_model_counterfactual.png", 0.8, 2.75)
}

# ---- Figure 7: profiles of the matching distance (04, 04b) ---------------------------
est <- rd("model_estimates.csv"); th <- setNames(est$estimate, est$param)
lab7 <- c(theta = "Speed of price adjustment, \u03b8", kappa = "Speed of tenure switching, \u03ba",
          eta = "Tenure response to prices, \u03b7")
P <- rd("model_profile.csv") |> filter(param %in% names(lab7)) |>
  mutate(panel = factor(lab7[param], levels = lab7))
est_pts <- data.frame(panel = factor(lab7, levels = lab7), value = unname(th[names(lab7)]))
g7 <- ggplot(P, aes(value, excess)) +
  geom_line(colour = "#1f4e79", linewidth = LW) +
  geom_point(colour = "#1f4e79", size = 1.4) +
  geom_vline(data = est_pts, aes(xintercept = value), linetype = "dashed", colour = "grey40") +
  facet_wrap(~panel, scales = "free_x") +
  labs(x = "Parameter value (held fixed; other six re-estimated)",
       y = "Increase in distance\nover the minimum") +
  theme_paper + theme(panel.spacing.x = grid::unit(1.2, "lines"))
save_fig(g7, "fig_model_profile.png", 1, 2.5)

# ---- Figure D.1: instrumented and OLS responses (03f) -------------------------------
lvD1 <- c("National timing: prices", "Exposure: prices", "National timing: rents", "Exposure: rents")
lp <- rd("lpiv_apra.csv") |>
  mutate(lo = iv - 1.645 * iv_se, hi = iv + 1.645 * iv_se,
         panel = factor(paste0(instrument, ": ", tolower(outcome)), levels = lvD1))
gD1 <- ggplot(lp, aes(h)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = NAVY, alpha = 0.15) +
  geom_line(aes(y = iv, linetype = "Instrumented"), linewidth = LW, colour = NAVY) +
  geom_line(aes(y = ols, linetype = "OLS"), linewidth = LW, colour = "#c0392b") +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  scale_linetype_manual(values = c(Instrumented = "solid", OLS = "dashed"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  guides(linetype = guide_legend(override.aes = list(colour = c(NAVY, "#c0392b")))) +
  labs(x = "Quarters after t", y = "Change in log level per unit\nof cumulative credit growth") +
  theme_paper + theme(legend.key.width = grid::unit(2, "lines"))
save_fig(gD1, "fig_lpiv_main.png", 0.9, 4.5)

# ---- Figure D.2: valid ranges of rho_xu (03f) ----------------------------------------
rowsD2 <- c("4 quarters, national", "4 quarters, exposure", "8 quarters, national",
            "8 quarters, exposure", "12 quarters, national", "12 quarters, exposure")
vr <- rd("cr_valid_ranges.csv") |>
  mutate(row = factor(paste0(h, " quarters, ", ifelse(instrument == "Exposure", "exposure", "national")),
                      levels = rev(rowsD2)),
         outcome = factor(outcome, levels = c("Prices", "Rents")))
gD2 <- ggplot(vr, aes(y = row)) +
  geom_segment(aes(x = 0, xend = 0.8, yend = row), linewidth = 4, colour = "grey88") +
  geom_segment(data = filter(vr, !is.na(left)),
               aes(x = left, xend = right, yend = row, colour = instrument), linewidth = 4) +
  geom_text(data = filter(vr, is.na(left)), aes(x = 0.4, label = "fails for every range"),
            size = TXT, colour = "grey35") +
  facet_wrap(~outcome, ncol = 2) +
  scale_colour_manual(values = c("National timing" = NAVY, "Exposure" = "#16a085"),
                      breaks = c("National timing", "Exposure"), name = NULL) +
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  labs(x = expression("Assumed correlation of credit with the error, " * rho[xu]), y = NULL) +
  theme_paper +
  theme(panel.grid.major.y = element_blank(), panel.spacing.x = grid::unit(1.2, "lines"))
save_fig(gD2, "fig_cr_valid_range.png", 1, 3.05)

# ---- Figure E.2: rolling partial R-squared of investor credit (02) -----------------
roll <- rd("rolling_financial_r2.csv") |>
  tidyr::pivot_longer(c(price_invR2, rent_invR2), names_to = "equation", values_to = "invR2") |>
  mutate(equation = recode(equation, price_invR2 = "Price equation", rent_invR2 = "Rent equation"))
gE2 <- ggplot(roll, aes(qend, invR2, colour = equation)) +
  geom_vline(xintercept = 2017, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = LW) +
  scale_colour_manual(values = c("Price equation" = unname(PAL["Price"]),
                                 "Rent equation"  = unname(PAL["Rent"])), name = NULL) +
  labs(x = NULL, y = expression("Partial " * R^2 * " of investor credit")) +
  theme_paper
save_fig(gE2, "fig_divergence.png", 0.8, 3.0)

cat("Done. Copy the eight files to the paper folder and recompile.\n")
