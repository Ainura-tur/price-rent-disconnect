# =============================================================================
# 04c_inflation.R -- credit conditions and measured inflation (Section 6)
#
# The CPI rent index measures rents on all current leases, which is the measured
# rent r_t of the model. This script turns the estimated responses into annual
# (four-quarter) inflation rates of prices and rents:
#   N1  After a one-SD investor-credit innovation (horizons 0-12).
#   N2  During and after the counterfactual tightening of Section 6 (0-28).
#   N3  Peak timing across the block-bootstrap draws.
# Contributions of rents to headline CPI inflation are reported at the Australian
# CPI weight, at a rental-equivalence weight for Australia (owner-occupied
# housing priced by imputed rents, as in the US CPI), and at the US weight.
#
# Reads:  output/model_estimates.csv, output/model_counterfactual.csv, the model
#         (04_model_matching.R definitions), output/boot_b8_*.rds (05_bootstrap.R)
# Writes: output/model_inflation.txt, output/model_inflation.csv,
#         output/fig_model_inflation.png (Figure 9)
# Run after 05_bootstrap.R, despite its number: N3 and the bands need its draws.
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

# Model definitions from 04_model_matching.R, without re-running its estimation.
ex <- parse("04_model_matching.R")
stop_at <- which(sapply(ex, function(e) grepl("^Js <-", paste(deparse(e), collapse = ""))))[1]
for (e in ex[seq_len(stop_at - 1)]) eval(e, globalenv())
th <- read_csv(file.path(OUTDIR, "model_estimates.csv"), show_col_types = FALSE)$estimate
names(th) <- pn; J0 <- 8

# CPI weights (share of the basket):
#   Australia: Rents, 2025 weight, eight capitals, 6.61 percent (ABS, Annual
#     weight update of the CPI and Living Cost Indexes, December 2024;
#     abs2024weights in the .bib).
#   United States: relative importance, December 2025, rent of primary
#     residence 7.840 plus owners' equivalent rent of residences 26.204 (BLS
#     fact sheet on OER and rent, Table A; bls2025oer in the .bib).
W_AU <- 0.0661; W_US <- 0.0784 + 0.2620

# Rental-equivalence weight for Australia: the weight rents would carry if the
# CPI, like the US CPI, priced owner-occupied housing by an imputed rent.
# National accounts, 2021-22 (ABS, "Housing cost measures in the National
# Accounts", 2023; abs2023housingcost in the .bib): actual rents 4.6 and
# imputed rents of owner-occupiers 15.5 percent of household final
# consumption expenditure. Imputed rents enter the
# basket in that ratio to actual rents and replace new dwelling purchases by
# owner-occupiers (7.59 percent in the 2025 basket).
HFCE_ACTUAL <- 4.6; HFCE_IMPUTED <- 15.5; W_NEWDW <- 0.0759
w_imp <- W_AU * HFCE_IMPUTED / HFCE_ACTUAL
W_AU_RE <- (W_AU + w_imp) / (1 - W_NEWDW + w_imp)

# Annual inflation at horizon h: x_h - x_{h-4}, with x = 0 before the shock.
annual <- function(x) sapply(seq_along(x), function(i) x[i] - if (i > 4) x[i - 4] else 0)
q90 <- function(x) quantile(x, c(0.05, 0.95), na.rm = TRUE)

sink(file.path(OUTDIR, "model_inflation.txt"), split = TRUE)
cat(sprintf("CPI weights of rents: Australia %.4f; Australia, rental equivalence %.4f (imputed rents %.4f of the current basket); US %.4f\n\n",
            W_AU, W_AU_RE, w_imp, W_US))

# ---- N1. Credit innovation ------------------------------------------------------------
m <- model_irf(th, J0)
ap <- annual(m$p); ar <- annual(m$r)
cat("== N1 annual inflation after a one-SD credit innovation (log points) ==\n")
print(round(data.frame(h = 0:H, price = ap, rent = ar), 4), row.names = FALSE)
cat(sprintf("Peak annual price inflation %.4f at h = %d; peak annual rent inflation %.4f at h = %d\n",
            max(ap), which.max(ap) - 1, max(ar), which.max(ar) - 1))
cat(sprintf("Rent level at h = 12: %.4f\n", m$r[13]))
for (w in c(W_AU, W_AU_RE, W_US))
  cat(sprintf("CPI weight %.3f: rents add %.3f pp to the CPI level by h = 12 and at most %.3f pp to annual CPI inflation\n",
              w, 100 * w * m$r[13], 100 * w * max(ar)))

# ---- N2. Counterfactual tightening -----------------------------------------------------
cf <- read_csv(file.path(OUTDIR, "model_counterfactual.csv"), show_col_types = FALSE)
cp <- annual(cf$price); cr <- annual(cf$rent)
cat("\n== N2 annual inflation during the tightening (h = 0-15) and after ==\n")
print(round(data.frame(h = cf$h, price = cp, rent = cr)[cf$h %in% c(0, 3, 4, 7, 8, 11, 12, 15, 16, 19, 20, 24, 28), ], 4),
      row.names = FALSE)
cat(sprintf("Lowest annual price inflation %.4f at h = %d; lowest annual rent inflation %.4f at h = %d\n",
            min(cp), which.min(cp) - 1, min(cr), which.min(cr) - 1))
first_pos <- function(x, from) { i <- which(x[(from + 1):length(x)] > 0)[1]; if (is.na(i)) NA else i + from - 1 }
cat(sprintf("Annual price inflation turns positive at h = %d; annual rent inflation at h = %d (tightening ends after h = 15)\n",
            first_pos(cp, which.min(cp)), first_pos(cr, which.min(cr))))
for (w in c(W_AU, W_AU_RE, W_US))
  cat(sprintf("CPI weight %.3f: lowest contribution of rents to annual CPI inflation %.3f pp\n", w, 100 * w * min(cr)))

# ---- N3. Bootstrap draws (blocks of eight) -----------------------------------------------
f <- list.files(OUTDIR, pattern = "^boot_b8_\\d+_\\d+\\.rds$", full.names = TRUE)
# boot_b8_FIRST_LAST.rds holds draws FIRST to LAST. The files must cover 1 to N
# once each: a leftover test chunk (boot_b8_1_2.rds beside boot_b8_1_30.rds)
# would count draws twice.
rg <- do.call(rbind, lapply(regmatches(basename(f), regexec("_(\\d+)_(\\d+)\\.rds$", basename(f))),
                            function(m) as.integer(m[2:3])))
o <- order(rg[, 1], rg[, 2]); rg <- rg[o, , drop = FALSE]; f <- f[o]
if (rg[1, 1] != 1 || any(rg[-1, 1] != rg[-nrow(rg), 2] + 1))
  stop("Bootstrap files in ", OUTDIR, " overlap or leave a gap: ", paste(basename(f), collapse = ", "),
       ". Move test chunks out of the folder.")
B <- do.call(rbind, lapply(f, readRDS))
if (nrow(B) != rg[nrow(rg), 2]) stop("Expected ", rg[nrow(rg), 2], " bootstrap draws, found ", nrow(B))
AP <- t(apply(B[, paste0("mp", 0:H)], 1, annual)); AR <- t(apply(B[, paste0("mr", 0:H)], 1, annual))
CP <- t(apply(B[, paste0("cfp", 0:28)], 1, annual)); CR <- t(apply(B[, paste0("cfr", 0:28)], 1, annual))
pk_p <- apply(AP, 1, which.max) - 1; pk_r <- apply(AR, 1, which.max) - 1
lo_p <- apply(CP, 1, which.min) - 1; lo_r <- apply(CR, 1, which.min) - 1
cat("\n== N3 bootstrap, blocks of eight:", nrow(B), "draws ==\n")
# Quarters are whole numbers, so their 5-95 percent ranges use type-1 quantiles
# (actual draws) rather than interpolated ones such as 0.6 or 2.7.
q90i <- function(x) quantile(x, c(0.05, 0.95), type = 1, names = FALSE)
cat(sprintf("Credit innovation: rent-inflation peak later than price-inflation peak in %.0f%% of draws; median lag %.0f quarters (5-95%%: %s)\n",
            100 * mean(pk_r > pk_p), median(pk_r - pk_p), paste(q90i(pk_r - pk_p), collapse = " to ")))
cat(sprintf("Lag between 1 and 7 quarters in %.0f%% of draws\n", 100 * mean(pk_r - pk_p >= 1 & pk_r - pk_p <= 7)))
cat(sprintf("Peak quarter, price: 5-95%% %s; rent: %s\n", paste(q90i(pk_p), collapse = " to "), paste(q90i(pk_r), collapse = " to ")))
cat(sprintf("Tightening: lowest rent inflation later than lowest price inflation in %.0f%% of draws; median lag %.0f quarters\n",
            100 * mean(lo_r > lo_p), median(lo_r - lo_p)))
# In a draw where annual rent inflation never rises above zero (never falls below
# zero in the tightening), rents do not respond, and which.max (which.min) returns
# quarter 0. Count those draws and report the timing among the others.
up <- apply(AR, 1, max) > 1e-6; dn <- apply(CR, 1, min) < -1e-6
cat(sprintf("Innovation: rents never rise in %d draws (%.0f%%); in the other draws the rent-inflation peak is later in %.0f%%\n",
            sum(!up), 100 * mean(!up), 100 * mean(pk_r[up] > pk_p[up])))
cat(sprintf("Tightening: rents never fall in %d draws (%.0f%%); in the other draws the low in rent inflation is later in %.0f%%\n",
            sum(!dn), 100 * mean(!dn), 100 * mean(lo_r[dn] > lo_p[dn])))
cat(sprintf("Peak annual rent inflation after an innovation, 90%% interval: %s\n",
            paste(round(q90(apply(AR, 1, max)), 4), collapse = " to ")))
sink()

# ---- Output table and figure -------------------------------------------------------------
band <- function(M) t(apply(M, 2, q90))
bP <- band(AP); bR <- band(AR); bCP <- band(CP); bCR <- band(CR)
out <- bind_rows(
  data.frame(panel = "innovation", h = 0:H, outcome = "Price", value = ap, lo = bP[, 1], hi = bP[, 2]),
  data.frame(panel = "innovation", h = 0:H, outcome = "Rent",  value = ar, lo = bR[, 1], hi = bR[, 2]),
  data.frame(panel = "tightening", h = 0:28, outcome = "Price", value = cp, lo = bCP[, 1], hi = bCP[, 2]),
  data.frame(panel = "tightening", h = 0:28, outcome = "Rent",  value = cr, lo = bCR[, 1], hi = bCR[, 2]))
write_csv(out, file.path(OUTDIR, "model_inflation.csv"))

pal <- c(Price = "#c0392b", Rent = "#1b6f7a")
lab <- c(innovation = "(a) After a one-SD credit innovation",
         tightening = "(b) During and after a four-year tightening")
out$panel <- factor(lab[out$panel], levels = lab)
shade <- data.frame(panel = factor(lab["tightening"], levels = lab), xmin = 0, xmax = 15)
g <- ggplot(out, aes(h, value, colour = outcome, fill = outcome)) +
  geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.06) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.16, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~panel, scales = "free") +
  scale_x_continuous(breaks = seq(0, 28, 4)) +
  scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
  labs(x = "Quarters after the shock", y = "Four-quarter change, log points", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.spacing.x = grid::unit(1.4, "lines"))
# Saved 6.5 inches wide, like Figures 2 to 4, so that at \textwidth its text
# prints at about 8 pt (at 9 inches it printed at about 6 pt).
ggsave(file.path(OUTDIR, "fig_model_inflation.png"), g, width = 6.5, height = 3.2, dpi = 600)
cat("\nDone.\n")
