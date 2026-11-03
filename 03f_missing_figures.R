# =============================================================================
# 03f_missing_figures.R -- figures for Section 5.6 (APRA instruments)
#
#   fig_lpiv_main.png      Instrumented and OLS cumulative responses (Eq. lpiv),
#                          national-timing and exposure instruments, h = 0..12.
#   fig_cr_valid_range.png Valid ranges of rho_xu under which each instrument
#                          passes the correlation-restriction test.
#
# The LP-IV estimates reproduce Table D.1 exactly (IV, OLS and first-stage F).
# The valid ranges are drawn from the MCUB results reported in the text
# (cr_valid_ranges below). Where the text does not state an endpoint, the value
# comes from the MCUB test of the ivcrtest package run in 03g_cr_endpoints.R,
# which gives 0.15 at twelve quarters (now used in the text) and about 0
# where the text reports 0.01. cr_check below is an Imbens-Manski/Stoye cross-check, which reproduces
# every pass/fail verdict in Table D.1.
# =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({ library(plm); library(lmtest); library(ggplot2) })

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

set.seed(20261031)
dkv <- function(m, ml = 4) plm::vcovSCC(m, type = "HC0", maxlag = ml)
L <- function(x, k = 1) dplyr::lag(x, k)

# ---- Data: seasonally adjusted investor and owner-occupier commitments --------
lend <- read_csv(F_LEND, show_col_types = FALSE) |>
  filter(Measure == "Value", `Loan Purpose` == "Total dwellings excluding refinancing",
         `Housing Purpose` %in% c("Investor", "Owner occupier"),
         `Adjustment Type` == "Seasonally Adjusted", Region %in% names(STATE2CITY)) |>
  mutate(city = recode(Region, !!!STATE2CITY), quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)),
         v = as.numeric(OBS_VALUE)) |>
  distinct(city, quarter, `Housing Purpose`, .keep_all = TRUE) |>
  tidyr::pivot_wider(id_cols = c(city, quarter), names_from = `Housing Purpose`, values_from = v) |>
  rename(inv = Investor, oo = `Owner occupier`)
d <- readRDS(file.path(CLEAN, "panel_combined.rds")) |> left_join(lend, by = c("city", "quarter")) |>
  arrange(city, quarter) |> group_by(city) |>
  mutate(lp = log(prop_price), lr = log(rents), li = log(inv),
         dp = lp - L(lp), dr = lr - L(lr), di = li - L(li), dc = cash_rate - L(cash_rate),
         tight_l1 = L(tightness), dc_l1 = L(dc)) |> ungroup()
sh14 <- d |> filter(format(quarter, "%Y") == "2014") |> group_by(city) |>
  summarise(s14 = mean(inv / (inv + oo)), .groups = "drop")
d <- d |> left_join(sh14, by = "city") |>
  mutate(nat  = as.integer(quarter >= as.yearqtr("2015 Q1") & quarter <= as.yearqtr("2018 Q4")),
         io   = as.integer(quarter >= as.yearqtr("2017 Q2") & quarter <= as.yearqtr("2018 Q4")),
         zexp = s14 * io, qid = as.integer(factor(quarter)))
for (j in 1:4) d <- d |> group_by(city) |>
  mutate("di_l{j}" := L(di, j), "dp_l{j}" := L(dp, j), "dr_l{j}" := L(dr, j)) |> ungroup()

prep <- function(y, own, z, h, tw) {
  ctrl <- c(paste0("di_l", 1:4), paste0(own, "_l", 1:4), "tight_l1", if (!tw) "dc_l1")
  x <- d |> group_by(city) |>
    mutate(Y = dplyr::lead(.data[[y]], h) - L(.data[[y]]), X = dplyr::lead(li, h) - L(li)) |>
    ungroup() |> filter(if_all(all_of(c("Y", "X", z, ctrl)), is.finite))
  list(P = pdata.frame(x, index = c("city", "qid")), ctrl = ctrl, eff = if (tw) "twoways" else "individual")
}
lpiv <- function(y, own, z, h, tw) {
  o <- prep(y, own, z, h, tw); cs <- paste(o$ctrl, collapse = " + ")
  fs  <- plm(as.formula(paste("X ~", z, "+", cs)), o$P, model = "within", effect = o$eff)
  ols <- plm(as.formula(paste("Y ~ X +", cs)), o$P, model = "within", effect = o$eff)
  iv  <- plm(as.formula(paste("Y ~ X +", cs, "|", z, "+", cs)), o$P, model = "within", effect = o$eff)
  # Anderson-Rubin test of no response: with one instrument, the Wald test of
  # the instrument in the reduced form of Y, with Driscoll-Kraay errors.
  rf  <- plm(as.formula(paste("Y ~", z, "+", cs)), o$P, model = "within", effect = o$eff)
  ar  <- coef(rf)[z]^2 / dkv(rf)[z, z]
  data.frame(h = h, iv = coef(iv)["X"], iv_se = sqrt(dkv(iv)["X", "X"]),
             ols = coef(ols)["X"], F = coef(fs)[z]^2 / dkv(fs)[z, z], n = nobs(iv),
             ar_p = 1 - pchisq(ar, 1))
}
cfgs <- list(
  list(out = "Prices", ins = "National timing", y = "lp", own = "dp", z = "nat",  tw = FALSE),
  list(out = "Rents",  ins = "National timing", y = "lr", own = "dr", z = "nat",  tw = FALSE),
  list(out = "Prices", ins = "Exposure",        y = "lp", own = "dp", z = "zexp", tw = TRUE),
  list(out = "Rents",  ins = "Exposure",        y = "lr", own = "dr", z = "zexp", tw = TRUE))
lp_res <- bind_rows(lapply(cfgs, function(cf) bind_rows(lapply(0:12, function(h)
  lpiv(cf$y, cf$own, cf$z, h, cf$tw))) |> mutate(outcome = cf$out, instrument = cf$ins)))
rownames(lp_res) <- NULL
cat("== LP-IV at h = 4, 8, 12 (compare Table D.1) ==\n")
print(lp_res |> filter(h %in% c(4, 8, 12)) |> select(outcome, instrument, h, iv, ols, F), digits = 3)
write_csv(lp_res, file.path(OUTDIR, "lpiv_apra.csv"))

lp_plot <- lp_res |>
  mutate(lo = iv - 1.645 * iv_se, hi = iv + 1.645 * iv_se,
         panel = factor(paste0(instrument, ": ", tolower(outcome)),
                        levels = c("National timing: prices", "Exposure: prices",
                                   "National timing: rents",  "Exposure: rents")))
ggsave(file.path(OUTDIR, "fig_lpiv_main.png"),
  ggplot(lp_plot, aes(h)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#2c3e50", alpha = 0.15) +
    geom_line(aes(y = iv, linetype = "Instrumented"), linewidth = 0.9, colour = "#2c3e50") +
    geom_line(aes(y = ols, linetype = "OLS"), linewidth = 0.9, colour = "#c0392b") +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    scale_linetype_manual(values = c(Instrumented = "solid", OLS = "dashed"), name = NULL) +
    scale_x_continuous(breaks = seq(0, 12, 2)) +
    guides(linetype = guide_legend(override.aes = list(colour = c("#2c3e50", "#c0392b")))) +
    labs(x = "Quarters after t", y = "Change in log level per unit of cumulative credit growth") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold", hjust = 0)),
  width = 8.5, height = 6.5, dpi = 300)

# ---- Correlation-restriction check (Imbens-Manski/Stoye version) ------------
cors <- function(df) c(xy = cor(df$x, df$y), zx = cor(df$z, df$x), zy = cor(df$z, df$y))
rho_zu <- function(c, r) c * r[["zx"]] + sqrt(1 - c^2) * (r[["zy"]] - r[["xy"]] * r[["zx"]]) / sqrt(1 - r[["xy"]]^2)
boot_cov <- function(df, B = 1999, bl = 8) {
  qs <- sort(unique(df$qid)); n <- length(qs); nb <- ceiling(n / bl); starts <- 1:(n - bl + 1)
  cov(t(replicate(B, {
    idx <- unlist(lapply(sample(starts, nb, replace = TRUE), function(a) qs[a:(a + bl - 1)]))[1:n]
    cors(do.call(rbind, lapply(idx, function(q) df[df$qid == q, ])))
  })))
}
grid <- seq(0, 0.8, by = 0.01)
im_ci <- function(lo, hi, slo, shi, alpha = 0.05) {
  D <- max(hi - lo, 0); sm <- max(slo, shi)
  cc <- uniroot(function(c) pnorm(c + D / sm) - pnorm(-c) - (1 - alpha), c(0, 5))$root
  c(lo - cc * slo, hi + cc * shi)
}
if (Sys.getenv("CR_CHECK") == "1") {
cr_check <- bind_rows(lapply(cfgs, function(cf) bind_rows(lapply(c(4, 8, 12), function(h) {
  o <- prep(cf$y, cf$own, cf$z, h, cf$tw)
  rs <- function(v) as.numeric(residuals(plm(as.formula(paste(v, "~", paste(o$ctrl, collapse = "+"))),
                                             o$P, model = "within", effect = o$eff)))
  df <- data.frame(qid = as.integer(as.character(o$P$qid)), y = rs("Y"), x = rs("X"), z = rs(cf$z))
  if (cor(df$z, df$x) < 0) df$z <- -df$z
  r <- cors(df); V <- boot_cov(df)
  pts <- t(sapply(grid, function(c) {
    f <- function(rr) rho_zu(c, setNames(rr, names(r)))
    g <- sapply(1:3, function(k) { e <- rep(0, 3); e[k] <- 1e-6; (f(r + e) - f(r - e)) / 2e-6 })
    c(est = f(r), se = sqrt(drop(t(g) %*% V %*% g))) }))
  ci <- function(a, b) { k <- which(grid >= a - 1e-9 & grid <= b + 1e-9); P <- pts[k, , drop = FALSE]
    il <- which.min(P[, "est"]); iu <- which.max(P[, "est"])
    im_ci(P[il, "est"], P[iu, "est"], P[il, "se"], P[iu, "se"]) }
  cov0 <- function(x) x[1] <= 0 & x[2] >= 0
  left  <- sapply(grid, function(r) cov0(ci(0, r)))
  right <- sapply(grid, function(r) cov0(ci(r, 0.8)))
  full <- ci(0, 0.8)
  data.frame(outcome = cf$out, instrument = cf$ins, h = h, lo = full[1], hi = full[2],
             pass = cov0(full), left = if (any(left)) min(grid[left]) else NA,
             right = if (any(right)) max(grid[right]) else NA)
}))))
rownames(cr_check) <- NULL
cat("\n== CR check, IM/Stoye intervals over [0, 0.8] and valid ranges ==\n"); print(cr_check, digits = 3)
write_csv(cr_check, file.path(OUTDIR, "cr_check_im.csv"))
}

# ---- Valid ranges as reported in the text (MCUB) ------------------------------
# source = "text": stated in Section 5.6.2; "mcub": from 03g_cr_endpoints.R.
cr_valid_ranges <- tibble::tribble(
  ~outcome, ~instrument,        ~h, ~left, ~right, ~source,
  "Prices", "National timing",   4,  0.01,  0.80,  "text (left); mcub (right)",
  "Prices", "National timing",   8,  0.00,  0.80,  "mcub",
  "Prices", "National timing",  12,  0.15,  0.80,  "mcub (03g: 0.146)",
  "Prices", "Exposure",          4,    NA,    NA,  "text (fails throughout)",
  "Prices", "Exposure",          8,  0.00,  0.18,  "text",
  "Prices", "Exposure",         12,  0.00,  0.68,  "text",
  "Rents",  "National timing",   4,  0.00,  0.04,  "text",
  "Rents",  "National timing",   8,  0.00,  0.19,  "text",
  "Rents",  "National timing",  12,  0.00,  0.33,  "text",
  "Rents",  "Exposure",          4,  0.00,  0.80,  "text (passes throughout)",
  "Rents",  "Exposure",          8,  0.00,  0.80,  "text (passes throughout)",
  "Rents",  "Exposure",         12,  0.00,  0.80,  "text (passes throughout)")
write_csv(cr_valid_ranges, file.path(OUTDIR, "cr_valid_ranges.csv"))

vr <- cr_valid_ranges |>
  mutate(row = factor(paste0(h, " quarters, ", ifelse(instrument == "Exposure", "exposure", "national")),
                      levels = rev(c("4 quarters, national", "4 quarters, exposure", "8 quarters, national",
                                     "8 quarters, exposure", "12 quarters, national", "12 quarters, exposure"))),
         outcome = factor(outcome, levels = c("Prices", "Rents")))
ggsave(file.path(OUTDIR, "fig_cr_valid_range.png"),
  ggplot(vr, aes(y = row)) +
    geom_segment(aes(x = 0, xend = 0.8, yend = row), linewidth = 5, colour = "grey88") +
    geom_segment(data = vr |> filter(!is.na(left)),
                 aes(x = left, xend = right, yend = row, colour = instrument), linewidth = 5) +
    geom_text(data = vr |> filter(is.na(left)), aes(x = 0.4, label = "fails for every range"),
              size = 3, colour = "grey35") +
    facet_wrap(~outcome, ncol = 2) +
    scale_colour_manual(values = c("National timing" = "#2c3e50", "Exposure" = "#16a085"), name = NULL) +
    scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
    labs(x = expression("Assumed correlation of credit with the error, " * rho[xu]), y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold")),
  width = 9, height = 4.2, dpi = 300)
cat("\nDone.\n")
