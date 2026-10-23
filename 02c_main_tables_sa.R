# =============================================================================
# 02c_main_tables_sa.R  --  Tables 2, 4 and 5 on seasonally adjusted credit
#
# Rebuilds the paper's summary statistics (Table 2), price equation (Table 4)
# and divergence test (Table 5) with seasonally adjusted investor commitments,
# the paper's main credit measure, and reruns the Appendix C checks that build
# on them. Every model keeps the specification of 02_estimate.R; only the
# credit series changes. The original series runs alongside, so its results
# reproduce the published tables.
#
# Specifications (city fixed effects; Driscoll-Kraay errors, 4 lags):
#   Table 4  price growth on credit growth, the cash-rate change and lagged
#            tightness; column 2 lags credit and the cash rate one quarter.
#   Table 5  credit growth x post-2017, with a regime intercept, in the price
#            equation (cash-rate change, lagged tightness, wage growth) and the
#            rent equation (the same plus employment growth). Panel B lags
#            credit and the cash rate one quarter.
#   Appendix C checks, on the Table 5 price equation unless stated:
#     pandemic        credit x post-2020 added, with its intercept
#     Sydney          credit x post-2017 x Sydney added; Sydney's total slope
#     national share  growth of the national investor share of housing credit
#                     (RBA D2) added as a control
#     switching flow  net loan-purpose switching (RBA D2) added, on the window
#                     where it is observed, against that window without it
#     spline          natural spline in credit growth (3 df) in the Table 4
#                     current-quarter equation; joint test of the curvature terms
#     wild bootstrap  restricted wild cluster bootstrap-t for the current-quarter
#                     price interaction, Webb weights, all 6^5 draws (the
#                     02_estimate.R design: no regime intercept)
#   A variant of Table 5 with the same four controls in both equations (the
#   design of 02b_break_predictive_tests.R) is reported for comparison.
#
# Reads clean/panel_tests (.rds or .csv), written by 01a_build_test_panel.R.
# Run from the folder that holds clean/ (the repository root):
#   Rscript 02c_main_tables_sa.R
# It does not need 00_setup.R and runs in under a minute.
#
# Writes to output/:
#   main_tables_estimates.csv  every estimate, both credit series
#   main_tables_summary.txt    the tables and checks side by side
#   table_est_summary.tex      Table 2 rows for the adjusted series
#   table_price_eq.tex         Table 4, adjusted series
#   table_divergence.tex       Table 5, adjusted series
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("02c_main_tables_sa")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(zoo); library(plm); library(splines)
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

MEASURES      <- c(main = "dl_inv_sa", original = "dl_inv")
TABLE_MEASURE <- "main"           # the series the .tex tables use
DK_LAG        <- 4
POST          <- "post_2017q1"
COVID         <- "post_2020q1"
CTRL <- list(price     = c("d_cash",    "tight_l1", "dl_wpi"),
             rent      = c("d_cash",    "tight_l1", "dl_wpi", "dl_emp"),
             price_lag = c("d_cash_l1", "tight_l1", "dl_wpi"),
             rent_lag  = c("d_cash_l1", "tight_l1", "dl_wpi", "dl_emp"))
WEBB <- c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2))

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

need <- c("city", "quarter", "qid", "s_base", "dl_price", "dl_rent", "dl_wpi", "dl_emp",
          "d_cash", "d_cash_l1", "tight_l1", "tightness", "price_rent", "d_inv_share_sa",
          unname(MEASURES), paste0(MEASURES, "_l1"), POST, COVID,
          "inv_credit_nat", "oo_credit_nat", "net_switch_nat", "flag_reclass", "qtr")
missing_cols <- setdiff(need, names(tp))
if (length(missing_cols))
  stop("panel_tests lacks: ", paste(missing_cols, collapse = ", "),
       ". Rerun 01a_build_test_panel.R.")

tp <- tp |>
  mutate(quarter = as.yearqtr(quarter)) |>
  arrange(city, quarter) |>
  group_by(city) |>
  mutate(nat_share    = inv_credit_nat / (inv_credit_nat + oo_credit_nat),
         dl_nat_share = log(nat_share) - lag(log(nat_share))) |>
  ungroup() |>
  mutate(syd = as.integer(city == "Sydney"),
         gap = log(price_rent))
base <- filter(tp, s_base == 1)   # the 370 estimation rows, 2007 Q3 to 2025 Q4

# ---- Helpers -------------------------------------------------------------------
fit <- function(d, y, terms)
  plm(as.formula(paste(y, "~", paste(terms, collapse = " + "))),
      data = pdata.frame(as.data.frame(d), index = c("city", "qid")), model = "within")
dk_vcov <- function(m) plm::vcovSCC(m, type = "HC0", maxlag = DK_LAG)

est <- function(m, term, V = dk_vcov(m)) {
  b <- unname(coef(m)[term]); se <- sqrt(V[term, term])
  c(est = b, se = se, p = 2 * pnorm(-abs(b / se)))
}
lin_comb <- function(m, terms, V = dk_vcov(m)) {
  b <- sum(coef(m)[terms]); se <- sqrt(sum(V[terms, terms]))
  c(est = b, se = se, p = 2 * pnorm(-abs(b / se)), chi2 = (b / se)^2)
}
wald <- function(m, terms, V = dk_vcov(m)) {
  b <- coef(m)[terms]; W <- as.numeric(crossprod(b, solve(V[terms, terms], b)))
  c(chi2 = W, df = length(terms), p = pchisq(W, length(terms), lower.tail = FALSE))
}
r2w <- function(m) unname(summary(m)$r.squared["rsq"])

# Restricted wild cluster bootstrap-t, Webb weights, every one of the 6^G draws.
# The p-value compares |t*| with |t|; both use the same cluster-robust variance,
# so the usual small-sample factor cancels.
wild_webb <- function(d, y, terms, test) {
  X  <- model.matrix(as.formula(paste("~", paste(c(terms, "factor(city)"), collapse = " + "))), d)
  yv <- d[[y]]; k <- match(test, colnames(X)); cl <- as.integer(factor(d$city)); G <- max(cl)
  XtXi <- solve(crossprod(X))
  t_of <- function(U, B) {             # t statistics for each column of residuals U
    V <- 0
    for (g in seq_len(G)) {
      w <- XtXi[k, , drop = FALSE] %*% crossprod(X[cl == g, , drop = FALSE], U[cl == g, , drop = FALSE])
      V <- V + w^2
    }
    B[k, ] / sqrt(V)
  }
  b   <- XtXi %*% crossprod(X, yv)
  t0  <- t_of(matrix(yv - X %*% b), b)
  Xr  <- X[, -k, drop = FALSE]
  fr  <- as.vector(Xr %*% solve(crossprod(Xr), crossprod(Xr, yv))); ur <- yv - fr
  grid <- as.matrix(expand.grid(rep(list(WEBB), G)))
  Ys  <- fr + ur * t(grid[, cl])                     # N x 6^G bootstrap outcomes
  Bs  <- XtXi %*% crossprod(X, Ys)
  ts  <- t_of(Ys - X %*% Bs, Bs)
  c(t = as.numeric(t0), p = mean(abs(ts) >= abs(as.numeric(t0))), draws = nrow(grid))
}

# ---- Estimates -----------------------------------------------------------------
rows <- list()
add <- function(measure, block, model, term, v, n = NA, r2 = NA)
  rows[[length(rows) + 1]] <<- tibble(measure, block, model, term,
                                      est = v[["est"]], se = v[["se"]], p = v[["p"]],
                                      n = n, r2 = r2)

stats <- list(); checks <- list()
for (m in names(MEASURES)) {
  x <- MEASURES[[m]]; xl <- paste0(x, "_l1")
  d <- base |> mutate(cr = .data[[x]], cr_l1 = .data[[xl]],
                      ix = cr * .data[[POST]], ixl = cr_l1 * .data[[POST]],
                      ixc = cr * .data[[COVID]], ixs = cr * .data[[POST]] * syd)

  # Table 4: price equation
  t4c <- fit(d, "dl_price", c("cr", "d_cash", "tight_l1"))
  t4l <- fit(d, "dl_price", c("cr_l1", "d_cash_l1", "tight_l1"))
  for (tm in c("cr", "d_cash", "tight_l1")) add(m, "table4", "current", tm, est(t4c, tm), nobs(t4c), r2w(t4c))
  for (tm in c("cr_l1", "d_cash_l1", "tight_l1")) add(m, "table4", "lagged", tm, est(t4l, tm), nobs(t4l), r2w(t4l))

  # Table 5: divergence test
  p5a <- fit(d, "dl_price", c("cr", "ix", POST, CTRL$price))
  r5a <- fit(d, "dl_rent",  c("cr", "ix", POST, CTRL$rent))
  p5b <- fit(d, "dl_price", c("cr_l1", "ixl", POST, CTRL$price_lag))
  r5b <- fit(d, "dl_rent",  c("cr_l1", "ixl", POST, CTRL$rent_lag))
  add(m, "table5", "A price", "ix",  est(p5a, "ix"),  nobs(p5a))
  add(m, "table5", "A rent",  "ix",  est(r5a, "ix"),  nobs(r5a))
  add(m, "table5", "B price", "ixl", est(p5b, "ixl"), nobs(p5b))
  add(m, "table5", "B rent",  "ixl", est(r5b, "ixl"), nobs(r5b))
  add(m, "table5", "A price", "cr",  est(p5a, "cr"),  nobs(p5a))
  add(m, "table5", "A rent",  "cr",  est(r5a, "cr"),  nobs(r5a))
  add(m, "table5", "B price", "cr_l1", est(p5b, "cr_l1"), nobs(p5b))
  add(m, "table5", "B rent",  "cr_l1", est(r5b, "cr_l1"), nobs(r5b))

  # Table 5 with the same four controls in both equations (02b design)
  p5s <- fit(d, "dl_price", c("cr", "ix", POST, CTRL$rent))
  p5sl <- fit(d, "dl_price", c("cr_l1", "ixl", POST, CTRL$rent_lag))
  add(m, "table5_same_controls", "A price", "ix",  est(p5s, "ix"),   nobs(p5s))
  add(m, "table5_same_controls", "B price", "ixl", est(p5sl, "ixl"), nobs(p5sl))

  # Appendix C: pandemic and Sydney
  pc <- fit(d, "dl_price", c("cr", "ix", "ixc", CTRL$price, POST, COVID))
  add(m, "pandemic", "price", "ix",  est(pc, "ix"))
  add(m, "pandemic", "price", "ixc", est(pc, "ixc"))
  ps <- fit(d, "dl_price", c("cr", "ix", "ixs", CTRL$price, POST))
  add(m, "sydney", "price", "ix (other capitals)", est(ps, "ix"))
  add(m, "sydney", "price", "ixs (Sydney minus others)", est(ps, "ixs"))
  syd_total <- lin_comb(ps, c("ix", "ixs"))
  add(m, "sydney", "price", "Sydney total post-2017 slope", syd_total)

  # Appendix C: national share control and switching flow
  pn <- fit(d, "dl_price", c("cr", "ix", POST, "dl_nat_share", CTRL$price))
  add(m, "national_share", "price", "ix", est(pn, "ix"), nobs(pn))
  dsw <- filter(d, is.finite(net_switch_nat))
  psw0 <- fit(dsw, "dl_price", c("cr", "ix", POST, CTRL$price))
  psw1 <- fit(dsw, "dl_price", c("cr", "ix", POST, "net_switch_nat", CTRL$price))
  add(m, "switching", "price, same window", "ix", est(psw0, "ix"), nobs(psw0))
  add(m, "switching", "price, + switching flow", "ix", est(psw1, "ix"), nobs(psw1))
  sw_w <- wald(psw1, "net_switch_nat")

  # Appendix C: spline in credit growth (basis on every non-missing quarter)
  ok <- is.finite(tp[[x]])
  B <- matrix(NA_real_, nrow(tp), 3, dimnames = list(NULL, c("sp1", "sp2", "sp3")))
  B[ok, ] <- ns(tp[[x]][ok], df = 3)
  dsp <- bind_cols(tp, as.data.frame(B)) |> filter(s_base == 1) |> mutate(cr = .data[[x]])
  m_lin <- fit(dsp, "dl_price", c("cr", "d_cash", "tight_l1"))
  m_spl <- fit(dsp, "dl_price", c("cr", "sp2", "sp3", "d_cash", "tight_l1"))
  spl_w <- wald(m_spl, c("sp2", "sp3"))

  # Spline without the extreme quarters (credit growth beyond +/-25 percent),
  # and piecewise-linear slopes by tercile of credit growth
  dsp2 <- filter(dsp, abs(cr) < 0.25)
  spl_w2 <- wald(fit(dsp2, "dl_price", c("cr", "sp2", "sp3", "d_cash", "tight_l1")), c("sp2", "sp3"))
  q3 <- quantile(d$cr, c(1/3, 2/3))
  dpw <- d |> mutate(t_lo = pmin(cr, q3[1]), t_mid = pmax(pmin(cr, q3[2]) - q3[1], 0),
                     t_hi = pmax(cr - q3[2], 0))
  m_pw <- fit(dpw, "dl_price", c("t_lo", "t_mid", "t_hi", "d_cash", "tight_l1"))
  for (tm in c("t_lo", "t_mid", "t_hi")) add(m, "spline_terciles", "price", tm, est(m_pw, tm))

  # Appendix C: wild cluster bootstrap, current-quarter price interaction, and
  # the same bootstrap for the Table 4 credit slope
  wb  <- wild_webb(d, "dl_price", c("cr", "ix", "d_cash", "tight_l1", "dl_wpi"), "ix")
  wb4 <- wild_webb(d, "dl_price", c("cr", "d_cash", "tight_l1"), "cr")

  # Table 5 price interaction without the 2015-16 reclassification quarters
  p5r <- fit(filter(d, flag_reclass == 0), "dl_price", c("cr", "ix", POST, CTRL$price))
  add(m, "no_reclass", "A price", "ix", est(p5r, "ix"), nobs(p5r))

  # Sydney: with its own credit slope throughout; and split at 2017 and 2020
  d <- d |> mutate(cr_syd = cr * syd, post_syd = .data[[POST]] * syd,
                   ix20s = ixc * syd)
  ps2 <- fit(d, "dl_price", c("cr", "ix", "cr_syd", "ixs", POST, "post_syd", CTRL$price))
  add(m, "sydney_own_slope", "price", "cr_syd (Sydney slope throughout)", est(ps2, "cr_syd"))
  add(m, "sydney_own_slope", "price", "ixs (Sydney post-2017 change)", est(ps2, "ixs"))
  ps3 <- fit(d, "dl_price", c("cr", "ix", "ixs", "ixc", "ix20s", POST, COVID, CTRL$price))
  add(m, "sydney_2017_2020", "price", "ixs (Sydney after 2017)", est(ps3, "ixs"))
  add(m, "sydney_2017_2020", "price", "ix20s (Sydney after 2020)", est(ps3, "ix20s"))

  # City by city: the post-2017 change in each city's own price-credit slope
  # (one regression per city, Newey-West errors with 4 lags)
  for (cc in sort(unique(d$city))) {
    dc <- filter(d, city == cc)
    mc <- lm(as.formula(paste("dl_price ~ cr + ix +", POST, "+", paste(CTRL$price, collapse = " + "))), data = dc)
    Vc <- sandwich::NeweyWest(mc, lag = DK_LAG, prewhite = FALSE)
    bc <- coef(mc)[["ix"]]; sc <- sqrt(Vc["ix", "ix"])
    add(m, "by_city", cc, "post-2017 change", c(est = bc, se = sc, p = 2 * pnorm(-abs(bc / sc))), nobs(mc))
    add(m, "by_city", cc, "slope before 2017", c(est = coef(mc)[["cr"]], se = sqrt(Vc["cr", "cr"]),
                                                p = 2 * pnorm(-abs(coef(mc)[["cr"]] / sqrt(Vc["cr", "cr"])))), nobs(mc))
  }

  checks[[m]] <- list(syd_total = syd_total, sw_wald = sw_w,
                      sw_window = range(dsw$quarter), sw_n = nrow(dsw),
                      spline = c(r2_lin = r2w(m_lin), r2_spl = r2w(m_spl), spl_w),
                      spline_trim = c(n = nrow(dsp2), spl_w2),
                      wild = wb, wild_slope = wb4)
}

# Seasonal noise in the original series, before and after 2017: the share of
# credit growth that quarter-of-year (and city) dummies explain, and the ratio of
# the original-series price-credit slope to the adjusted-series slope
season <- bind_rows(lapply(c(pre = 0, post = 1), function(v) {
  dd <- filter(base, .data[[POST]] == v)
  tibble(period = ifelse(v == 0, "before 2017", "from 2017"), n = nrow(dd),
         seasonal_r2_original = summary(lm(dl_inv ~ factor(qtr) + factor(city), dd))$r.squared,
         seasonal_r2_adjusted = summary(lm(dl_inv_sa ~ factor(qtr) + factor(city), dd))$r.squared,
         slope_original = coef(lm(dl_price ~ dl_inv + factor(city), dd))[["dl_inv"]],
         slope_adjusted = coef(lm(dl_price ~ dl_inv_sa + factor(city), dd))[["dl_inv_sa"]])
})) |> mutate(slope_ratio = slope_original / slope_adjusted)
res <- bind_rows(rows)
write_csv(res, file.path(OUTDIR, "main_tables_estimates.csv"))

# ---- Table 2 rows --------------------------------------------------------------
desc <- function(v) {
  v <- v[is.finite(v)]
  c(N = length(v), Mean = mean(v), SD = sd(v), Min = min(v), Max = max(v))
}
t2 <- rbind(
  "$\\Delta \\ln$ Rent"                       = desc(tp$dl_rent),
  "$\\Delta \\ln$ Price"                      = desc(tp$dl_price),
  "$\\Delta \\ln$ Investor credit"            = desc(tp$dl_inv_sa),
  "$\\Delta$ Investor share of new lending"   = desc(tp$d_inv_share_sa),
  "$\\Delta \\ln$ WPI (wages)"                = desc(tp$dl_wpi),
  "$\\Delta \\ln$ Employment"                 = desc(tp$dl_emp),
  "$\\Delta$ Cash rate"                       = desc(tp$d_cash),
  "Tightness (level)"                         = desc(tp$tightness),
  "Price--rent gap (log level)"               = desc(tp$gap))
t2_orig <- desc(tp$dl_inv)

# ---- LaTeX ---------------------------------------------------------------------
fmt  <- function(v, k = 3) ifelse(v < 0, paste0("$-$", formatC(abs(v), format = "f", digits = k)),
                                  formatC(v, format = "f", digits = k))
star <- function(p) ifelse(p < 0.01, "\\sym{***}", ifelse(p < 0.05, "\\sym{**}", ifelse(p < 0.10, "\\sym{*}", "")))
cellv <- function(b, p) paste0(fmt(b), star(p))
cells <- function(b, se, p) c(cellv(b, p), paste0("(", formatC(se, format = "f", digits = 3), ")"))
get <- function(meas, block, model, term) filter(res, measure == meas, .data$block == !!block,
                                                  .data$model == !!model, .data$term == !!term)

tex2 <- c(apply(cbind(rownames(t2), t2), 1, function(r)
  sprintf("%-42s & %s & %s & %s & %s & %s \\\\", r[1], r[2],
          fmt(as.numeric(r[3])), formatC(as.numeric(r[4]), format = "f", digits = 3),
          fmt(as.numeric(r[5])), fmt(as.numeric(r[6])))))
writeLines(tex2, file.path(OUTDIR, "table_est_summary.tex"))

tm <- TABLE_MEASURE
t4row <- function(term_c, term_l, label) {
  a <- get(tm, "table4", "current", term_c); b <- get(tm, "table4", "lagged", term_l)
  ca <- cells(a$est, a$se, a$p); cb <- cells(b$est, b$se, b$p)
  c(sprintf("%-29s & %s & %s \\\\", label, ca[1], cb[1]),
    sprintf("%-29s & %s & %s \\\\", "", ca[2], cb[2]))
}
a1 <- get(tm, "table4", "current", "cr"); b1 <- get(tm, "table4", "lagged", "cr_l1")
tex4 <- c(t4row("cr", "cr_l1", "$\\Delta\\ln$ Investor credit"),
          t4row("d_cash", "d_cash_l1", "$\\Delta$ Cash rate"),
          t4row("tight_l1", "tight_l1", "Tightness$_{t-1}$"),
          "\\midrule",
          sprintf("%-29s & %d & %d \\\\", "Observations", a1$n, b1$n),
          sprintf("%-29s & %s & %s \\\\", "Within-$R^2$",
                  formatC(a1$r2, format = "f", digits = 3), formatC(b1$r2, format = "f", digits = 3)))
writeLines(tex4, file.path(OUTDIR, "table_price_eq.tex"))

t5 <- function(model, term) { r <- get(tm, "table5", model, term); cells(r$est, r$se, r$p) }
t5rows <- function(label, p, r) c(sprintf("%-42s & %s & %s \\\\", label, p[1], r[1]),
                                  sprintf("%-42s & %s & %s \\\\", "", p[2], r[2]))
tex5 <- c("\\multicolumn{3}{l}{\\emph{Panel A. Current-quarter credit}} \\\\",
          t5rows("$\\Delta\\ln$ Investor credit", t5("A price", "cr"), t5("A rent", "cr")),
          t5rows("$\\Delta\\ln$ Investor credit $\\times$ post", t5("A price", "ix"), t5("A rent", "ix")),
          "\\addlinespace",
          "\\multicolumn{3}{l}{\\emph{Panel B. One-quarter-lagged credit}} \\\\",
          t5rows("$\\Delta\\ln$ Investor credit", t5("B price", "cr_l1"), t5("B rent", "cr_l1")),
          t5rows("$\\Delta\\ln$ Investor credit $\\times$ post", t5("B price", "ixl"), t5("B rent", "ixl")))
writeLines(tex5, file.path(OUTDIR, "table_divergence.tex"))

# ---- Summary -------------------------------------------------------------------
show <- function(df) print(as.data.frame(df |> mutate(across(c(est, se), ~ round(.x, 4)),
                                                       p = round(p, 3), r2 = round(r2, 3))),
                           row.names = FALSE)
# The sink starts inside tryCatch, so source(echo = TRUE) does not copy the
# code into the summary.
con <- file(file.path(OUTDIR, "main_tables_summary.txt"), open = "wt")
old_width <- options(width = 200)
tryCatch({
  sink(con, split = TRUE)
  cat("Tables 2, 4 and 5 and the Appendix C checks,", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("main = seasonally adjusted investor commitments; original = the published series.\n")
  cat("City fixed effects; Driscoll-Kraay errors (4 lags); 370 city-quarters unless stated.\n")
  cat("Terms: cr = credit growth; ix = credit x post-2017; ixl = lagged credit x post-2017;\n",
      "ixc = credit x post-2020; ixs = credit x post-2017 x Sydney.\n\n")

  cat("1. Table 2: credit rows (N, mean, SD, min, max)\n")
  cat("   adjusted: ", paste(round(t2[3, ], 3), collapse = "  "), "\n")
  cat("   share:    ", paste(round(t2[4, ], 4), collapse = "  "), "\n")
  cat("   original: ", paste(round(t2_orig, 3), collapse = "  "), "\n\n")

  cat("2. Table 4: price equation\n"); show(filter(res, block == "table4") |> arrange(model, measure))
  cat("\n3. Table 5: credit x post-2017 (published controls)\n")
  show(filter(res, block == "table5", term %in% c("ix", "ixl")) |> arrange(model, measure))
  cat("\n   Credit slope before 2017 in the same models\n")
  show(filter(res, block == "table5", term %in% c("cr", "cr_l1")) |> arrange(model, measure))
  cat("\n   Same four controls in both equations (02b design), price equation\n")
  show(filter(res, block == "table5_same_controls") |> arrange(model, measure))

  cat("\n4. Appendix C checks\n")
  show(filter(res, block %in% c("pandemic", "sydney", "national_share", "switching")) |>
         arrange(block, measure, model, term))
  for (m in names(checks)) {
    ck <- checks[[m]]
    cat(sprintf("\n   %s: Sydney total slope = 0: chi2(1) = %.2f, p = %.3f\n", m,
                ck$syd_total[["chi2"]], ck$syd_total[["p"]]))
    cat(sprintf("   %s: switching flow window %s to %s (%d rows); Wald on the flow: chi2(1) = %.2f, p = %.3f\n",
                m, format(ck$sw_window[1]), format(ck$sw_window[2]), ck$sw_n,
                ck$sw_wald[["chi2"]], ck$sw_wald[["p"]]))
    cat(sprintf("   %s: spline: within-R2 %.3f linear, %.3f spline; curvature chi2(2) = %.2f, p = %.3f\n",
                m, ck$spline[["r2_lin"]], ck$spline[["r2_spl"]], ck$spline[["chi2"]], ck$spline[["p"]]))
    cat(sprintf("   %s: spline without |credit growth| >= 0.25 (%d rows): curvature chi2(2) = %.2f, p = %.3f\n",
                m, ck$spline_trim[["n"]], ck$spline_trim[["chi2"]], ck$spline_trim[["p"]]))
    cat(sprintf("   %s: wild cluster bootstrap (Webb, %d draws): post-2017 price interaction t = %.2f, p = %.3f;",
                m, ck$wild[["draws"]], ck$wild[["t"]], ck$wild[["p"]]))
    cat(sprintf(" Table 4 credit slope t = %.2f, p = %.3f\n", ck$wild_slope[["t"]], ck$wild_slope[["p"]]))
  }

  cat("\n5. Further checks for Appendix C\n")
  show(filter(res, block %in% c("no_reclass", "sydney_own_slope", "sydney_2017_2020", "spline_terciles")) |>
         arrange(block, measure, term))
  cat("\n   Post-2017 change in each city's price-credit slope (one regression per city, Newey-West)\n")
  show(filter(res, block == "by_city") |> arrange(measure, term, model))

  cat("\n6. Seasonal noise in the original series, before and after 2017\n")
  print(as.data.frame(season |> mutate(across(where(is.numeric) & !n, ~ round(.x, 3)))), row.names = FALSE)
}, finally = {
  sink()
  close(con)
  options(old_width)
})
message("Wrote main_tables_estimates.csv, main_tables_summary.txt and three .tex tables to ", OUTDIR, "/")

end_run_log()
