# =============================================================================
# 02b_break_predictive_tests.R  --  break tests and predictive tests
#
# Reads the files that 01a_build_test_panel.R writes (clean/panel_tests and
# clean/panel_tests_stacked, .rds or .csv) and runs three sets of tests on the
# five-city panel:
#   A. Break tests on the credit slope, in the price and the rent equation:
#      an unknown-date test (sup-Wald), and dated breaks at 2015 Q1 (APRA
#      investor benchmark), 2017 Q1 (the paper's break) and 2017 Q2 (APRA
#      interest-only cap).
#   B. Granger tests in both directions: does credit growth predict price and
#      rent growth, and do price and rent growth predict credit growth?
#   C. Stacked tests: do the price and rent equations' credit slopes, and the
#      change in each slope after a break, differ?
#
# Run from the folder that holds clean/ (the repository root), after
# 01a_build_test_panel.R:
#   Rscript 02b_break_predictive_tests.R
# It does not need 00_setup.R, and it takes under a minute.
#
# Writes to output/:
#   tests_break_dated.csv   dated breaks: every measure, timing and sample
#   tests_break_sup.csv     unknown-date test: largest Wald, its date, critical values
#   tests_break_path.csv    Wald statistic at every candidate date (for a figure)
#   tests_granger.csv       pooled Granger tests, Driscoll-Kraay Wald
#   tests_granger_dh.csv    Dumitrescu-Hurlin tests, with city-by-city results
#   tests_stacked.csv       price-rent comparisons
#   tests_summary.txt       the main numbers, also printed to the console
#   table_break_tests.tex   the paper's table of break tests (main measure)
#   table_granger.tex       the paper's table of Granger tests (main measure)
#
# Design, as agreed on 8 October 2026:
#   - Credit measures: seasonally adjusted investor commitments (main); the
#     original series used in the earlier version (continuity); and the change in
#     the seasonally adjusted investor share of new lending.
#   - The same controls in the price and rent equations: the cash-rate change,
#     lagged tightness, wage growth and employment growth. With lagged credit,
#     the cash-rate change is lagged too, as in the paper.
#   - City fixed effects and Driscoll-Kraay errors with 4 lags, as in the paper.
#   - Each break regression includes a regime intercept and tests the change in
#     the credit slope, one restriction, as the paper's divergence test does.
#   - The unknown-date test compares the largest Wald statistic with Andrews'
#     (1993, 2003) critical values for one restriction and 15 percent trimming.
#     The data keep 12 of 74 quarters out of each end (16 percent), so these
#     values are slightly conservative.
#   - Granger regressions with city fixed effects add quarter-of-year dummies,
#     so the original series' seasonal pattern cannot pass for prediction. The
#     two-way version (city and quarter effects) removes every national shock,
#     leaving only differences between cities.
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("02b_break_predictive_tests")

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

MEASURES <- c(main = "dl_inv_sa", original = "dl_inv", share = "d_inv_share_sa")
CONTROLS <- list(current = c("d_cash",    "tight_l1", "dl_wpi", "dl_emp"),
                 lagged  = c("d_cash_l1", "tight_l1", "dl_wpi", "dl_emp"))
OUTCOMES <- c(price = "dl_price", rent = "dl_rent")
BREAKS   <- c(post_2015q1 = "2015 Q1", post_2017q1 = "2017 Q1", post_2017q2 = "2017 Q2")
GRANGER_P <- c(2, 4, 6)   # lag orders; the summary reports 4
DK_LAG    <- 4
CV_SUP    <- c(`10%` = 7.12, `5%` = 8.68, `1%` = 12.16)

# Samples for the dated break tests. Each drops quarters that a flag marks.
VARIANTS <- list(
  full       = function(d) d,
  no_reclass = function(d) filter(d, flag_reclass == 0),   # 2015 Q3 to 2016 Q2
  no_covid   = function(d) filter(d, flag_covid == 0),     # 2020 Q2 to 2021 Q4
  to_2021q4  = function(d) filter(d, quarter <= as.yearqtr("2021 Q4")),  # before the price splice
  no_cra     = function(d) filter(d, flag_cra == 0)        # CPI rent dips, 2023-24
)

# ---- Data ----------------------------------------------------------------------
read_panel <- function(name) {
  f_rds <- file.path(CLEAN, paste0(name, ".rds"))
  f_csv <- file.path(CLEAN, paste0(name, ".csv"))
  if (file.exists(f_rds)) return(readRDS(f_rds))
  if (file.exists(f_csv))
    return(read_csv(f_csv, show_col_types = FALSE) |> mutate(quarter = as.yearqtr(quarter)))
  stop("Neither ", f_rds, " nor ", f_csv, " was found. Run 01a_build_test_panel.R first.")
}
tp  <- read_panel("panel_tests")
stk <- read_panel("panel_tests_stacked")

lag_cols <- function(v, p) paste0(v, "_l", seq_len(p))
need <- c(OUTCOMES, MEASURES, paste0(MEASURES, "_l1"), unlist(CONTROLS), names(BREAKS),
          lag_cols(c(OUTCOMES, MEASURES), max(GRANGER_P)),
          "city", "quarter", "qid", "qtr", "s_base", "s_granger", "break_candidate",
          "flag_reclass", "flag_covid", "flag_cra")
missing_cols <- setdiff(need, names(tp))
if (length(missing_cols))
  stop("panel_tests lacks: ", paste(missing_cols, collapse = ", "),
       ". Rerun 01a_build_test_panel.R.")

base <- filter(tp, s_base == 1)       # the paper's 370 rows
gr   <- filter(tp, s_granger == 1)    # 355 rows that hold lags up to 6
sb   <- filter(stk, s_base == 1)      # stacked: 2 x 370 rows

# ---- Helpers -------------------------------------------------------------------
dk_vcov <- function(m) plm::vcovSCC(m, type = "HC0", maxlag = DK_LAG)

# Within (fixed-effects) regression of y on the listed terms.
fit <- function(d, y, terms, index = c("city", "qid"), effect = "individual") {
  f <- as.formula(paste(y, "~", paste(terms, collapse = " + ")))
  plm(f, data = pdata.frame(as.data.frame(d), index = index),
      model = "within", effect = effect)
}

# Driscoll-Kraay Wald test that the named coefficients are all zero.
wald <- function(m, V, terms) {
  b <- coef(m)[terms]
  W <- as.numeric(crossprod(b, solve(V[terms, terms, drop = FALSE], b)))
  c(wald = W, df = length(terms), p = pchisq(W, length(terms), lower.tail = FALSE))
}

# Estimate and standard error of the sum of the named coefficients.
lin_comb <- function(m, V, terms)
  c(est = sum(coef(m)[terms]), se = sqrt(sum(V[terms, terms, drop = FALSE])))

# Run expr; on an error, report it and return NULL so the loop goes on.
try_fit <- function(expr, label) {
  tryCatch(expr, error = function(e) {
    message("  skipped (", label, "): ", conditionMessage(e)); NULL
  })
}

credit_var <- function(measure, timing)
  if (timing == "lagged") paste0(MEASURES[[measure]], "_l1") else MEASURES[[measure]]

# ---- A. Break tests -------------------------------------------------------------
# y = city effects + b*x + d*(x*D) + g*D + controls + e, with D = 1 from the
# break quarter on. The test is d = 0: no change in the credit slope.
break_fit <- function(d, y, x_var, ctrl, D_var) {
  d <- d |> mutate(x = .data[[x_var]], D = .data[[D_var]], xD = x * D)
  m <- fit(d, y, c("x", "xD", "D", ctrl))
  V <- dk_vcov(m)
  w <- wald(m, V, "xD")
  after <- lin_comb(m, V, c("x", "xD"))
  tibble(n = nobs(m),
         slope_before = coef(m)[["x"]],  se_before = sqrt(V["x", "x"]),
         change       = coef(m)[["xD"]], se_change = sqrt(V["xD", "xD"]),
         slope_after  = after[["est"]],  se_after  = after[["se"]],
         wald = w[["wald"]], p = w[["p"]])
}

message("A. Dated breaks ...")
grid_a <- expand_grid(outcome = names(OUTCOMES), measure = names(MEASURES),
                      timing = names(CONTROLS), break_var = names(BREAKS),
                      variant = names(VARIANTS))
dated <- bind_rows(lapply(seq_len(nrow(grid_a)), function(i) {
  g <- grid_a[i, ]
  res <- try_fit(break_fit(VARIANTS[[g$variant]](base), OUTCOMES[[g$outcome]],
                           credit_var(g$measure, g$timing), CONTROLS[[g$timing]],
                           g$break_var),
                 paste(g, collapse = "/"))
  if (is.null(res)) return(NULL)
  bind_cols(g, break_date = BREAKS[[g$break_var]], res)
}))
write_csv(dated, file.path(OUTDIR, "tests_break_dated.csv"))

message("A. Unknown-date test ...")
cand <- sort(unique(base$quarter[base$break_candidate == 1]))
grid_u <- expand_grid(outcome = names(OUTCOMES), measure = names(MEASURES),
                      timing = names(CONTROLS))
path <- bind_rows(lapply(seq_len(nrow(grid_u)), function(i) {
  g <- grid_u[i, ]
  bind_rows(lapply(cand, function(tau) {
    d <- base |> mutate(D_tau = as.integer(quarter >= tau))
    res <- try_fit(break_fit(d, OUTCOMES[[g$outcome]], credit_var(g$measure, g$timing),
                             CONTROLS[[g$timing]], "D_tau"),
                   paste(c(g, format(tau)), collapse = "/"))
    if (is.null(res)) return(NULL)
    bind_cols(g, quarter = format(tau), select(res, change, se_change, wald, p))
  }))
}))
write_csv(path, file.path(OUTDIR, "tests_break_path.csv"))

sup <- path |>
  group_by(outcome, measure, timing) |>
  summarise(sup_wald = max(wald), sup_date = quarter[which.max(wald)],
            change_at_sup = change[which.max(wald)],
            n_dates = n(), first_date = first(quarter), last_date = last(quarter),
            .groups = "drop") |>
  mutate(cv_10 = CV_SUP[["10%"]], cv_5 = CV_SUP[["5%"]], cv_1 = CV_SUP[["1%"]],
         reject = case_when(sup_wald > cv_1  ~ "1%",
                            sup_wald > cv_5  ~ "5%",
                            sup_wald > cv_10 ~ "10%",
                            TRUE             ~ "no"),
         # A maximum at the first or last candidate compares the edge of the
         # sample with the rest, not a break inside it.
         at_edge = sup_date %in% c(first_date, last_date))
write_csv(sup, file.path(OUTDIR, "tests_break_sup.csv"))

# ---- B. Granger tests ------------------------------------------------------------
# dep on p of its own lags and p lags of cause; the test is that the cause
# lags are all zero. sum_lags is the sum of the cause lags' coefficients.
granger_fit <- function(d, dep, cause, p, effect) {
  own <- lag_cols(dep, p); other <- lag_cols(cause, p)
  terms <- c(own, other)
  if (effect == "individual") terms <- c(terms, "factor(qtr)")
  m <- fit(d, dep, terms, effect = effect)
  V <- dk_vcov(m)
  w <- wald(m, V, other)
  s <- lin_comb(m, V, other)
  tibble(n = nobs(m), wald = w[["wald"]], df = w[["df"]], p = w[["p"]],
         sum_lags = s[["est"]], se_sum = s[["se"]])
}

message("B. Granger tests ...")
grid_b <- expand_grid(outcome = names(OUTCOMES), measure = names(MEASURES),
                      direction = c("credit_to_outcome", "outcome_to_credit"),
                      lags = GRANGER_P, effects = c("city", "city_and_quarter"))
granger <- bind_rows(lapply(seq_len(nrow(grid_b)), function(i) {
  g <- grid_b[i, ]
  y <- OUTCOMES[[g$outcome]]; x <- MEASURES[[g$measure]]
  dep   <- if (g$direction == "credit_to_outcome") y else x
  cause <- if (g$direction == "credit_to_outcome") x else y
  eff   <- if (g$effects == "city") "individual" else "twoways"
  res <- try_fit(granger_fit(gr, dep, cause, g$lags, eff), paste(g, collapse = "/"))
  if (is.null(res)) return(NULL)
  bind_cols(g, res)
}))
write_csv(granger, file.path(OUTDIR, "tests_granger.csv"))

# Dumitrescu-Hurlin: a Granger test city by city, pooled into one statistic.
# Its p-value assumes many cities, so with five read it beside the city results.
# It cannot take quarter dummies; for the original series, seasonality may bias it.
message("B. Dumitrescu-Hurlin tests ...")
grid_dh <- expand_grid(outcome = names(OUTCOMES), measure = names(MEASURES),
                       direction = c("credit_to_outcome", "outcome_to_credit"))
dh <- bind_rows(lapply(seq_len(nrow(grid_dh)), function(i) {
  g <- grid_dh[i, ]
  y <- OUTCOMES[[g$outcome]]; x <- MEASURES[[g$measure]]
  dep   <- if (g$direction == "credit_to_outcome") y else x
  cause <- if (g$direction == "credit_to_outcome") x else y
  d <- tp |> filter(is.finite(.data[[dep]]), is.finite(.data[[cause]]))
  res <- try_fit(pgrangertest(as.formula(paste(dep, "~", cause)),
                              data = pdata.frame(as.data.frame(d), index = c("city", "qid")),
                              order = 4L),
                 paste(g, collapse = "/"))
  if (is.null(res)) return(NULL)
  ind <- res$indgranger
  bind_cols(g, tibble(z_tilde = unname(res$statistic), p = unname(res$p.value),
                      cities_p05 = sum(ind[["p-value"]] < 0.05),
                      city_p = paste(sprintf("%s %.3f", ind$city, ind[["p-value"]]),
                                     collapse = "; ")))
}))
write_csv(dh, file.path(OUTDIR, "tests_granger_dh.csv"))

# ---- C. Stacked price-rent tests ---------------------------------------------------
# Two rows per city-quarter, every term interacted with is_rent, fixed effects
# for each city and equation. Driscoll-Kraay errors let the price and rent
# errors of a quarter be correlated. r_x is the rent slope minus the price
# slope; r_xD is the rent change minus the price change after the break.
stack_fit <- function(d, x_var, ctrl, D_var) {
  d <- d |> mutate(x = .data[[x_var]], D = .data[[D_var]], xD = x * D)
  terms <- c("x", "xD", "D", ctrl)
  for (v in terms) d[[paste0("r_", v)]] <- d$is_rent * d[[v]]
  m <- fit(d, "y", c(terms, paste0("r_", terms)), index = c("unit", "qid"))
  V <- dk_vcov(m)
  rent_slope  <- lin_comb(m, V, c("x", "r_x"))
  rent_change <- lin_comb(m, V, c("xD", "r_xD"))
  w_slope  <- wald(m, V, "r_x")
  w_change <- wald(m, V, "r_xD")
  w_joint  <- wald(m, V, c("r_x", "r_xD"))
  tibble(n = nobs(m),
         price_slope = coef(m)[["x"]],    rent_slope = rent_slope[["est"]],
         diff_slope  = coef(m)[["r_x"]],  se_diff_slope = sqrt(V["r_x", "r_x"]),
         p_slope     = w_slope[["p"]],
         price_change = coef(m)[["xD"]],  rent_change = rent_change[["est"]],
         diff_change  = coef(m)[["r_xD"]], se_diff_change = sqrt(V["r_xD", "r_xD"]),
         p_change     = w_change[["p"]],
         p_joint      = w_joint[["p"]])
}

message("C. Stacked tests ...")
grid_c <- expand_grid(measure = names(MEASURES), timing = names(CONTROLS),
                      break_var = names(BREAKS))
stacked <- bind_rows(lapply(seq_len(nrow(grid_c)), function(i) {
  g <- grid_c[i, ]
  res <- try_fit(stack_fit(sb, credit_var(g$measure, g$timing), CONTROLS[[g$timing]],
                           g$break_var),
                 paste(g, collapse = "/"))
  if (is.null(res)) return(NULL)
  bind_cols(g, break_date = BREAKS[[g$break_var]], res)
}))
write_csv(stacked, file.path(OUTDIR, "tests_stacked.csv"))

# ---- Summary -----------------------------------------------------------------------
f3 <- function(x) sprintf("%.3f", x)
est_p <- function(est, p) sprintf("%.3f (p %.3f)", est, p)

# The sink starts inside tryCatch, so source(echo = TRUE) does not copy the
# code into the summary.
con <- file(file.path(OUTDIR, "tests_summary.txt"), open = "wt")
old_width <- options(width = 250)   # keep wide tables on one line
tryCatch({
  sink(con, split = TRUE)
  cat("Break and predictive tests,", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat(sprintf("Samples: %d city-quarters for the break and stacked tests (%s to %s); %d for Granger (%s to %s)\n",
              nrow(base), format(min(base$quarter)), format(max(base$quarter)),
              nrow(gr), format(min(gr$quarter)), format(max(gr$quarter))))
  cat("Measures: main = seasonally adjusted investor commitments; original = the published series;\n",
      "         share = change in the seasonally adjusted investor share (per 1.00 change; 0.01 = one point)\n",
      "Controls in both equations: cash-rate change, lagged tightness, wage growth, employment growth.\n",
      "Driscoll-Kraay errors with ", DK_LAG, " lags; (p ...) gives the p-value.\n", sep = "")

  cat("\nA1. Unknown-date test on the credit slope (full sample)\n")
  cat(sprintf("    Candidate breaks %s to %s; critical values 7.12 (10%%), 8.68 (5%%), 12.16 (1%%)\n",
              format(min(cand)), format(max(cand))))
  print(as.data.frame(sup |> transmute(outcome, measure, timing,
                                       sup_wald = f3(sup_wald), date = sup_date,
                                       change = f3(change_at_sup), reject,
                                       at_edge = ifelse(at_edge, "yes", ""))),
        row.names = FALSE)
  cat("    at_edge = the largest statistic falls on the first or last candidate date, so the\n",
      "    test compares the start or end of the sample with the rest. See tests_break_path.csv.\n",
      sep = "")

  cat("\nA2. Change in the credit slope at the dated breaks (full sample)\n")
  a2 <- dated |>
    filter(variant == "full") |>
    transmute(outcome, measure, timing, break_date,
              cell = sprintf("%s -> %s, change %s",
                             f3(slope_before), f3(slope_after), est_p(change, p))) |>
    pivot_wider(names_from = break_date, values_from = cell)
  print(as.data.frame(a2), row.names = FALSE)

  cat("\nA3. Main measure, price equation, current-quarter credit: change by sample\n")
  a3 <- dated |>
    filter(measure == "main", outcome == "price", timing == "current") |>
    transmute(variant, break_date, cell = est_p(change, p)) |>
    pivot_wider(names_from = break_date, values_from = cell)
  print(as.data.frame(a3), row.names = FALSE)
  cat("    The same for the rent equation:\n")
  a3r <- dated |>
    filter(measure == "main", outcome == "rent", timing == "current") |>
    transmute(variant, break_date, cell = est_p(change, p)) |>
    pivot_wider(names_from = break_date, values_from = cell)
  print(as.data.frame(a3r), row.names = FALSE)

  cat("\nB1. Granger tests, 4 lags, city effects and quarter dummies\n")
  cat("    Wald test that the 4 lags of the cause are zero; sum of their coefficients\n")
  b1 <- granger |>
    filter(lags == 4, effects == "city") |>
    mutate(test = ifelse(direction == "credit_to_outcome",
                         paste("credit ->", outcome), paste(outcome, "-> credit")),
           cell = sprintf("W %.1f (p %.3f), sum %s", wald, p, f3(sum_lags))) |>
    select(test, measure, cell) |>
    pivot_wider(names_from = measure, values_from = cell)
  print(as.data.frame(b1), row.names = FALSE)

  cat("\nB2. Granger p-values by lag order and effects\n")
  b2 <- granger |>
    mutate(test = ifelse(direction == "credit_to_outcome",
                         paste("credit ->", outcome), paste(outcome, "-> credit")),
           col = paste0(ifelse(effects == "city", "city", "two-way"), " p=", lags)) |>
    select(test, measure, col, p) |>
    mutate(p = f3(p)) |>
    pivot_wider(names_from = col, values_from = p)
  print(as.data.frame(b2), row.names = FALSE)

  cat("\nB3. Dumitrescu-Hurlin, 4 lags: Z-tilde, p-value and cities with p < 0.05\n")
  print(as.data.frame(dh |>
    transmute(test = ifelse(direction == "credit_to_outcome",
                            paste("credit ->", outcome), paste(outcome, "-> credit")),
              measure, z_tilde = f3(z_tilde), p = f3(p), cities_p05)),
    row.names = FALSE)

  cat("\nC. Stacked price-rent tests: rent minus price\n")
  cat("   price_slope and rent_slope are the credit slopes before the break; diff_slope = rent minus price;\n",
      "   diff_change = rent minus price in the change after the break; p_joint tests both differences\n",
      sep = "")
  print(as.data.frame(stacked |>
    transmute(measure, timing, break_date,
              price_slope = f3(price_slope), rent_slope = f3(rent_slope),
              diff_slope = est_p(diff_slope, p_slope),
              diff_change = est_p(diff_change, p_change),
              p_joint = f3(p_joint))),
    row.names = FALSE)

  cat("\nFull results: output/tests_break_dated.csv, tests_break_sup.csv, tests_break_path.csv,\n",
      "tests_granger.csv, tests_granger_dh.csv and tests_stacked.csv\n", sep = "")
}, finally = {
  sink()
  close(con)
  options(old_width)
})

# ---- LaTeX tables for the paper (added 26 October 2026) -------------------------------
# Two tables, main credit measure, full sample, written in full (caption, label,
# notes) so they can be pasted into the manuscript as they are:
#   table_break_tests.tex  dated breaks, the stacked rent-minus-price difference,
#                          and the unknown-date test
#   table_granger.tex      Granger tests in both directions
fmt3  <- function(v) ifelse(v < 0, sprintf("$-$%.3f", abs(v)), sprintf("%.3f", v))
fmtse <- function(v) sprintf("(%.3f)", v)
fmtp  <- function(p) ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
star  <- function(p) ifelse(p < 0.01, "\\sym{***}", ifelse(p < 0.05, "\\sym{**}",
                            ifelse(p < 0.10, "\\sym{*}", "")))
star_sup <- function(w) ifelse(w > CV_SUP[["1%"]], "\\sym{***}", ifelse(w > CV_SUP[["5%"]], "\\sym{**}",
                               ifelse(w > CV_SUP[["10%"]], "\\sym{*}", "")))
qfmt  <- function(q) sub(" ", ":", format(as.yearqtr(q)))      # "2017 Q1" -> "2017:Q1"
row_tex <- function(label, cells) paste0(label, " & ", paste(cells, collapse = " & "), " \\\\")

# Table: break tests. Columns: price and rent equations, then rent minus price,
# each with current-quarter and lagged credit.
COLS_EQ <- expand_grid(outcome = c("price", "rent"), timing = c("current", "lagged"))
BREAK_ROWS <- c(post_2015q1 = "2015:Q1, investor benchmark",
                post_2017q1 = "2017:Q1, break in Table~\\ref{tab:divergence}",
                post_2017q2 = "2017:Q2, interest-only cap")
dated_main <- filter(dated, measure == "main", variant == "full")
stk_main   <- filter(stacked, measure == "main")
sup_main   <- filter(sup, measure == "main")

tab_b <- c(
  "\\begin{table}[t]",
  "\\centering",
  "\\footnotesize",
  "\\begin{threeparttable}",
  "\\caption{Break tests on the investor-credit slope}",
  "\\label{tab:break_tests}",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lcccccc@{}}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Price equation} & \\multicolumn{2}{c}{Rent equation} & \\multicolumn{2}{c}{Rent minus price} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  " & Current & Lagged & Current & Lagged & Current & Lagged \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{Panel A. Change in the credit slope at a dated break}} \\\\")
for (bv in names(BREAK_ROWS)) {
  eq <- lapply(seq_len(nrow(COLS_EQ)), function(j)
    filter(dated_main, outcome == COLS_EQ$outcome[j], timing == COLS_EQ$timing[j], break_var == bv))
  st <- lapply(c("current", "lagged"), function(tm) filter(stk_main, timing == tm, break_var == bv))
  est_cells <- c(sapply(eq, function(r) paste0(fmt3(r$change), star(r$p))),
                 sapply(st, function(r) paste0(fmt3(r$diff_change), star(r$p_change))))
  se_cells  <- c(sapply(eq, function(r) fmtse(r$se_change)),
                 sapply(st, function(r) fmtse(r$se_diff_change)))
  tab_b <- c(tab_b, row_tex(BREAK_ROWS[[bv]], est_cells), row_tex("", se_cells))
}
# Panel B rows, in the column order of COLS_EQ; the rent-minus-price columns stay empty.
sup_rows <- bind_rows(lapply(seq_len(nrow(COLS_EQ)), function(j)
  filter(sup_main, outcome == COLS_EQ$outcome[j], timing == COLS_EQ$timing[j])))
stopifnot(nrow(sup_rows) == nrow(COLS_EQ))
sup_wald_cells   <- paste0(sprintf("%.1f", sup_rows$sup_wald), star_sup(sup_rows$sup_wald))
sup_date_cells   <- paste0(qfmt(sup_rows$sup_date), ifelse(sup_rows$at_edge, "$^{a}$", ""))
sup_change_cells <- fmt3(sup_rows$change_at_sup)
tab_b <- c(tab_b,
  "\\addlinespace",
  sprintf("\\multicolumn{7}{l}{\\emph{Panel B. Break at an unknown date, %s to %s}} \\\\",
          qfmt(min(cand)), qfmt(max(cand))),
  row_tex("Largest Wald statistic", c(sup_wald_cells, "", "")),
  row_tex("Date of the largest statistic", c(sup_date_cells, "", "")),
  row_tex("Change in the credit slope at that date", c(sup_change_cells, "", "")),
  "\\bottomrule",
  "\\end{tabular*}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Main credit measure (seasonally adjusted), ", nrow(base), " city-quarters. ",
         "Each regression adds to the city fixed effects an indicator that is one from the break ",
         "quarter on and its interaction with credit growth, in the current quarter or lagged one ",
         "quarter. Both equations have the same four controls: the cash-rate change (lagged with ",
         "lagged credit), lagged tightness, wage growth and employment growth. The price equation ",
         "therefore differs slightly from Table~\\ref{tab:divergence}, which omits employment ",
         "growth. Panel A: the coefficient on the interaction, that is, the change in the credit ",
         "slope after the break, with Driscoll--Kraay standard errors in parentheses. Rent minus ",
         "price: the rent change less the price change, from the two equations stacked. Panel B: ",
         "the largest Driscoll--Kraay Wald statistic for a change in the credit slope over the ",
         length(cand), " candidate quarters, against the critical values of ",
         "\\citet{andrews1993tests, andrews2003tests} for one restriction and 15 percent trimming: ",
         sprintf("%.2f (10 percent), %.2f (5 percent) and %.2f (1 percent). ",
                 CV_SUP[["10%"]], CV_SUP[["5%"]], CV_SUP[["1%"]]),
         "$^{a}$The first candidate date, so the test compares the first ",
         sum(sort(unique(base$quarter)) < min(cand)), " quarters of the sample with the rest. ",
         "\\sym{*}$p<0.10$, \\sym{**}$p<0.05$, \\sym{***}$p<0.01$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}")
writeLines(tab_b, file.path(OUTDIR, "table_break_tests.tex"))

# Table: Granger tests in both directions, main measure.
GR_COLS <- tibble(outcome   = c("price", "price", "rent", "rent"),
                  direction = c("credit_to_outcome", "outcome_to_credit",
                                "credit_to_outcome", "outcome_to_credit"),
                  head      = c("Credit $\\to$ price", "Price $\\to$ credit",
                                "Credit $\\to$ rent", "Rent $\\to$ credit"))
gm <- filter(granger, measure == "main")
gcell <- function(lags_, effects_, f) sapply(seq_len(nrow(GR_COLS)), function(j) {
  r <- filter(gm, outcome == GR_COLS$outcome[j], direction == GR_COLS$direction[j],
              lags == lags_, effects == effects_)
  f(r)
})
tab_g <- c(
  "\\begin{table}[t]",
  "\\centering",
  "\\small",
  "\\begin{threeparttable}",
  "\\caption{Granger tests in both directions}",
  "\\label{tab:granger}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Prices} & \\multicolumn{2}{c}{Rents} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  paste0(" & ", paste(GR_COLS$head, collapse = " & "), " \\\\"),
  "\\midrule",
  row_tex("Wald statistic, 4 lags", gcell(4, "city", function(r) sprintf("%.1f", r$wald))),
  row_tex("$p$-value", gcell(4, "city", function(r) fmtp(r$p))),
  row_tex("Sum of the lag coefficients", gcell(4, "city", function(r) fmt3(r$sum_lags))),
  row_tex("", gcell(4, "city", function(r) fmtse(r$se_sum))),
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\emph{$p$-value in other specifications}} \\\\",
  row_tex("2 lags", gcell(2, "city", function(r) fmtp(r$p))),
  row_tex("6 lags", gcell(6, "city", function(r) fmtp(r$p))),
  row_tex("4 lags, quarter fixed effects", gcell(4, "city_and_quarter", function(r) fmtp(r$p))),
  "\\midrule",
  row_tex("Observations", gcell(4, "city", function(r) sprintf("%d", as.integer(r$n)))),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Main credit measure (seasonally adjusted). Each column regresses the growth of the ",
         "predicted variable on four lags of itself and four lags of the predicting variable, with ",
         "city fixed effects and quarter-of-year dummies. The test is that the lags of the ",
         "predicting variable are all zero: a Driscoll--Kraay Wald test with four degrees of ",
         "freedom. The sum of their coefficients has its Driscoll--Kraay standard error in ",
         "parentheses. Every specification uses the same ", nrow(gr), " city-quarters, ",
         qfmt(min(gr$quarter)), " to ", qfmt(max(gr$quarter)), ", which hold six lags. The last ",
         "row replaces the quarter-of-year dummies with quarter fixed effects, which remove every ",
         "national shock."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}")
writeLines(tab_g, file.path(OUTDIR, "table_granger.tex"))
message("Wrote table_break_tests.tex and table_granger.tex to ", OUTDIR, "/")

end_run_log()
