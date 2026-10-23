# =============================================================================
# 01a_build_test_panel.R  --  data for the break tests and the predictive tests
#
# Extends stage 01 for the revision. It reads the panel that 01_build_panel.R
# writes and the investor-lending export that 01 already reads (F_LEND), and
# writes:
#   clean/panel_tests.rds          one row per city-quarter: the paper's
#                                  variables plus new credit measures, lags built
#                                  within city, regime and measurement flags,
#                                  and sample flags
#   clean/panel_tests_stacked.rds  the price and rent outcomes stacked, two rows
#                                  per city-quarter, for tests of whether the
#                                  two equations' coefficients differ
#   CSV copies of both, and output/panel_tests_checks.txt
#
# Run from the folder that holds raw/ and clean/ (the repository root):
#   Rscript 01a_build_test_panel.R
# It needs two inputs:
#   clean/panel_combined.rds, or clean/panel_combined.csv if the .rds is absent
#   raw/ABS_LEND_HOUSING_1_1_______10_20_1_2_3_4_5_6_7_8_AUS_Q.csv
# It uses 00_setup.R when that file is present and otherwise defines the few
# constants it needs (section 0), so it runs without 00_setup.R.
# It does not change the panel files, so the earlier version's tables still
# reproduce. After a data refresh, rebuild the panel with 01_build_panel.R and
# rerun this script.
#
# Choices made here, each explained where it is made:
#   1. Credit measures. The earlier version of the paper used the ORIGINAL
#      (not seasonally adjusted) value of new investor commitments. About 45
#      percent of the variance of its quarterly log change is seasonal, so the
#      file adds the seasonally adjusted value, the investor share of new
#      lending by value (Original and SA), and investor loan counts, which
#      start in 2019 Q3.
#   2. Lags are built here, within city. With `lag <- dplyr::lag` (namespace
#      block), lag() inside a plm formula shifts by row and carries the last
#      value of one city into the first rows of the next. Test scripts should
#      use the *_l1 ... *_l6 columns, not lag() in formulas.
#   3. Time index. qid counts quarters from the panel's first quarter.
#      plm::pgrangertest fails on the yearqtr index, so index tests on
#      c("city", "qid"), or c("unit", "qid") for the stacked file.
#   4. Flags mark quarters that tests should be able to drop or control for.
#
# Not changed here: the cash-rate tail in 01_build_panel.R (cash_path) uses the
# rate at the end of 2025 Q1-Q3, while the history uses quarterly averages.
# The effect on the estimates is nil; fix it when cash_path is next extended.
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("01a_build_test_panel")

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

# ---- 0. Paths and constants ------------------------------------------------------
# The same values as 00_setup.R in the replication repository. If 00_setup.R
# is in this folder, the script uses it instead.
if (file.exists("00_setup.R")) {
  source("00_setup.R")
} else {
  RAW        <- "raw"
  CLEAN      <- "clean"
  OUTDIR     <- "output"
  CITIES     <- c("Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth")
  STATE2CITY <- c(`New South Wales` = "Sydney", Victoria = "Melbourne",
                  Queensland = "Brisbane", `South Australia` = "Adelaide",
                  `Western Australia` = "Perth")
  BASE_QTR   <- as.yearqtr("2021 Q4")   # last quarter of the 6416.0 price index
  F_LEND     <- file.path(RAW, "ABS_LEND_HOUSING_1_1_______10_20_1_2_3_4_5_6_7_8_AUS_Q.csv")
}
for (d in c(CLEAN, OUTDIR)) dir.create(d, showWarnings = FALSE)
if (!file.exists(F_LEND)) stop("Lending export not found: ", F_LEND)

# ---- Settings ----------------------------------------------------------------
P_MAX <- 6      # longest lag built; Granger tests can compare orders 1 to 6
TRIM  <- 0.15   # share of quarters kept out of each end in the unknown-date test

# Variables that get lags 1..P_MAX (outcomes, credit measures, cash rate).
LAG_VARS <- c("dl_price", "dl_rent", "dl_inv", "dl_inv_sa",
              "d_inv_share_sa", "d_cash")

# Regime indicators: 1 from the first full quarter after each event.
REGIMES <- c(
  post_2015q1 = "2015 Q1",  # APRA investor-lending benchmark, letter of 9 December 2014
  post_2017q1 = "2017 Q1",  # break used in the earlier version (same as `post`)
  post_2017q2 = "2017 Q2",  # APRA interest-only cap, 31 March 2017
  post_2018q3 = "2018 Q3",  # investor benchmark removed, 1 July 2018
  post_2019q1 = "2019 Q1",  # interest-only benchmark removed, 1 January 2019
  post_2020q1 = "2020 Q1"   # pandemic marker used in the earlier robustness check
)

in_q <- function(q, from, to) q >= as.yearqtr(from) & q <= as.yearqtr(to)
first_last <- function(q, x) {
  ok <- is.finite(x)
  if (!any(ok)) return("none")
  paste(format(min(q[ok])), "to", format(max(q[ok])))
}

# ---- 1. The paper's panel (earlier vintage) -----------------------------------
# Reads the .rds that 01_build_panel.R writes, or else its CSV copy.
f_panel <- file.path(CLEAN, "panel_combined.rds")
if (file.exists(f_panel)) {
  panel <- readRDS(f_panel)
} else {
  f_panel <- file.path(CLEAN, "panel_combined.csv")
  if (!file.exists(f_panel))
    stop("Neither clean/panel_combined.rds nor clean/panel_combined.csv was found. ",
         "Run 01_build_panel.R first, or copy panel_combined.csv into clean/.")
  panel <- read_csv(f_panel, show_col_types = FALSE) |>
    mutate(quarter = as.yearqtr(quarter))
}
panel <- panel |> arrange(city, quarter)
stopifnot(setequal(unique(panel$city), CITIES),
          !anyDuplicated(panel[c("city", "quarter")]))

# ---- 2. Investor and owner-occupier lending, by state -------------------------
# Same export and slice as inv_commit in 01_build_panel.R: new loan commitments,
# total dwellings excluding refinancing, all lenders. Values run from 2006 Q1;
# investor counts start in 2019 Q3.
lend <- read_csv(F_LEND, show_col_types = FALSE) |>
  filter(`Loan Purpose` == "Total dwellings excluding refinancing",
         `Lender Type` == "Total lender type",
         `Housing Purpose` %in% c("Investor", "Owner occupier"),
         Region %in% names(STATE2CITY)) |>
  mutate(city    = unname(STATE2CITY[Region]),
         quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)),
         series  = paste(ifelse(Measure == "Value", "val", "num"),
                         ifelse(`Housing Purpose` == "Investor", "inv", "oo"),
                         ifelse(`Adjustment Type` == "Original", "o", "sa"),
                         sep = "_"),
         value   = as.numeric(OBS_VALUE)) |>
  select(city, quarter, series, value)
stopifnot(!anyDuplicated(lend[c("city", "quarter", "series")]))
lend <- lend |> pivot_wider(names_from = series, values_from = value)
need <- c("val_inv_o", "val_oo_o", "val_inv_sa", "val_oo_sa", "num_inv_sa", "num_oo_sa")
if (!all(need %in% names(lend)))
  stop("Lending export lacks: ", paste(setdiff(need, names(lend)), collapse = ", "),
       ". Re-export LEND_HOUSING with values and numbers, Original and SA.")
stopifnot(setequal(unique(lend$city), CITIES))
lend <- lend |> select(city, quarter, all_of(need))

# ---- 3. Variables, built within city ------------------------------------------
# Changes are quarter-on-quarter log changes (dl_) or simple changes (d_).
# Shares are fractions (0.35 = 35 percent), so d_inv_share = 0.01 is one point.
q0 <- min(panel$quarter)
tp <- panel |>
  left_join(lend, by = c("city", "quarter")) |>
  arrange(city, quarter) |>
  group_by(city) |>
  mutate(
    dl_price = log(prop_price) - lag(log(prop_price)),
    dl_rent  = log(rents)      - lag(log(rents)),
    # Credit, as in the paper: Original value of new investor commitments
    dl_inv   = log(inv_commit) - lag(log(inv_commit)),
    # Credit, seasonally adjusted value
    dl_inv_sa = log(val_inv_sa) - lag(log(val_inv_sa)),
    # Investor share of new lending by value. Both values scale with prices,
    # so the share does not rise mechanically when prices rise.
    inv_share      = val_inv_o  / (val_inv_o  + val_oo_o),
    inv_share_sa   = val_inv_sa / (val_inv_sa + val_oo_sa),
    d_inv_share    = inv_share    - lag(inv_share),
    d_inv_share_sa = inv_share_sa - lag(inv_share_sa),
    # Investor share by number of loans, and growth in investor loan counts:
    # a check from 2019 Q3 only
    inv_share_num_sa = num_inv_sa / (num_inv_sa + num_oo_sa),
    dl_inv_num_sa    = log(num_inv_sa) - lag(log(num_inv_sa)),
    # Controls, defined as in 02_estimate.R
    d_cash   = cash_rate - lag(cash_rate),
    tight_l1 = lag(tightness),
    dl_wpi   = log(wpi)        - lag(log(wpi)),
    dl_emp   = log(employment) - lag(log(employment)),
    gap_l1   = lag(log(price_rent))
  ) |>
  ungroup()

# Lags 1..P_MAX within city. Columns are named <variable>_l<j>.
for (v in LAG_VARS) for (j in seq_len(P_MAX)) {
  tp <- tp |> group_by(city) |> mutate("{v}_l{j}" := lag(.data[[v]], j)) |> ungroup()
}

# Calendar: integer time index, year, quarter of year (for seasonal dummies).
tp <- tp |>
  mutate(qid     = as.integer(round((as.numeric(quarter) - as.numeric(q0)) * 4)) + 1L,
         year    = as.integer(format(quarter, "%Y")),
         qtr     = as.integer(format(quarter, "%q")),
         city_id = match(city, CITIES))

# ---- 4. Regime indicators ----------------------------------------------------
for (nm in names(REGIMES)) tp[[nm]] <- as.integer(tp$quarter >= as.yearqtr(REGIMES[[nm]]))
tp <- tp |>
  mutate(apra_inv_on = as.integer(in_q(quarter, "2015 Q1", "2018 Q2")),  # investor benchmark in force
         apra_io_on  = as.integer(in_q(quarter, "2017 Q2", "2018 Q4")))  # interest-only cap in force
stopifnot(identical(tp$post_2017q1, as.integer(tp$post)))

# ---- 5. Measurement flags ----------------------------------------------------
# flag_reclass: lenders and borrowers reclassified loans between investor and
#   owner-occupier purposes. RBA D2 switching peaks in 2015 Q3 and Q4 and
#   settles at about $3bn a quarter from mid-2016. The investor share drops in
#   2015 Q3 and recovers by 2016 Q3, just after the 2015 Q1 break date, so the
#   flag covers 2015 Q3 to 2016 Q2. The checks print the D2 flows.
# flag_covid: border closures and pandemic support, for dropping the window.
# flag_cra: CPI rents are net of Commonwealth Rent Assistance; the CRA rises
#   of September 2023 and 20 September 2024 lowered measured rent growth in
#   these quarters (ABS CPI releases, December quarters 2023 and 2024).
# flag_splice: price growth for the four non-Sydney cities comes from the ABS
#   state mean dwelling price after 2021 Q4 (BASE_QTR), not from 6416.0.
tp <- tp |>
  mutate(flag_reclass = as.integer(in_q(quarter, "2015 Q3", "2016 Q2")),
         flag_covid   = as.integer(in_q(quarter, "2020 Q2", "2021 Q4")),
         flag_cra     = as.integer(format(quarter) %in% c("2023 Q4", "2024 Q3", "2024 Q4")),
         flag_splice  = as.integer(city != "Sydney" & quarter > BASE_QTR))

# ---- 6. Sample flags -----------------------------------------------------------
# s_base: rows every equation in the paper can use (370 in the earlier vintage).
# s_granger: s_base rows that also hold every lag up to P_MAX, so lag orders
#   1..P_MAX are compared on one sample.
base_vars <- c("dl_price", "dl_rent", "dl_inv", "d_cash", "tight_l1",
               "dl_wpi", "dl_emp", "gap_l1")
gr_vars   <- c(LAG_VARS, paste0(rep(LAG_VARS, each = P_MAX), "_l", seq_len(P_MAX)))
tp <- tp |>
  mutate(s_base    = as.integer(if_all(all_of(base_vars), ~ is.finite(.x))),
         s_granger = as.integer(s_base == 1 & if_all(all_of(gr_vars), ~ is.finite(.x))))

# Candidate dates for the unknown-date break test: each regime keeps at least
# ceiling(TRIM * T) quarters of the s_base sample, with the break in the first
# quarter of the new regime.
qs_base <- sort(unique(tp$quarter[tp$s_base == 1]))
T_base  <- length(qs_base)
m_trim  <- ceiling(TRIM * T_base)
cand_q  <- qs_base[(m_trim + 1):(T_base - m_trim + 1)]
tp <- tp |> mutate(break_candidate = as.integer(quarter %in% cand_q))

# ---- 7. Stacked file for the price-rent comparison --------------------------
# Two rows per city-quarter: eq = "price" (y = dl_price) and eq = "rent"
# (y = dl_rent), with y's own lags in y_l1..y_l6 and every regressor repeated.
# unit = city x equation. In plm, index = c("unit", "qid"), so unit fixed
# effects give each equation its own city intercepts, and Driscoll-Kraay
# errors allow the price and rent errors of a quarter to be correlated.
stack_one <- function(eqn, yvar) {
  out <- tp |> mutate(eq = eqn, is_rent = as.integer(eqn == "rent"), y = .data[[yvar]])
  for (j in seq_len(P_MAX)) out[[paste0("y_l", j)]] <- out[[paste0(yvar, "_l", j)]]
  out
}
stk <- bind_rows(stack_one("price", "dl_price"), stack_one("rent", "dl_rent")) |>
  mutate(unit = paste(city, eq, sep = "_")) |>
  relocate(unit, eq, is_rent, city, quarter, qid, y, starts_with("y_l")) |>
  arrange(eq, city, quarter)

# ---- 8. Write -------------------------------------------------------------------
to_csv <- function(df) df |> mutate(quarter = format(quarter))
saveRDS(tp,  file.path(CLEAN, "panel_tests.rds"))
saveRDS(stk, file.path(CLEAN, "panel_tests_stacked.rds"))
write_csv(to_csv(tp),  file.path(CLEAN, "panel_tests.csv"))
write_csv(to_csv(stk), file.path(CLEAN, "panel_tests_stacked.csv"))

# ---- 9. Checks ------------------------------------------------------------------
# Printed to the console and saved to output/panel_tests_checks.txt.
seas_r2 <- function(v) {
  d <- tp |> filter(s_base == 1, is.finite(.data[[v]]))
  summary(lm(d[[v]] ~ factor(d$qtr) + factor(d$city)))$r.squared
}
# The sink starts inside tryCatch, so source(echo = TRUE) does not copy the
# code into the summary.
con <- file(file.path(OUTDIR, "panel_tests_checks.txt"), open = "wt")
tryCatch({
  sink(con, split = TRUE)
  cat("panel_tests checks,", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("Inputs:", f_panel, "and", F_LEND, "\n\n")

  cat("1. Panel shape\n")
  per_city <- tp |> group_by(city) |>
    summarise(rows = n(), first = format(min(quarter)), last = format(max(quarter)),
              gaps = sum(diff(qid) != 1), .groups = "drop")
  print(as.data.frame(per_city), row.names = FALSE)
  stopifnot(all(per_city$gaps == 0), length(unique(per_city$rows)) == 1)

  cat("\n2. Investor value matches inv_commit in panel_combined\n")
  dif <- max(abs(tp$val_inv_o - tp$inv_commit), na.rm = TRUE)
  cat(sprintf("   largest absolute difference: %.6f ($m)\n", dif))
  stopifnot(dif < 1e-6)

  cat("\n3. Coverage of the new series (first to last quarter with a value)\n")
  for (v in c("dl_inv", "dl_inv_sa", "inv_share", "inv_share_sa", "d_inv_share_sa",
              "inv_share_num_sa", "dl_inv_num_sa"))
    cat(sprintf("   %-17s %s\n", v, first_last(tp$quarter, tp[[v]])))

  cat("\n4. Investor share of new lending by value\n")
  cat(sprintf("   Original: %.3f to %.3f;  SA: %.3f to %.3f\n",
              min(tp$inv_share, na.rm = TRUE), max(tp$inv_share, na.rm = TRUE),
              min(tp$inv_share_sa, na.rm = TRUE), max(tp$inv_share_sa, na.rm = TRUE)))
  stopifnot(all(tp$inv_share > 0 & tp$inv_share < 1, na.rm = TRUE))
  by_period <- tp |>
    mutate(period = cut(as.numeric(quarter), c(-Inf, 2015, 2017.25, 2020, 2022.25, Inf),
                        right = FALSE,
                        labels = c("to 2014Q4", "2015Q1-2017Q1", "2017Q2-2019Q4",
                                   "2020Q1-2022Q1", "2022Q2 on"))) |>
    group_by(city, period) |>
    summarise(share = round(mean(inv_share_sa, na.rm = TRUE), 3), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = share)
  print(as.data.frame(by_period), row.names = FALSE)
  r <- tp |> filter(is.finite(inv_share_num_sa))
  cat(sprintf("   From 2019 Q3, corr(share by value, share by number) = %.3f\n",
              cor(r$inv_share_sa, r$inv_share_num_sa)))
  r <- tp |> filter(is.finite(dl_inv_num_sa))
  cat(sprintf("   From 2019 Q4, corr(growth in value, growth in number) = %.3f (SA)\n",
              cor(r$dl_inv_sa, r$dl_inv_num_sa)))

  cat("\n5. Seasonality: R-squared of quarter-of-year and city dummies (s_base rows)\n")
  for (v in c("dl_inv", "dl_inv_sa", "d_inv_share", "d_inv_share_sa", "dl_price", "dl_rent"))
    cat(sprintf("   %-15s R2 = %.3f   SD = %.4f\n", v, seas_r2(v),
                sd(tp[[v]][tp$s_base == 1], na.rm = TRUE)))

  cat("\n6. Samples\n")
  for (s in c("s_base", "s_granger")) {
    qq <- tp$quarter[tp[[s]] == 1]
    cat(sprintf("   %-9s %3d rows, %s to %s\n", s, sum(tp[[s]]),
                format(min(qq)), format(max(qq))))
  }
  cat(sprintf("   Unknown-date test: T = %d quarters, %d kept out of each end;\n",
              T_base, m_trim))
  cat(sprintf("   candidate breaks %s to %s (%d quarters)\n",
              format(min(cand_q)), format(max(cand_q)), length(cand_q)))
  for (b in c("2015 Q1", "2017 Q1", "2017 Q2", "2018 Q3", "2022 Q1"))
    cat(sprintf("   %s is %sa candidate\n", b,
                if (as.yearqtr(b) %in% cand_q) "" else "NOT "))

  cat("\n7. Flags (quarters marked; rows in s_base)\n")
  for (f in c("flag_reclass", "flag_covid", "flag_cra", "flag_splice",
              "apra_inv_on", "apra_io_on")) {
    qq <- sort(unique(tp$quarter[tp[[f]] == 1]))
    span <- if (all(diff(as.numeric(qq)) == 0.25)) {
      paste(format(min(qq)), "to", format(max(qq)))
    } else paste(format(qq), collapse = ", ")
    cat(sprintf("   %-13s %s: %d quarters, %d s_base rows\n", f, span, length(qq),
                sum(tp[[f]] == 1 & tp$s_base == 1)))
  }
  cat("   RBA D2 net loan-purpose switching, $bn per quarter (basis for flag_reclass):\n")
  sw <- tp |> filter(city == CITIES[1], in_q(quarter, "2015 Q3", "2017 Q2")) |>
    transmute(quarter = format(quarter), net_switch_nat = round(net_switch_nat, 1))
  cat("   ", paste(sw$quarter, sw$net_switch_nat, sep = ": ", collapse = "; "), "\n")

  cat("\n8. Stacked file\n")
  cat(sprintf("   %d rows, %d units; rows by equation: %s\n", nrow(stk),
              length(unique(stk$unit)),
              paste(names(table(stk$eq)), table(stk$eq), sep = " = ", collapse = ", ")))
  stopifnot(!anyDuplicated(stk[c("unit", "qid")]))

  cat("\nWrote clean/panel_tests.{rds,csv} (", nrow(tp), " rows, ", ncol(tp),
      " columns) and clean/panel_tests_stacked.{rds,csv} (", nrow(stk), " rows).\n",
      sep = "")
}, finally = {
  sink()
  close(con)
})

end_run_log()
