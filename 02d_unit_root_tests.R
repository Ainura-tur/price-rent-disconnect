# =============================================================================
# 02d_unit_root_tests.R  --  Unit-root tests for the estimation variables
#
# Rebuilds the appendix unit-root table (tab:unit_root) on the current panel.
# The earlier table came from an older version of the data and no script in
# the pipeline produced it. This one tests the series the estimating equations
# use, each in levels and in first differences:
#   log rent, log dwelling price, log wage price index, log employment, log
#   investor credit and the investor share of new lending (both seasonally
#   adjusted), the cash rate, tightness, the single-quarter tightness ratio
#   (the unsmoothed measure of the Appendix C checks) and the log price-rent gap.
#
# Tests, all from plm::purtest:
#   city by city  ADF regression with a constant; the level tests of the five
#                 log series add a linear trend. p-values use MacKinnon (1994),
#                 pinned so that results do not depend on whether the urca
#                 package is installed (with urca, plm would switch to 1996).
#   panel         Im-Pesaran-Shin W statistic: standard normal under the null
#                 that every city's series has a unit root. It assumes the
#                 cities are independent, which national shocks violate, so the
#                 count of cities whose own test rejects is reported beside it.
#   national      the cash rate is one series for all cities, so its row
#                 reports the ADF t statistic of that single series.
# Lag orders run from 0 to PMAX and are chosen city by city, once by AIC and
# once by SIC; the table reports both, because the tightness result depends on
# the choice.
#
# Reads clean/panel_tests (.rds or .csv), written by 01a_build_test_panel.R.
# Run from the folder that holds clean/ (the repository root):
#   Rscript 02d_unit_root_tests.R
# It does not need 00_setup.R and runs in a few seconds.
#
# Writes to output/:
#   unit_root_tests.csv    one row per series, transformation and lag rule
#   unit_root_by_city.csv  the city ADF regressions behind each row
#   table_unit_root.tex    the appendix table (tab:unit_root)
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("02d_unit_root_tests")

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

PMAX   <- 4                          # largest lag order: one year of quarters
RULES  <- c("AIC", "SIC")            # lag-selection criteria, both reported
ALPHA  <- 0.05                       # level for counting city rejections
CITIES <- c("Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth")

# Series in table order. trend: the level test adds a linear trend (the log
# series grow); ratios, shares and rates get a constant only. national: one
# series shared by every city.
SERIES <- tribble(
  ~var,           ~label,                        ~trend, ~national,
  "l_rent",       "Log rent",                    TRUE,   FALSE,
  "l_price",      "Log dwelling price",          TRUE,   FALSE,
  "l_wpi",        "Log wages (WPI)",             TRUE,   FALSE,
  "l_emp",        "Log employment",              TRUE,   FALSE,
  "l_inv",        "Log investor credit",         TRUE,   FALSE,
  "inv_share_sa", "Investor share",              FALSE,  FALSE,
  "cash_rate",    "Cash rate$^{a}$",             FALSE,  TRUE,
  "tightness",    "Tightness",                   FALSE,  FALSE,
  "tight_1q",     "Tightness, one quarter$^{b}$", FALSE, FALSE,
  "gap",          "Price--rent gap",             FALSE,  FALSE
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
tp <- read_panel("panel_tests")

need <- c("city", "quarter", "rents", "prop_price", "wpi", "employment", "val_inv_sa",
          "inv_share_sa", "cash_rate", "tightness", "net_migration", "dwell_total",
          "price_rent")
missing_cols <- setdiff(need, names(tp))
if (length(missing_cols))
  stop("panel_tests lacks: ", paste(missing_cols, collapse = ", "),
       ". Rerun 01a_build_test_panel.R.")

# The same definitions as the estimation scripts: gap = log(price/rent) as in
# 02c, tight_1q = the un-smoothed ratio of 02_estimate.R section 7.2.
tp <- tp |>
  mutate(quarter  = as.yearqtr(quarter),
         l_rent   = log(rents),
         l_price  = log(prop_price),
         l_wpi    = log(wpi),
         l_emp    = log(employment),
         l_inv    = log(val_inv_sa),
         gap      = log(price_rent),
         tight_1q = net_migration / dwell_total) |>
  arrange(city, quarter)

# One series as a quarter-by-city table over the quarters in which every city
# has a value, so each test runs on a balanced panel over the series' own span.
wide <- function(v) {
  w <- tp |>
    select(city, quarter, value = all_of(v)) |>
    pivot_wider(names_from = city, values_from = value) |>
    arrange(quarter)
  w[stats::complete.cases(w), c("quarter", CITIES)]
}

# ---- Tests ---------------------------------------------------------------------
# ADF regressions city by city and the panel statistic for one series,
# transformation and lag rule. x has one column per city, rows in time order.
# A national series has one column; purtest then warns that the panel has a
# single individual, which is expected, and that one warning is muffled.
run_test <- function(x, exo, rule) {
  single <- ncol(x) == 1L
  r <- withCallingHandlers(
    purtest(x, test = if (single) "madwu" else "ips", exo = exo, lags = rule,
            pmax = PMAX, p.approx = "MacKinnon1994"),
    warning = function(w)
      if (single && grepl("more than one individual", conditionMessage(w)))
        invokeRestart("muffleWarning"))
  city <- tibble(city   = names(x),
                 lags   = vapply(r$idres, function(e) as.integer(e[["lags"]]), 0L),
                 adf_t  = vapply(r$idres, function(e) e[["trho"]],   0),
                 adf_p  = vapply(r$idres, function(e) e[["p.trho"]], 0),
                 nobs   = nrow(x))
  if (single) {
    panel <- tibble(test = "ADF t", statistic = city$adf_t, p_value = city$adf_p,
                    cities_reject = NA_integer_)
  } else {
    panel <- tibble(test = "IPS W", statistic = unname(r$statistic$statistic),
                    p_value = unname(r$statistic$p.value),
                    cities_reject = sum(city$adf_p < ALPHA))
  }
  list(panel = panel, city = city)
}

# First-order autocorrelation, pooled within cities (a fixed-effects AR(1)
# slope); for a single series, the AR(1) slope with an intercept. A guide to
# persistence beside the tests, for levels and differences alike.
ar1_within <- function(x) {
  y <- as.matrix(x); n <- nrow(y)
  cur  <- sweep(y[-1, , drop = FALSE], 2, colMeans(y[-1, , drop = FALSE]))
  prev <- sweep(y[-n, , drop = FALSE], 2, colMeans(y[-n, , drop = FALSE]))
  sum(cur * prev) / sum(prev^2)
}

panel_rows <- list(); city_rows <- list()
for (i in seq_len(nrow(SERIES))) {
  s <- SERIES[i, ]
  w <- wide(s$var)
  if (s$national) {
    vals <- as.matrix(w[, CITIES])
    if (any(apply(vals, 1, function(r) length(unique(r)) > 1)))
      stop(s$var, " is flagged national but differs across cities.")
    x_level <- data.frame(Australia = vals[, 1])
  } else {
    x_level <- as.data.frame(w[, CITIES])
  }
  for (tr in c("level", "difference")) {
    x   <- if (tr == "level") x_level else as.data.frame(lapply(x_level, diff))
    exo <- if (tr == "level" && s$trend) "trend" else "intercept"
    q   <- if (tr == "level") w$quarter else w$quarter[-1]
    for (rule in RULES) {
      res <- run_test(x, exo, rule)
      panel_rows[[length(panel_rows) + 1]] <- tibble(
        series = s$var, label = s$label, transform = tr, lag_rule = rule,
        deterministic = if (exo == "trend") "constant and trend" else "constant",
        first_quarter = format(min(q)), last_quarter = format(max(q)),
        quarters = length(q), cities = ncol(x)) |>
        bind_cols(res$panel) |>
        mutate(mean_city_t = mean(res$city$adf_t),
               city_lags   = paste(res$city$lags, collapse = "-"),
               ar1         = ar1_within(x))
      city_rows[[length(city_rows) + 1]] <- res$city |>
        mutate(series = s$var, transform = tr, lag_rule = rule, .before = 1)
    }
  }
}
ur      <- bind_rows(panel_rows)
by_city <- bind_rows(city_rows)

write_csv(ur,      file.path(OUTDIR, "unit_root_tests.csv"))
write_csv(by_city, file.path(OUTDIR, "unit_root_by_city.csv"))

# ---- Console summary -------------------------------------------------------------
cat("\nUnit-root tests (H0: unit root). IPS W, or ADF t for the national cash rate;\n")
cat(sprintf("rej = cities whose own ADF rejects at %.0f percent; lags by city in the order %s.\n",
            100 * ALPHA, paste(CITIES, collapse = ", ")))
for (rule in RULES) {
  cat(sprintf("\nLags chosen by %s (0 to %d):\n", rule, PMAX))
  cat(sprintf("  %-30s %-10s %8s %7s %4s  %-10s\n", "series", "transform", "stat", "p", "rej", "lags"))
  u <- filter(ur, lag_rule == rule)
  for (k in seq_len(nrow(u)))
    cat(sprintf("  %-30s %-10s %8.2f %7.3f %4s  %-10s\n",
                gsub("\\$\\^\\{.\\}\\$", "", u$label[k]), u$transform[k], u$statistic[k],
                u$p_value[k], ifelse(is.na(u$cities_reject[k]), "-", u$cities_reject[k]),
                u$city_lags[k]))
}
cat("\nFirst-order autocorrelation, pooled within cities:\n")
cat(sprintf("  %-30s %8s %11s\n", "series", "level", "difference"))
a <- ur |> filter(lag_rule == RULES[1]) |> select(label, transform, ar1) |>
  pivot_wider(names_from = transform, values_from = ar1)
for (k in seq_len(nrow(a)))
  cat(sprintf("  %-30s %8.3f %11.3f\n", gsub("\\$\\^\\{.\\}\\$", "", a$label[k]),
              a$level[k], a$difference[k]))

# ---- LaTeX table ---------------------------------------------------------------
stars <- function(p) ifelse(p < 0.01, "\\sym{***}", ifelse(p < 0.05, "\\sym{**}",
                     ifelse(p < 0.10, "\\sym{*}", "")))
cell <- function(var, tr, rule) {
  r <- filter(ur, series == var, transform == tr, lag_rule == rule)
  c(sprintf("%.2f%s", r$statistic, stars(r$p_value)),
    if (is.na(r$cities_reject)) "--" else as.character(r$cities_reject))
}
body <- vapply(seq_len(nrow(SERIES)), function(i) {
  s <- SERIES[i, ]
  cells <- c(cell(s$var, "level", "AIC"), cell(s$var, "difference", "AIC"),
             cell(s$var, "level", "SIC"), cell(s$var, "difference", "SIC"))
  paste0(s$label, " & ", if (s$trend) "Yes" else "No", " & ",
         paste(cells, collapse = " & "), " \\\\")
}, "")

# Coverage for the notes: the common span, and the two tightness measures,
# which end a quarter early because net migration does. Warn if another
# series departs from the common span, since the note would then be wrong.
spans    <- ur |> filter(transform == "level", lag_rule == RULES[1])
span_of  <- function(v) unlist(spans[spans$series == v, c("first_quarter", "last_quarter")])
colon_q  <- function(q) sub(" ", ":", q)   # "2006 Q3" -> "2006:Q3"
common   <- span_of("l_rent")
odd      <- spans |> filter(!series %in% c("tightness", "tight_1q"),
                            first_quarter != common[1] | last_quarter != common[2])
if (nrow(odd))
  warning("The table note gives the wrong span for: ", paste(odd$series, collapse = ", "))
span_txt <- sprintf(paste("The series run from %s to %s, except tightness, from %s to %s,",
                          "and the one-quarter ratio, from %s to %s."),
                    colon_q(common[1]), colon_q(common[2]),
                    colon_q(span_of("tightness")[1]), colon_q(span_of("tightness")[2]),
                    colon_q(span_of("tight_1q")[1]),  colon_q(span_of("tight_1q")[2]))

# Reserve room for superscript stars, which take less space than text stars.
Scol <- "S[table-format=-2.2, table-space-text-post = {\\sym{***}}]"
tex <- c(
  "% Generated by 02d_unit_root_tests.R (output/table_unit_root.tex).",
  "\\begin{table}[!htbp]",
  "\\small",
  "\\centering",
  "\\caption{Unit-root tests for the estimation variables}",
  "\\label{tab:unit_root}",
  "\\begin{threeparttable}",
  "\\setlength{\\tabcolsep}{2pt}",
  paste0("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lc",
         paste(rep(paste0(Scol, "c"), 4), collapse = ""), "@{}}"),
  "\\toprule",
  " & & \\multicolumn{4}{c}{Lags chosen by AIC} & \\multicolumn{4}{c}{Lags chosen by SIC} \\\\",
  "\\cmidrule(lr){3-6}\\cmidrule(lr){7-10}",
  paste0(" & & \\multicolumn{2}{c}{Level} & \\multicolumn{2}{c}{Difference}",
         " & \\multicolumn{2}{c}{Level} & \\multicolumn{2}{c}{Difference} \\\\"),
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}\\cmidrule(lr){9-10}",
  paste0("Variable & Trend", strrep(" & {$W$} & Cities", 4), " \\\\"),
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular*}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} $W$ is the panel statistic of \\citet{im2003testing}. It is ",
         "standard normal under the null that every city's series has a unit root, and ",
         "large negative values reject. Cities is the number of the five cities whose own ",
         "ADF test rejects a unit root at the 5 percent level. Every test includes a ",
         "constant, and the level tests of the five log series also include a linear ",
         "trend, as the Trend column shows. Lag orders run from 0 to ", PMAX, " and are ",
         "chosen city by city, by the Akaike (AIC) or the Schwarz (SIC) criterion. ",
         span_txt, " Investor credit and the ",
         "investor share are seasonally adjusted. The $W$ test assumes that the cities ",
         "are independent. National shocks correlate the five series, which makes the ",
         "test reject too often, so the city counts are the safer guide. ",
         "$^{a}$One national series: the row reports its ADF $t$ statistic. ",
         "$^{b}$Net migration over dwelling completions within the quarter, the unsmoothed ",
         "ratio used in the tightness checks of this appendix. ",
         "\\sym{*}$p<0.10$, \\sym{**}$p<0.05$, \\sym{***}$p<0.01$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}")
writeLines(tex, file.path(OUTDIR, "table_unit_root.tex"))

cat("\nWrote output/unit_root_tests.csv, output/unit_root_by_city.csv and",
    "output/table_unit_root.tex.\n")
end_run_log()
