# =============================================================================
# 00_setup.R  --  shared constants, paths, namespace protection, and helpers
#
# Sourced at the top of every stage script (and by run_all.R). Centralises the
# things that were previously copy-pasted across nine files: the dplyr verb
# reassignments, the paths, the city/date constants, the ABS-Excel reader and
# growth-chain helpers, and the Driscoll-Kraay SE used throughout.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl); library(zoo)
  library(purrr)
})

# ---- Namespace protection (avoid MASS/stats masking dplyr verbs) ------------
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag
lead   <- dplyr::lead

# ---- Paths ------------------------------------------------------------------
RAW    <- "raw"
CLEAN  <- "clean"
OUTDIR <- "output"
for (d in c(CLEAN, OUTDIR)) dir.create(d, showWarnings = FALSE)

# ---- Constants --------------------------------------------------------------
CITIES     <- c("Sydney","Melbourne","Brisbane","Adelaide","Perth")
BASE_QTR   <- as.yearqtr("2021 Q4")   # price splice / overlap quarter
TARGET_END <- as.yearqtr("2025 Q4")   # sample end after extension
REBASE_WIN <- as.yearqtr(c("2011 Q3","2011 Q4","2012 Q1","2012 Q2"))

# State label -> city, and 6416.0 series-ID -> city, used in several readers.
STATE2CITY <- c(`New South Wales`="Sydney", Victoria="Melbourne",
                Queensland="Brisbane", `South Australia`="Adelaide",
                `Western Australia`="Perth")
IDMAP_6416 <- c(A83728383L="Sydney", A83728392R="Melbourne", A83728401F="Brisbane",
                A83728410J="Adelaide", A83728419C="Perth")

CITY_COLS <- c(Sydney="#c0392b", Melbourne="#2c3e50", Brisbane="#16a085",
               Adelaide="#8e44ad", Perth="#e67e22")

# ---- Modern-API source files (centralised for the quarterly refresh) --------
# The three API-dataflow CSVs added after the original nine-script pipeline.
# Kept here so a data refresh updates filenames in one place. All are read in
# 01_build_panel.R; the long ABS DSD filenames are the only volatile part.
F_LEND <- file.path(RAW, "ABS_LEND_HOUSING_1_1_______10_20_1_2_3_4_5_6_7_8_AUS_Q.csv")   # investor lending (5601.0 successor)
F_OMAD <- file.path(RAW, "ABS_OMAD_VISA___11_12_15_22_23_24_25_1009_1010_1020_1030_1040_1041_2203_2208_01_02_03_1_2_3_4_5_AUS_Q.csv")  # NOM by visa group
F_D2   <- file.path(RAW, "d02hist.xlsx")                                                  # RBA D2 national housing credit

# Migration visa-share guards (used when forming nom_temp_share / nom_student_share
# from 4q rolling sums). The raw quarterly ratio is unusable: total net NOM crosses
# zero during the 2020-21 border closures, so a quarterly ratio explodes. The 4q
# rolling sum keeps the denominator positive; NOM_FLOOR drops near-zero-denominator
# windows and SHARE_BAND nulls any residual out-of-range value.
NOM_FLOOR  <- 5000             # persons per trailing 4q; below this the share is NA
SHARE_BAND <- c(-0.05, 1.05)   # plausible share range; outside this set NA

# ---- ABS reader helpers -----------------------------------------------------
# All ABS "Data1"-style workbooks share the same shape: a header block, the
# series IDs around row 10, observations from row 11, column 1 the Excel-serial
# date. These three helpers replace ~8 copy-pasted read/clean/chain blocks.

# Read one numeric column of an ABS xlsx by position; return tibble(date, value)
# with the Excel serial date parsed and NA rows dropped.
read_abs_xlsx_col <- function(path, sheet, col, first_data_row = 11) {
  ws <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  tibble::tibble(date  = ws[[1]],
                 value = suppressWarnings(as.numeric(ws[[col]]))) |>
    dplyr::slice(first_data_row:dplyr::n()) |>
    dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |>
    dplyr::filter(!is.na(date), !is.na(value))
}

# Collapse a monthly (date,value) frame to a quarterly (quarter,value) frame.
to_quarterly <- function(df, fun = mean) {
  df |>
    dplyr::mutate(quarter = zoo::as.yearqtr(date)) |>
    dplyr::group_by(quarter) |>
    dplyr::summarise(value = fun(value), .groups = "drop") |>
    dplyr::arrange(quarter)
}

# Chain a source series' q/q growth onto a city's panel level at `anchor`,
# returning the extended tail (quarters in (anchor, target_end]).
# src_q: tibble(quarter, value); panel: the base panel; varname: column to anchor on.
chain_tail <- function(src_q, panel, varname, city, anchor, target_end) {
  lvl <- panel[[varname]][panel$city == city & panel$quarter == anchor]
  if (length(lvl) != 1 || is.na(lvl))
    stop(sprintf("chain_tail: no anchor level for %s/%s at %s",
                 city, varname, format(anchor)))
  src_q |>
    dplyr::arrange(quarter) |>
    dplyr::mutate(g = value / dplyr::lag(value) - 1) |>
    dplyr::filter(quarter > anchor, quarter <= target_end) |>
    dplyr::mutate(value = lvl * cumprod(1 + g), city = city) |>
    dplyr::select(city, quarter, value)
}

# Build an extended tail for a variable read from a per-city column map of an
# ABS xlsx (months -> quarterly mean -> growth-chained onto the panel level).
# col_map: named integer vector city -> column index. Returns tibble(city,quarter,value).
build_xlsx_tail <- function(path, sheet, col_map, panel, varname, anchor, target_end,
                            agg = mean) {
  purrr::imap_dfr(col_map, function(col, city)
    read_abs_xlsx_col(path, sheet, col) |>
      to_quarterly(fun = agg) |>
      chain_tail(panel, varname, city, anchor, target_end))
}

# Guard: a freshly built tail should cover every city for every new quarter and
# be finite. Call right after building, so an ABS layout change fails loudly.
assert_tail_ok <- function(tail_df, n_cities, n_new_q, what) {
  n_exp <- n_cities * n_new_q
  if (nrow(tail_df) != n_exp)
    warning(sprintf("%s tail has %d rows, expected %d (city x new-quarter)",
                    what, nrow(tail_df), n_exp))
  if (any(!is.finite(tail_df$value)))
    warning(sprintf("%s tail contains non-finite values", what))
  invisible(tail_df)
}

# ---- Estimation helper ------------------------------------------------------
# Driscoll-Kraay SE for a plm fit, matching the standard errors reported in the
# manuscript tables. Used by the estimation and LP stages alike so the code and
# the paper agree on the vcov.
dk_se <- function(m, maxlag = 4) sqrt(diag(plm::vcovSCC(m, type = "HC0", maxlag = maxlag)))

# Driscoll-Kraay coefficient table (coef, SE, t, p) for a plm fit.
dk_coeftest <- function(m, maxlag = 4)
  lmtest::coeftest(m, vcov = function(x) plm::vcovSCC(x, type = "HC0", maxlag = maxlag))