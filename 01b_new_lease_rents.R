# 01b_new_lease_rents.R
#
# Builds quarterly indices of rents on new leases (new bonds) for Sydney,
# Melbourne, Brisbane and Adelaide from the state bond reports in raw_rev/.
# Perth has no published bond medians.
#
# Each index chains quarterly changes in median rents within fixed cells
# (dwelling type by bedrooms; LGA too for Brisbane; bedrooms only for
# Sydney), weighted by each cell's share of new bonds over the whole sample.
# Fixed weights keep shifts in the mix of dwellings let from moving the index.
#
# Inputs, from the repository folder (in file names, spaces, hyphens and
# underscores are interchangeable, and case does not matter):
#   raw_rev/vic/Quarterly median rents by Local Government Area*.xlsx
#       Homes Victoria via DataVic; metro Melbourne, Jun 1999 on
#   raw_rev/vic/Tables from Rental Report*.xlsx (optional)
#       Homes Victoria's Metropolitan Rent Index, used only as a check
#   raw_rev/qld/rta-bond-statistics*.xlsx
#       Queensland RTA; five Greater Brisbane LGAs, Mar 2012 on
#   raw_rev/sa/*YYYY-MM*.xls(x)
#       SA Private Rental Report, one file per quarter, Jun 2008 on
#   raw_rev/nsw/dcj_xlsx/*.xls(x)
#       DCJ Rent and Sales Report, from DCJ's "Previous rent and sales
#       reports" page (2017 to 2024) and its current Rent and Sales Report
#       page (2025 on), names as downloaded:
#       - the rent tables, one workbook per quarter from June 2017 (issue
#         120, an .xls) on, e.g. issue-121-rent-tables-september-2017.xlsx
#       - the time series "Rents, Trend March 1990 - June 2017, Metropolitan
#         LGAs", issue-120-rents-june-2017-metro-lgas.xls
#       The script tells them apart by their sheets.
#   raw_rev/nsw/dcj_pdf/issue-NNN-rent-*.pdf (optional)
#       DCJ report PDFs, Mar 2008 to Mar 2017 (fetch_rent_reports.R); a check
#       on the time series, or Sydney's source before June 2017 if
#       SYD_BEFORE_2017 is "pdf" or the time series is missing
#   raw/panel_main.csv (optional)
#       CPI rents, used only for the lead check
#
# Outputs:
#   clean_rev/new_lease_cells.csv  one row per city, quarter and cell
#   clean_rev/new_lease_rents.csv  one row per city and quarter: log change,
#                                  seasonally adjusted change, index
#                                  (2011-12 = 100), weight coverage
#   clean_rev/new_lease_checks.txt coverage, weights, seasonality and checks
#
# Sydney: four cells, Greater Sydney new bonds for all dwellings with one,
# two, three, and four or more bedrooms.
#   To June 2017: DCJ's time series of median rents, March 1990 to June
#     2017. Its medians equal those printed in the reports for June 2012
#     (issue 100: 410, 450, 460, 580), December 2014 (issue 110: 450, 500,
#     495, 600) and June 2017 (issue 120: 500, 540, 530, 650). Quarters are
#     read by position, because sheet 3BR_A labels its September 2015
#     column "Mar-15". With
#     SYD_BEFORE_2017 = "pdf", March 2008 to March 2017 come from the PDFs
#     instead, chained by DCJ's reported changes as in version 5, and June
#     2017 from issue 120's rent tables.
#   From September 2017: DCJ's rent tables, sheet LGA, rows Greater Sydney,
#     ring Total, LGA Total, dwelling type Total. The chain uses DCJ's
#     reported change from the previous quarter. Those changes match the
#     previous table's medians to within rounding, except where DCJ revised
#     that quarter. June 2020 (issue 132) reports no quarterly change, so it
#     is bridged: September 2020's median and reported change give June
#     2020's revised median (DCJ moved its 3-bedroom median from 530 to 540
#     and its 4-bedroom median from 640 to 635).
#   Each table's quarter comes from its file name, or from its title if the
#     name has none. Issue 129 (September 2019) is titled June Quarter 2019
#     in error; its reported changes start from June 2019's medians.
#   Weights: new bonds by bedrooms in the rent tables, September 2017 on.
#     The time series and the PDFs have no bond counts by bedrooms.
#
# Run from the repository folder: Rscript 01b_new_lease_rents.R

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

SCRIPT_VERSION <- "version 6, 5 November 2026"   # written at the top of the checks file

RAW_REV   <- "raw_rev"
CLEAN_REV <- "clean_rev"
dir.create(CLEAN_REV, showWarnings = FALSE)

MIN_COVER <- 0.5   # a quarter's change needs cells holding half the weight
MONTHS    <- c("Mar", "Jun", "Sep", "Dec")

# Sydney before June 2017: "series" (DCJ's 1990-2017 time series) or "pdf"
# (the report PDFs, March 2008 to March 2017, as in version 5).
SYD_BEFORE_2017 <- "series"

checks <- character()
note <- function(...) {
  msg <- paste0(...)
  checks <<- c(checks, msg)
  message(msg)
}

num <- function(v) suppressWarnings(as.numeric(v))
qn_of <- function(year, q) as.integer(year) * 4L + as.integer(q) - 1L   # quarter number
q_label <- function(qn) sprintf("%dQ%d", qn %/% 4L, qn %% 4L + 1L)

read_text_sheet <- function(path, sheet) {
  x <- as.matrix(read_excel(path, sheet = sheet, col_names = FALSE,
                            col_types = "text", .name_repair = "minimal"))
  x[is.na(x)] <- ""
  trimws(x)
}

# Newest file in dir whose name matches pattern once it is lower-cased and
# each run of spaces, hyphens, dashes or underscores is made one underscore.
# Downloads keep the publisher's spaces; uploads often turn them into
# underscores. Both forms match.
name_key <- function(f) tolower(gsub("[^A-Za-z0-9.]+", "_", basename(f)))
latest_file <- function(dir, pattern) {
  f <- list.files(dir, full.names = TRUE)
  f <- f[grepl(pattern, name_key(f))]
  if (!length(f)) return(character())
  f[which.max(file.mtime(f))]
}

# readxl cannot open some non-ASCII file names (an en dash, say) outside a
# UTF-8 locale. read_with() reads such a workbook from an ASCII-named
# temporary copy and keeps the original name in the source column.
read_with <- function(reader, path) {
  if (!grepl("[^ -~]", basename(path), useBytes = TRUE)) return(reader(path))
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  file.copy(path, tmp)
  on.exit(unlink(tmp))
  out <- reader(tmp)
  if (is.data.frame(out) && "source" %in% names(out)) out$source <- basename(path)
  out
}

# For a note when no file matches: what the folder holds.
folder_note <- function(dir) {
  if (!dir.exists(dir)) return(paste0(dir, " does not exist"))
  f <- list.files(dir)
  if (!length(f)) return(paste0(dir, " is empty"))
  paste0(dir, " holds ", paste(head(f, 8), collapse = ", "), if (length(f) > 8) ", ...")
}

# ---- Melbourne: Homes Victoria, quarterly medians by LGA ----------------------

read_vic <- function(path) {
  types <- c("1 bedroom flat" = "flat_1", "2 bedroom flat" = "flat_2",
             "3 bedroom flat" = "flat_3", "2 bedroom house" = "house_2",
             "3 bedroom house" = "house_3", "4 bedroom house" = "house_4")
  sheets <- excel_sheets(path)
  out <- list()
  for (s in names(types)) {
    sh <- sheets[tolower(trimws(sheets)) == s]
    if (!length(sh)) stop("Melbourne: no sheet '", s, "' in ", basename(path))
    x <- read_text_sheet(path, sh[1])
    is_q <- matrix(grepl("^(Mar|Jun|Sep|Dec) \\d{4}$", x), nrow(x))
    qrow <- which.max(rowSums(is_q))
    cnt <- which(is_q[qrow, ] & tolower(x[qrow + 1, ]) == "count")
    cnt <- cnt[cnt < ncol(x) & tolower(x[qrow + 1, cnt + 1]) == "median"]
    metro <- which(apply(x[, seq_len(min(3, ncol(x))), drop = FALSE], 1,
                         function(r) any(r == "Metro")))
    if (!length(metro)) stop("Melbourne: no 'Metro' row in sheet '", sh[1], "'")
    m <- metro[length(metro)]
    lab <- x[qrow, cnt]
    out[[s]] <- data.frame(
      cell = types[[s]],
      qn = qn_of(sub(".* ", "", lab), match(sub(" .*", "", lab), MONTHS)),
      bonds = num(x[m, cnt]), median = num(x[m, cnt + 1]))
  }
  bind_rows(out) |> mutate(city = "Melbourne", source = basename(path))
}

# Homes Victoria's Metropolitan Rent Index, annual change (a check only).
read_vic_mri <- function(path) {
  sh <- excel_sheets(path)
  sh <- sh[grepl("^fig(ure)? 1 source$", tolower(trimws(sh)))]
  if (!length(sh)) {
    note("Melbourne check: ", basename(path), " has no 'Fig 1 source' sheet; check skipped")
    return(NULL)
  }
  x <- read_excel(path, sheet = sh[1], col_names = FALSE, col_types = "text",
                  .name_repair = "minimal")
  serial <- num(x[[1]]); val <- num(x[[2]])      # dates come as Excel serial numbers
  ok <- !is.na(serial) & serial > 30000 & !is.na(val)
  d <- as.Date(serial[ok], origin = "1899-12-30")
  data.frame(qn = qn_of(format(d, "%Y"), (as.integer(format(d, "%m")) + 2) %/% 3),
             mri_annual = val[ok])
}

# ---- Brisbane: Queensland RTA, medians and new bonds by LGA --------------------

BNE_LGAS  <- "^(Brisbane|Ipswich|Logan|Moreton Bay|Redland) \\("
BNE_CELLS <- c("Flat 1", "Flat 2", "Flat 3", "House 2", "House 3", "House 4",
               "Townhouse 2", "Townhouse 3")

read_rta_sheet <- function(path, pattern, value) {
  sheets <- excel_sheets(path)
  sh <- sheets[grepl(pattern, tolower(trimws(sheets)))]
  if (!length(sh)) stop("Brisbane: no sheet matching '", pattern, "' in ", basename(path))
  x <- read_text_sheet(path, sh[1])
  hit <- which(x == "Local Government Area", arr.ind = TRUE)
  if (!nrow(hit)) stop("Brisbane: no 'Local Government Area' header in sheet '", sh[1], "'")
  lga_col <- hit[1, "col"]
  dwell_col <- which(x[hit[1, "row"], ] == "Dwelling")[1]
  mrow <- which.max(rowSums(matrix(x %in% MONTHS, nrow(x))))
  qcols <- which(x[mrow, ] %in% MONTHS)
  q <- qn_of(x[mrow + 1, qcols], match(x[mrow, qcols], MONTHS))
  body <- (mrow + 2):nrow(x)
  keep <- body[grepl(BNE_LGAS, x[body, lga_col]) & x[body, dwell_col] %in% BNE_CELLS]
  vals <- matrix(num(x[keep, qcols]), nrow = length(keep))
  data.frame(lga = rep(x[keep, lga_col], times = length(qcols)),
             cell = rep(x[keep, dwell_col], times = length(qcols)),
             qn = rep(q, each = length(keep)),
             v = as.vector(vals)) |>
    setNames(c("lga", "cell", "qn", value))
}

read_rta <- function(path) {
  m <- read_rta_sheet(path, "lga-rents", "median")
  b <- read_rta_sheet(path, "lga-new-bonds", "bonds")
  lg <- unique(sub(" \\(.*", "", m$lga))
  if (length(lg) != 5) stop("Brisbane: expected five LGAs, found ", paste(lg, collapse = ", "))
  left_join(m, b, by = c("lga", "cell", "qn")) |>
    mutate(cell = paste(sub(" \\(.*", "", lga), cell, sep = ": "),
           city = "Brisbane", source = basename(path)) |>
    select(-lga)
}

# ---- Adelaide: SA Private Rental Report, one file per quarter ------------------

read_sa_file <- function(path, period) {
  sheets <- excel_sheets(path)
  sh <- sheets[grepl("region", sheets, ignore.case = TRUE)][1]
  if (is.na(sh)) stop("Adelaide: no Region sheet in ", basename(path))
  x <- read_text_sheet(path, sh)
  rc <- which.max(rowSums(tolower(x) == "count"))           # the Count/Median row
  typ <- x[rc - 2, ]                                         # dwelling type, spread right
  for (k in seq_along(typ)[-1]) if (typ[k] == "") typ[k] <- typ[k - 1]
  lab <- tolower(paste(x[rc - 1, ], typ))
  cnt <- which(tolower(x[rc, ]) == "count" &
               c(tolower(x[rc, -1]) == "median", FALSE))
  type <- ifelse(grepl("flat|unit", lab), "flat", ifelse(grepl("house", lab), "house", NA))
  bed_ok <- grepl("(\\d)\\s*\\+?\\s*(br|bed)", lab)
  bed <- ifelse(bed_ok, sub(".*?(\\d)\\s*\\+?\\s*(br|bed).*", "\\1", lab), NA)
  keep <- cnt[!is.na(type[cnt]) & !is.na(bed[cnt]) & !grepl("unknown|total", lab[cnt])]
  if (length(keep) != 8) stop("Adelaide: found ", length(keep), " cells, not 8, in ", basename(path))
  cand <- which(grepl("^metro( total)?$", tolower(x[, 1])))
  if (!length(cand)) stop("Adelaide: no Metro row in ", basename(path))
  mi <- cand[which.max(vapply(cand, function(i) sum(!is.na(num(x[i, -1]))), 0))]
  data.frame(cell = paste0(type[keep], "_", bed[keep]),
             qn = qn_of(substr(period, 1, 4), as.integer(substr(period, 6, 7)) / 3),
             bonds = num(x[mi, keep]), median = num(x[mi, keep + 1]),
             source = basename(path))
}

read_sa <- function(dir) {
  f <- list.files(dir, pattern = "\\d{4}-\\d{2}.*\\.xlsx?$", full.names = TRUE)
  per <- regmatches(basename(f), regexpr("\\d{4}-\\d{2}", basename(f)))
  pick <- tapply(seq_along(f), per, function(i) {
    if (length(i) == 1) return(i)
    am <- i[grepl("amend", basename(f[i]), ignore.case = TRUE)]
    if (length(am)) am[1] else i[which.max(file.mtime(f[i]))]
  })
  dups <- names(pick)[tapply(per, per, length) > 1]
  if (length(dups)) note("Adelaide: several files for ", paste(dups, collapse = ", "),
                         "; used the amended or newest one")
  bind_rows(lapply(names(pick), function(p) read_sa_file(f[pick[[p]]], p))) |>
    mutate(city = "Adelaide")
}

# ---- Sydney: DCJ Rent and Sales Report ------------------------------------------

SYD_CELLS <- paste0("bed_", 1:4)

# Numbers as DCJ prints them: 0.0185, "1.85E-2", "55,028", "-.69%", "2.86%".
# "s" and "-" (too few bonds) become NA. A trailing % divides by 100, so
# changes come out as fractions whichever way a table stores them.
num_dcj <- function(v) {
  s <- gsub("[,[:space:]]", "", v)
  pct <- grepl("%$", s)
  x <- num(sub("%$", "", s))
  ifelse(pct, x / 100, x)
}

# Bedroom labels to cells: "1 Bedroom", "4 or more Bedrooms", "One Bedroom"
# and "Four + Bedrooms" give bed_1 to bed_4; Bedsitter, Not Specified and
# Total give NA.
bed_cell <- function(lab) {
  l <- tolower(trimws(lab))
  for (k in 1:4) l <- sub(paste0("^", c("one", "two", "three", "four")[k], "\\b"), k, l, perl = TRUE)
  ifelse(grepl("^[1-4]\\D.*bed", l, perl = TRUE), paste0("bed_", substr(l, 1, 1)), NA_character_)
}

# The quarter a text names: a month followed by a four-digit year, as in
# "september-2019", "June Quarter 2017", "Reporting period: July - September
# 2020" or "January to March 2026". NA if there is none.
q_from_text <- function(s) {
  s <- tolower(paste(s, collapse = " "))
  rx <- "(?<![a-z])(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*[^a-z0-9]+(quarter[^a-z0-9]+)?(\\d{4})"
  m <- regmatches(s, regexec(rx, s, perl = TRUE))[[1]]
  if (!length(m)) return(NA_integer_)
  qn_of(m[4], (match(m[2], tolower(month.abb)) - 1) %/% 3 + 1)
}

# Rent tables from September 2017 on: sheet LGA, one row per region, dwelling
# type and bedrooms. Columns are found by their headings, which DCJ reworded
# over the years. Each Greater Sydney row appears twice (GMR "Total" and
# "GMR"); the copies are compared, then one is kept.
read_dcj_lga <- function(path) {
  x <- read_text_sheet(path, "LGA")
  top <- tolower(gsub("\\s+", " ", x[seq_len(min(20, nrow(x))), , drop = FALSE]))
  hr <- which(apply(top, 1, function(r) any(grepl("^median weekly rent", r))))[1]
  if (is.na(hr)) stop("Sydney: no heading row in sheet LGA of ", basename(path))
  h <- top[hr, ]
  col <- function(rx) {
    k <- which(grepl(rx, h, perl = TRUE))
    if (length(k) != 1) stop("Sydney: ", length(k), " columns match '", rx, "' in ", basename(path))
    k
  }
  c_gs <- col("^greater sydney$"); c_ring <- col("^rings?$")
  c_lga <- col("local government area|^lga\\b"); c_dw <- col("^dwelling type")
  c_bed <- col("bedroom"); c_med <- col("^median weekly rent")
  c_bond <- col("^new bonds lodged"); c_chg <- col("^(quarterly|qtly) change in median")
  body <- x[(hr + 1):nrow(x), , drop = FALSE]
  lc <- function(k) tolower(body[, k])
  b <- body[lc(c_gs) == "greater sydney" & lc(c_ring) == "total" & lc(c_lga) == "total" &
              lc(c_dw) == "total", , drop = FALSE]
  cell <- bed_cell(b[, c_bed])
  d <- data.frame(cell = cell, median = num_dcj(b[, c_med]), bonds = num_dcj(b[, c_bond]),
                  q_change = num_dcj(b[, c_chg]))[!is.na(cell), ]
  agree <- nrow(unique(d)) == length(unique(d$cell))
  d <- d[!duplicated(d$cell), ]
  if (!setequal(d$cell, SYD_CELLS))
    stop("Sydney: Greater Sydney rows for ", paste(sort(d$cell), collapse = ", "), " only in ", basename(path))
  list(cells = d, title = x[seq_len(hr - 1), 1], copies_agree = agree)
}

# The June 2017 rent tables (issue 120, an .xls in the old layout): Table 2,
# sheet lga_ad, the GREATER SYDNEY row. Medians and quarterly changes (in %)
# by bedrooms; no bond counts by bedrooms.
read_dcj_lga_ad <- function(path) {
  x <- read_text_sheet(path, "lga_ad")
  gs <- which(apply(x, 1, function(r) any(toupper(r) == "GREATER SYDNEY")))[1]
  if (is.na(gs)) stop("Sydney: no GREATER SYDNEY row in sheet lga_ad of ", basename(path))
  hb <- which(apply(x[seq_len(gs - 1), , drop = FALSE], 1,
                    function(r) sum(!is.na(bed_cell(r))) == 4))[1]
  if (is.na(hb)) stop("Sydney: no bedroom headings in sheet lga_ad of ", basename(path))
  starts <- which(!is.na(bed_cell(x[hb, ])))
  ends <- c(starts[-1] - 1, ncol(x))
  d <- bind_rows(lapply(seq_along(starts), function(k) {
    cols <- starts[k]:ends[k]
    lab <- apply(x[(hb + 1):(gs - 1), cols, drop = FALSE], 2,
                 function(v) tolower(paste(v, collapse = " ")))
    data.frame(cell = bed_cell(x[hb, starts[k]]),
               median = num_dcj(x[gs, cols[grepl("median", lab)][1]]),
               bonds = NA_real_,
               q_change = num_dcj(x[gs, cols[grepl("qtly|quarterly", lab)][1]]) / 100)
  }))
  if (!setequal(d$cell, SYD_CELLS)) stop("Sydney: bedroom columns not found in ", basename(path))
  list(cells = d, title = x[1, 1], copies_agree = TRUE)
}

# Quarter labels in the time series: "Mar-90" text or, in some readers, an
# Excel date serial. NA for anything else.
trend_q <- function(v) {
  v <- trimws(v)
  out <- rep(NA_integer_, length(v))
  m <- regmatches(v, regexec("^([A-Za-z]{3})[a-z]*[- ](\\d{2}|\\d{4})$", v))
  for (i in seq_along(v)) {
    if (length(m[[i]]) == 3) {
      mon <- match(tolower(m[[i]][2]), tolower(month.abb))
      yr <- as.integer(m[[i]][3])
      if (yr < 100) yr <- yr + if (yr >= 50) 1900L else 2000L
      if (!is.na(mon)) out[i] <- qn_of(yr, (mon - 1) %/% 3 + 1)
    } else {
      s <- num(v[i])
      if (!is.na(s) && s > 30000 && s < 60000) {
        d <- as.Date(s, origin = "1899-12-30")
        out[i] <- qn_of(format(d, "%Y"), (as.integer(format(d, "%m")) - 1) %/% 3 + 1)
      }
    }
  }
  out
}

# DCJ's time series of median rents, March 1990 to June 2017 (issue 120):
# the GREATER SYDNEY row of the all-dwellings sheets for one to four or more
# bedrooms.
read_dcj_trend <- function(path) {
  sheets <- excel_sheets(path)
  want <- c("1BR_A" = "bed_1", "2BR_A" = "bed_2", "3BR_A" = "bed_3", "4+BR_A" = "bed_4")
  bind_rows(lapply(names(want), function(s) {
    sh <- sheets[toupper(trimws(sheets)) == s]
    if (!length(sh)) stop("Sydney: no sheet ", s, " in ", basename(path))
    x <- read_text_sheet(path, sh[1])
    top <- seq_len(min(10, nrow(x)))
    hr <- top[which.max(vapply(top, function(i) sum(!is.na(trend_q(x[i, ]))), 0))]
    qc <- which(!is.na(trend_q(x[hr, ])))
    # Quarters run on by position from the first label. A stray label (sheet
    # 3BR_A calls its September 2015 column "Mar-15") is noted, not trusted.
    q_lab <- trend_q(x[hr, qc])
    q <- q_lab[1] + seq_along(q_lab) - 1L
    bad <- which(q_lab != q)
    if (!length(q) || length(bad) > 2 || tail(q_lab, 1) != tail(q, 1))
      stop("Sydney: the quarter labels in sheet ", s, " of ", basename(path), " do not run consecutively")
    if (length(bad))
      note("Sydney: sheet ", s, " of ", basename(path), " labels its ",
           paste(sprintf("%s column \"%s\"", q_label(q[bad]), x[hr, qc[bad]]), collapse = " and "),
           "; read by position")
    gs <- which(apply(x, 1, function(r) any(toupper(r) == "GREATER SYDNEY")))[1]
    if (is.na(gs)) stop("Sydney: no GREATER SYDNEY row in sheet ", s, " of ", basename(path))
    data.frame(cell = want[[s]], qn = q, median = num(x[gs, qc]), source = basename(path))
  }))
}

# Every DCJ workbook in dir, told apart by its sheets: rent tables (sheet LGA
# from September 2017, lga_ad in June 2017) and the time series (sheet 1BR_A).
read_dcj_dir <- function(dir) {
  f <- list.files(dir, pattern = "\\.xlsx?$", full.names = TRUE, ignore.case = TRUE)
  f <- f[!startsWith(basename(f), "~$")]                 # Excel's lock files
  sales <- grepl("sales", basename(f), ignore.case = TRUE)
  if (any(sales)) note("Sydney: skipped ", sum(sales), " sales workbook(s) in ", dir, ", e.g. ", basename(f[sales][1]))
  f <- f[!sales]
  tabs <- list(); trend <- list()
  for (p in f) {
    sh <- tryCatch(read_with(excel_sheets, p), error = function(e) character())
    kind <- if ("LGA" %in% sh) "lga" else if ("lga_ad" %in% sh) "lga_ad" else
      if ("1BR_A" %in% toupper(sh)) "trend" else NA
    if (is.na(kind)) {
      note("Sydney: ", basename(p), " is neither a DCJ rent table nor the rent time series; skipped")
      next
    }
    if (kind == "trend") {
      trend[[basename(p)]] <- read_with(read_dcj_trend, p)
      next
    }
    r <- read_with(if (kind == "lga") read_dcj_lga else read_dcj_lga_ad, p)
    q_name <- q_from_text(basename(p)); q_title <- q_from_text(r$title)
    qn <- if (!is.na(q_name)) q_name else q_title
    if (is.na(qn)) {
      note("Sydney: ", basename(p), " names no quarter in its file name or title; skipped")
      next
    }
    if (!is.na(q_name) && !is.na(q_title) && q_name != q_title)
      note("Sydney: ", basename(p), " is named for ", q_label(q_name), " but titled ", q_label(q_title),
           "; used ", q_label(q_name), " (the check of reported changes below tests this)")
    if (!r$copies_agree)
      note("Sydney: the two copies of the Greater Sydney rows differ in ", basename(p), "; used the first")
    tabs[[basename(p)]] <- r$cells |> mutate(qn = qn, source = basename(p))
  }
  tabs <- bind_rows(tabs)
  if (nrow(tabs)) {
    dup <- tabs |> distinct(qn, source) |> group_by(qn) |> filter(n() > 1) |> ungroup()
    if (nrow(dup)) stop("Sydney: more than one rent table for the same quarter in ", dir, ": ",
                        paste(sprintf("%s (%s)", dup$source, q_label(dup$qn)), collapse = ", "),
                        ". Keep one per quarter.")
  }
  if (length(trend) > 1) note("Sydney: several time series workbooks in ", dir, "; used ", names(trend)[1])
  list(tabs = tabs, trend = if (length(trend)) trend[[1]] else NULL)
}

read_dcj_pdf <- function(dir) {
  f <- list.files(dir, pattern = "^issue-\\d+-rent-.*\\.pdf$", full.names = TRUE)
  mon <- c(march = 1, june = 2, september = 3, december = 4)
  rows <- lapply(f, function(p) {
    b <- basename(p)
    issue <- as.integer(sub("^issue-(\\d+)-.*", "\\1", b))
    rq <- regmatches(b, regexec("rent-([a-z]+)-(\\d{4})", b))[[1]]
    qn <- qn_of(rq[3], mon[[rq[2]]])
    L <- tryCatch(unlist(strsplit(pdftools::pdf_text(p), "\n")), error = function(e) character())
    h <- grep("Table 2\\.", L)
    h <- h[grepl("all dwellings", tolower(L[h]))]
    # The Table 2 heading names the rent quarter; trust it over the file name.
    hq <- if (length(h)) regmatches(L[h[1]], regexec("(Mar|Jun|Sep|Dec)[a-z]*\\s+Quarter\\s+(\\d{4})",
                                                      L[h[1]]))[[1]] else character()
    if (length(hq) == 3) {
      qh <- qn_of(hq[3], match(hq[2], MONTHS))
      if (qh != qn) {
        note("Sydney: ", b, " is named for ", q_label(qn), " but its Table 2 covers ",
             q_label(qh), "; used ", q_label(qh))
        qn <- qh
      }
    }
    row <- if (length(h)) grep("^\\s*(GREATER SYDNEY|SYDNEY SD)\\s", L[h[1]:min(length(L), h[1] + 30)],
                               value = TRUE)[1] else NA
    if (is.na(row)) return(data.frame(issue, qn, cell = paste0("bed_", 1:4),
                                      median = NA, q_change = NA, readable = FALSE))
    tok <- strsplit(trimws(sub("^\\s*(GREATER SYDNEY|SYDNEY SD)", "", row)), "\\s+")[[1]]
    tok <- tok[tok != "s"]
    step <- if (length(tok) == 12) 3 else if (length(tok) == 8) 2 else NA
    if (is.na(step)) return(data.frame(issue, qn, cell = paste0("bed_", 1:4),
                                       median = NA, q_change = NA, readable = FALSE))
    at <- (0:3) * step
    data.frame(issue, qn, cell = paste0("bed_", 1:4),
               median = num(tok[at + 1]), q_change = num(tok[at + 2]), readable = TRUE)
  })
  bind_rows(rows) |> mutate(city = "Sydney", source = paste0("DCJ issue ", issue))
}

# Quarterly log changes from DCJ's reported changes, given as fractions
# (version 5's dcj_changes took percentages). A quarter with no reported
# change, or no readable table, is bridged: the next quarter's median and
# reported change give its median.
chain_reported <- function(d) {
  r <- d |> filter(!is.na(median)) |> arrange(cell, qn)
  all_q <- seq(min(r$qn), max(r$qn))
  out <- list()
  for (cl in unique(r$cell)) {
    x <- r |> filter(cell == cl)
    for (t in all_q) {
      cur <- x[x$qn == t, ]
      if (nrow(cur) && !is.na(cur$q_change)) {
        out[[length(out) + 1]] <- data.frame(cell = cl, qn = t, median = cur$median,
                                             dl = log(1 + cur$q_change), how = "reported")
      } else {
        prv <- x[x$qn == t - 1, ]; nxt <- x[x$qn == t + 1, ]
        if (nrow(prv) && nrow(nxt) && !is.na(nxt$q_change)) {
          implied <- nxt$median / (1 + nxt$q_change)
          out[[length(out) + 1]] <- data.frame(cell = cl, qn = t, median = implied,
                                               dl = log(implied / prv$median), how = "bridged")
        }
      }
    }
  }
  bind_rows(out)
}

# Annual log change from quarterly ones (NA unless four consecutive quarters).
annual_from_q <- function(qn, dlog) {
  vapply(seq_along(qn), function(i) {
    if (i < 4 || qn[i] - qn[i - 3] != 3) return(NA_real_)
    sum(dlog[(i - 3):i])
  }, 0)
}

# ---- Index construction ---------------------------------------------------------

# cells: cell, qn, median, bonds (for weights), dl (optional: given changes)
build_index <- function(cells, weights, city) {
  ch <- cells
  if (!"dl" %in% names(ch)) {
    ch <- ch |> arrange(cell, qn) |> group_by(cell) |>
      mutate(dl = ifelse(qn - lag(qn) == 1, log(median) - log(lag(median)), NA_real_)) |>
      ungroup()
  }
  ch |> filter(is.finite(dl)) |>
    inner_join(weights, by = "cell") |>
    group_by(qn) |>
    summarise(dlog = sum(w * dl) / sum(w), cover = sum(w), n_cells = n(), .groups = "drop") |>
    mutate(dlog = ifelse(cover >= MIN_COVER, dlog, NA_real_), city = city)
}

weights_from_bonds <- function(cells) {
  cells |> group_by(cell) |>
    summarise(w = sum(bonds, na.rm = TRUE), .groups = "drop") |>
    mutate(w = w / sum(w))
}

finish_index <- function(idx) {
  idx |> arrange(city, qn) |> group_by(city) |>
    mutate(qtr = qn %% 4L + 1L,
           dlog_sa = dlog - ave(dlog, qtr, FUN = function(v) mean(v, na.rm = TRUE)) +
             mean(dlog, na.rm = TRUE)) |>
    group_by(city) |>
    group_modify(function(g, key) {
      # level from the quarter before the first change; base 2011-12 = 100
      g <- bind_rows(data.frame(qn = min(g$qn) - 1L, dlog = NA_real_, dlog_sa = NA_real_), g) |>
        mutate(qtr = qn %% 4L + 1L)
      lv <- cumsum(ifelse(is.na(g$dlog), 0, g$dlog))
      lv_sa <- cumsum(ifelse(is.na(g$dlog_sa), 0, g$dlog_sa))
      fy <- g$qn >= qn_of(2011, 3) & g$qn <= qn_of(2012, 2)
      b <- if (any(fy)) mean(lv[fy]) else lv[1]
      b_sa <- if (any(fy)) mean(lv_sa[fy]) else lv_sa[1]
      g$index <- 100 * exp(lv - b); g$index_sa <- 100 * exp(lv_sa - b_sa)
      g
    }) |> ungroup() |>
    mutate(year = qn %/% 4L, quarter = q_label(qn)) |>
    select(city, quarter, year, qtr, dlog, dlog_sa, index, index_sa, cover, n_cells)
}

# ---- Run ------------------------------------------------------------------------

cells_out <- list(); idx_out <- list()

vic_file <- latest_file(file.path(RAW_REV, "vic"), "^quarterly_median_rents_by_local_government_area.*\\.xlsx$")
if (length(vic_file)) {
  mel <- read_with(read_vic, vic_file)
  w <- weights_from_bonds(mel)
  idx_out$mel <- build_index(mel, w, "Melbourne")
  cells_out$mel <- mel |> left_join(w, by = "cell")
  note("Melbourne: ", basename(vic_file), ", ", q_label(min(mel$qn)), " to ", q_label(max(mel$qn)),
       ", 6 cells; weights ", paste(sprintf("%s %.3f", w$cell, w$w), collapse = ", "))
} else note("Melbourne: no 'Quarterly median rents by Local Government Area' workbook found; ",
            folder_note(file.path(RAW_REV, "vic")), "; skipped")

rta_file <- latest_file(file.path(RAW_REV, "qld"), "^rta_bond_statistics.*\\.xlsx$")
if (length(rta_file)) {
  bne <- read_with(read_rta, rta_file)
  w <- weights_from_bonds(bne)
  idx_out$bne <- build_index(bne, w, "Brisbane")
  cells_out$bne <- bne |> left_join(w, by = "cell")
  note("Brisbane: ", basename(rta_file), ", ", q_label(min(bne$qn)), " to ", q_label(max(bne$qn)),
       ", ", nrow(w), " cells in five LGAs; largest weights ",
       paste(head(sprintf("%s %.3f", w$cell[order(-w$w)], sort(w$w, decreasing = TRUE)), 4), collapse = ", "))
} else note("Brisbane: no rta-bond-statistics workbook found; ",
            folder_note(file.path(RAW_REV, "qld")), "; skipped")

if (dir.exists(file.path(RAW_REV, "sa"))) {
  ade <- read_sa(file.path(RAW_REV, "sa"))
  w <- weights_from_bonds(ade)
  idx_out$ade <- build_index(ade, w, "Adelaide")
  cells_out$ade <- ade |> left_join(w, by = "cell")
  miss <- setdiff(seq(min(ade$qn), max(ade$qn)), unique(ade$qn))
  note("Adelaide: ", length(unique(ade$qn)), " quarterly files, ", q_label(min(ade$qn)), " to ",
       q_label(max(ade$qn)), if (length(miss)) paste0("; missing ", paste(q_label(miss), collapse = ", ")),
       "; weights ", paste(sprintf("%s %.3f", w$cell, w$w), collapse = ", "))
} else note("Adelaide: no raw_rev/sa folder; skipped")

nsw_dir <- file.path(RAW_REV, "nsw")
dcj <- if (dir.exists(file.path(nsw_dir, "dcj_xlsx"))) read_dcj_dir(file.path(nsw_dir, "dcj_xlsx")) else
  list(tabs = data.frame(), trend = NULL)
tabs <- dcj$tabs; trend <- dcj$trend
pdf_raw <- NULL
pdf_dir <- file.path(nsw_dir, "dcj_pdf")
if (length(list.files(pdf_dir, pattern = "\\.pdf$"))) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    pdf_raw <- read_dcj_pdf(pdf_dir)
  } else note("Sydney: R package pdftools is not installed, so the report PDFs were not read")
}

if (nrow(tabs)) {
  have <- sort(unique(tabs$qn))
  gap <- setdiff(seq(min(have), max(have)), have)
  tab_info <- tabs |> select(qn, cell, bonds, source)
  if (SYD_BEFORE_2017 == "series" && is.null(trend))
    note("Sydney: no time series workbook in ", file.path(nsw_dir, "dcj_xlsx"), "; ",
         if (is.null(pdf_raw)) "the index starts with the rent tables" else "used the report PDFs before June 2017")
  if (SYD_BEFORE_2017 == "series" && !is.null(trend)) {
    early <- trend |> arrange(cell, qn) |> group_by(cell) |>
      mutate(dl = ifelse(qn - lag(qn) == 1, log(median) - log(lag(median)), NA_real_),
             how = "time series") |> ungroup()
    syd <- bind_rows(early, chain_reported(tabs) |> filter(qn > max(early$qn)) |>
                       left_join(tab_info, by = c("qn", "cell")))
    pre <- paste0("DCJ's time series (", early$source[1], ", ", q_label(min(early$qn)), " to ",
                  q_label(max(early$qn)), ")")
    first_tab <- max(early$qn) + 1L
  } else if (!is.null(pdf_raw)) {
    comb <- bind_rows(pdf_raw |> filter(qn < min(tabs$qn)) |>
                        transmute(qn, cell, median, q_change = q_change / 100, bonds = NA_real_,
                                  source = paste0("DCJ issue ", issue, " PDF, Table 2")),
                      tabs |> select(qn, cell, median, q_change, bonds, source))
    syd <- chain_reported(comb) |> left_join(comb |> select(qn, cell, bonds, source), by = c("qn", "cell"))
    pre <- paste0("the report PDFs (", q_label(min(pdf_raw$qn)), " to ",
                  q_label(max(pdf_raw$qn[pdf_raw$qn < min(tabs$qn)])), ")")
    first_tab <- min(tabs$qn)
  } else {
    syd <- chain_reported(tabs) |> left_join(tab_info, by = c("qn", "cell"))
    pre <- NULL
    first_tab <- min(tabs$qn)
  }
  syd <- syd |>
    mutate(city = "Sydney",
           source = ifelse(how != "bridged", source,
                           paste0(ifelse(is.na(source), "no table", paste(source, "has no reported change")),
                                  "; bridged from the next quarter")))
  w <- weights_from_bonds(tabs)
  has_w <- all(is.finite(w$w))
  if (has_w) idx_out$syd <- build_index(syd, w, "Sydney") else
    note("Sydney: the rent tables give no bond counts by bedrooms, so the index has no weights; cells only")
  cells_out$syd <- syd |> left_join(w, by = "cell")
  bq <- if (has_w) range(tabs$qn[!is.na(tabs$bonds)]) else c(NA_integer_, NA_integer_)
  bridged <- sort(unique(syd$qn[syd$how == "bridged"]))
  note("Sydney: ", length(have), " DCJ rent tables, ", q_label(min(have)), " to ", q_label(max(have)),
       if (length(gap)) paste0(", none for ", paste(q_label(gap), collapse = ", ")),
       "; changes from ", q_label(first_tab), " come from the tables",
       if (!is.null(pre)) paste0(", earlier ones from ", pre),
       if (length(bridged)) paste0("; bridged ", paste(q_label(bridged), collapse = ", ")),
       if (has_w) paste0("; weights from new bonds ", q_label(bq[1]), " to ", q_label(bq[2]), ": ",
                         paste(sprintf("%s %.3f", w$cell, w$w), collapse = ", ")))

  # Each table's reported change against the previous table's median. Gaps
  # beyond rounding mean DCJ revised the earlier quarter, or a table sits in
  # the wrong quarter.
  nochg <- tabs |> filter(is.na(q_change)) |> distinct(qn, source)
  if (nrow(nochg)) note("Sydney: no reported quarterly change in ",
                        paste(sprintf("%s (%s)", nochg$source, q_label(nochg$qn)), collapse = ", "))
  rc <- tabs |> arrange(cell, qn) |> group_by(cell) |>
    mutate(gap = ifelse(qn - lag(qn) == 1, median / (1 + q_change) - lag(median), NA_real_)) |>
    ungroup() |> filter(!is.na(gap))
  big <- rc |> filter(abs(gap) > 1)
  if (nrow(rc)) note(sprintf("Sydney check: each table's reported change against the previous table's median, %d pairs of quarters and cells: %s",
               nrow(rc),
               if (nrow(big)) paste0("gaps above $1 at ", paste(sprintf("%s %s %+.1f", q_label(big$qn), big$cell, big$gap), collapse = ", "),
                                     " (DCJ revised the earlier quarter, or a table sits in the wrong quarter); all others within $",
                                     sprintf("%.2f", max(c(0, abs(rc$gap[abs(rc$gap) <= 1])))))
               else sprintf("all within $%.2f", max(abs(rc$gap)))))

  if (!is.null(trend)) {
    j <- inner_join(trend |> select(qn, cell, m_ts = median), tabs |> select(qn, cell, m_tab = median),
                    by = c("qn", "cell"))
    if (nrow(j)) note("Sydney check: time series and rent tables, medians in ", paste(q_label(unique(j$qn)), collapse = ", "),
                      ": ", paste(sprintf("%s %g and %g", j$cell, j$m_ts, j$m_tab), collapse = ", "))
  }
  if (!is.null(trend) && !is.null(pdf_raw)) {
    cmp <- pdf_raw |> filter(!is.na(median)) |> select(qn, cell, m_pdf = median) |>
      inner_join(trend |> select(qn, cell, m_ts = median), by = c("qn", "cell"))
    ts_dl <- trend |> arrange(cell, qn) |> group_by(cell) |>
      mutate(dl_ts = log(median) - log(lag(median))) |> ungroup() |> select(qn, cell, dl_ts)
    cc <- chain_reported(pdf_raw |> mutate(q_change = q_change / 100)) |>
      inner_join(ts_dl, by = c("qn", "cell")) |> group_by(cell) |>
      summarise(a = min(qn), b = max(qn), pdf = sum(dl), ts = sum(dl_ts), .groups = "drop")
    if (nrow(cmp)) note(sprintf("Sydney check: report PDFs against the time series, %s to %s: %d of %d medians equal, largest gap $%.1f; log change %s to %s, PDF chain and time series: %s",
                                q_label(min(cmp$qn)), q_label(max(cmp$qn)), sum(cmp$m_pdf == cmp$m_ts), nrow(cmp),
                                max(abs(cmp$m_pdf - cmp$m_ts)), q_label(min(cc$a) - 1L), q_label(max(cc$b)),
                                paste(sprintf("%s %.3f and %.3f", cc$cell, cc$pdf, cc$ts), collapse = ", ")))
  }
  if (has_w) {
    a1 <- idx_out$syd |> arrange(qn)
    a2 <- build_index(syd, data.frame(cell = SYD_CELLS, w = 0.25), "Sydney") |> arrange(qn)
    d_ann <- annual_from_q(a1$qn, a1$dlog) - annual_from_q(a2$qn, a2$dlog)
    note(sprintf("Sydney check: with equal weights instead, annual growth differs by at most %.2f points (mean absolute %.2f)",
                 100 * max(abs(d_ann), na.rm = TRUE), 100 * mean(abs(d_ann), na.rm = TRUE)))
  }
} else note("Sydney: no DCJ rent tables found (", folder_note(file.path(nsw_dir, "dcj_xlsx")),
            "); the index needs their bond counts for its weights; skipped")

cells <- bind_rows(lapply(cells_out, function(d) {
  d |> mutate(quarter = q_label(qn)) |>
    select(any_of(c("city", "quarter", "qn", "cell", "median", "bonds", "w", "dl", "how", "source")))
}))
write.csv(cells, file.path(CLEAN_REV, "new_lease_cells.csv"), row.names = FALSE)

if (!length(idx_out)) stop("No city has an index; the notes above say which inputs are missing")
idx <- finish_index(bind_rows(idx_out))
write.csv(idx, file.path(CLEAN_REV, "new_lease_rents.csv"), row.names = FALSE)

# ---- Checks -------------------------------------------------------------------------

for (cty in unique(idx$city)) {
  g <- idx |> filter(city == cty)
  sm <- tapply(g$dlog, g$qtr, mean, na.rm = TRUE)
  note(sprintf("%s: %d quarterly changes, %s to %s; lowest weight coverage %.2f; mean change by quarter of year %s",
               cty, sum(!is.na(g$dlog)), g$quarter[which(!is.na(g$dlog))[1]], tail(g$quarter, 1),
               min(g$cover, na.rm = TRUE), paste(sprintf("Q%d %+.4f", 1:4, sm), collapse = " ")))
}

mri_file <- latest_file(file.path(RAW_REV, "vic"), "^tables_from_rental_report.*\\.xlsx$")
if (length(mri_file) && "Melbourne" %in% idx$city) {
  mri <- tryCatch(read_with(read_vic_mri, mri_file), error = function(e) {
    note("Melbourne check: could not read ", basename(mri_file), " (", conditionMessage(e), "); skipped")
    NULL
  })
  if (!is.null(mri)) {
    m <- idx |> filter(city == "Melbourne") |> arrange(year, qtr) |>
      mutate(qn = year * 4L + qtr - 1L, ann = exp(log(index) - log(lag(index, 4))) - 1) |>
      inner_join(mri, by = "qn") |> filter(!is.na(ann))
    note(sprintf("Melbourne check: annual change of this index against Homes Victoria's Metropolitan Rent Index, %s to %s: correlation %.3f, mean absolute gap %.4f",
                 q_label(min(m$qn)), q_label(max(m$qn)), cor(m$ann, m$mri_annual), mean(abs(m$ann - m$mri_annual))))
  }
} else if ("Melbourne" %in% idx$city) {
  note("Melbourne check: no 'Tables from Rental Report' workbook found; ",
       folder_note(file.path(RAW_REV, "vic")), "; check against the Metropolitan Rent Index skipped")
}

cpi_file <- file.path("raw", "panel_main.csv")
if (file.exists(cpi_file)) {
  p <- read.csv(cpi_file) |>
    mutate(d = as.Date(quarter, format = "%d/%m/%Y"),
           qn = qn_of(format(d, "%Y"), (as.integer(format(d, "%m")) + 2) %/% 3)) |>
    select(city, qn, rents)
  for (cty in unique(idx$city)) {
    a <- idx |> filter(city == cty) |> mutate(qn = year * 4L + qtr - 1L) |>
      inner_join(p, by = c("city", "qn")) |> arrange(qn) |>
      mutate(g_nl = log(index) - log(lag(index, 4)), g_cpi = log(rents) - log(lag(rents, 4)))
    cc <- sapply(0:6, function(k) cor(lag(a$g_nl, k), a$g_cpi, use = "complete.obs"))
    note(sprintf("%s check: correlation of annual growth in CPI rents with new-lease rents k quarters earlier (k = 0 to 6, %s to %s): %s",
                 cty, q_label(min(a$qn)), q_label(max(a$qn)), paste(sprintf("%.2f", cc), collapse = " ")))
  }
}

writeLines(c(paste0("01b_new_lease_rents.R (", SCRIPT_VERSION, "), run ", format(Sys.time(), "%Y-%m-%d %H:%M")), "", checks),
           file.path(CLEAN_REV, "new_lease_checks.txt"))
message("Wrote ", file.path(CLEAN_REV, "new_lease_rents.csv"), ", new_lease_cells.csv and new_lease_checks.txt")
