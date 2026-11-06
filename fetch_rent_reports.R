# fetch_rent_reports.R
#
# Downloads the bond-based rent reports behind the new-lease rent series.
#
#   sa  : South Australia, Private Rental Report, one workbook per quarter.
#         The file list comes from the catalogue API of data.sa.gov.au (the
#         publisher), with data.gov.au as a fallback. Saved to raw_rev/sa/.
#   nsw : NSW Rent and Sales Report PDFs, issues 83 to 119 (rent quarters
#         March 2008 to March 2017). Saved to raw_rev/nsw/dcj_pdf/.
#
# Run from the repository folder:
#   Rscript fetch_rent_reports.R          # both sources
#   Rscript fetch_rent_reports.R sa       # one source
#
# Files already saved are skipped, so rerun the script after any failure.
# Each source writes inventory.csv next to its files. A file counts as saved
# only if its first bytes show it is a PDF or an Excel workbook, so an error
# page is never kept under a data file's name.
#
# Some SA files are old-style .xls workbooks although the catalogue labels
# them XLSX. The script names each file by what it contains:
# private-rental-report-YYYY-MM.xlsx or .xls, YYYY-MM being the last month of
# the quarter.

suppressPackageStartupMessages({
  library(jsonlite)
  library(curl)
  library(dplyr)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

args <- commandArgs(trailingOnly = TRUE)
todo <- if (length(args)) tolower(args) else c("sa", "nsw")

# Addresses. The environment variables exist only to test the script
# against a local stand-in; leave them unset.
SA_API <- strsplit(Sys.getenv("FETCH_SA_API", paste(
  "https://data.sa.gov.au/data/api/3/action/package_show?id=8eb97a72-9919-448b-8de6-fc1530b3f7ec",
  "https://data.sa.gov.au/data/api/3/action/package_show?id=private-rent-report",
  "https://data.gov.au/data/api/3/action/package_show?id=private-rent-report",
  sep = ",")), ",")[[1]]
DCJ_BASE <- Sys.getenv("FETCH_DCJ_BASE", paste0(
  "https://dcj.nsw.gov.au/documents/about-us/families-and-communities-statistics/",
  "housing-and-rent-sales/previous-rent-and-sales-reports/"))
DCJ_ISSUES <- eval(parse(text = Sys.getenv("FETCH_DCJ_ISSUES", "83:119")))
PAUSE <- as.numeric(Sys.getenv("FETCH_PAUSE", "5"))   # seconds between retries

new_h <- function() {
  new_handle(useragent = "Mozilla/5.0 (compatible; research data download)",
             followlocation = TRUE, connecttimeout = 30, timeout = 300)
}

# What a file holds, judged by its first bytes.
file_kind <- function(path) {
  if (!file.exists(path) || file.size(path) < 1000) return("none")
  magic <- readBin(path, "raw", 8)
  if (identical(magic[1:4], charToRaw("%PDF"))) return("pdf")
  if (identical(magic[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04)))) return("xlsx")
  if (identical(magic[1:8], as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1)))) return("xls")
  "other"
}

# Downloads url to a temporary file, retrying up to three times. Returns the
# temporary path and the kind of file received, or the last error.
download_try <- function(url) {
  msg <- "no attempt"
  for (attempt in 1:3) {
    tmp <- tempfile(fileext = ".part")
    res <- tryCatch({ curl_download(url, tmp, handle = new_h(), quiet = TRUE); "ok" },
                    error = function(e) conditionMessage(e))
    kind <- file_kind(tmp)
    if (identical(res, "ok") && kind %in% c("pdf", "xlsx", "xls"))
      return(list(path = tmp, kind = kind, msg = "ok"))
    msg <- if (identical(res, "ok")) paste("received", kind, "content, not a data file") else res
    unlink(tmp)
    if (attempt < 3) Sys.sleep(PAUSE * attempt)
  }
  list(path = NA_character_, kind = NA_character_, msg = substr(msg, 1, 200))
}

# ---- South Australia ---------------------------------------------------------

fetch_sa <- function() {
  dir <- file.path("raw_rev", "sa")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  res <- NULL
  for (api in SA_API) {
    r <- tryCatch(curl_fetch_memory(api, handle = new_h()), error = function(e) NULL)
    if (is.null(r) || r$status_code != 200) next
    j <- tryCatch(fromJSON(rawToChar(r$content)), error = function(e) NULL)
    if (!is.null(j) && isTRUE(j$success) && length(j$result$resources)) {
      res <- j$result$resources
      message("SA: file list from ", api)
      break
    }
  }
  if (is.null(res)) {
    message("SA: no catalogue address answered. Download the files by hand from ",
            "https://www.data.gov.au/data/dataset/private-rent-report into ", dir,
            ", named private-rental-report-YYYY-MM.xlsx (or .xls).")
    return(invisible(NULL))
  }

  res <- as.data.frame(res)[, c("name", "url")]
  res$period <- sub(".*?(\\d{4})-(\\d{2}).*", "\\1-\\2", res$name)
  res <- res[grepl("^\\d{4}-\\d{2}$", res$period), ]
  dup <- duplicated(res$period)
  if (any(dup)) message("SA: more than one file for ", paste(unique(res$period[dup]), collapse = ", "),
                        "; keeping the first listed")
  res <- res[!dup, ]
  res <- res[order(res$period), ]
  message("SA: ", nrow(res), " quarterly files listed, ", res$period[1], " to ",
          res$period[nrow(res)])

  inv <- vector("list", nrow(res))
  for (i in seq_len(nrow(res))) {
    stem <- file.path(dir, paste0("private-rental-report-", res$period[i]))
    have <- c(paste0(stem, ".xlsx"), paste0(stem, ".xls"))
    have <- have[vapply(have, function(p) file_kind(p) %in% c("xlsx", "xls"), logical(1))]
    if (length(have)) {
      status <- "already saved"; dest <- have[1]
    } else {
      d <- download_try(res$url[i])
      if (is.na(d$path)) {
        status <- paste("failed:", d$msg); dest <- NA_character_
      } else {
        dest <- paste0(stem, ".", d$kind)
        file.copy(d$path, dest, overwrite = TRUE); unlink(d$path)
        status <- "saved"
      }
    }
    inv[[i]] <- data.frame(period = res$period[i], name = res$name[i], url = res$url[i],
                           file = if (is.na(dest)) NA_character_ else basename(dest),
                           kind = if (is.na(dest)) NA_character_ else file_kind(dest),
                           bytes = if (is.na(dest)) NA_real_ else file.size(dest),
                           status = status)
    message(sprintf("SA %s: %s", res$period[i], status))
  }
  inv <- do.call(rbind, inv)
  write.csv(inv, file.path(dir, "inventory.csv"), row.names = FALSE)
  message("SA: ", sum(inv$status != "already saved" & !grepl("^failed", inv$status)), " saved now, ",
          sum(inv$status == "already saved"), " already saved, ",
          sum(grepl("^failed", inv$status)), " failed (see ", file.path(dir, "inventory.csv"), ")")
}

# ---- NSW ---------------------------------------------------------------------

# Issue 119 reports rents for the March quarter 2017 and sales for the
# December quarter 2016; each earlier issue is one quarter earlier. The file
# names follow that pattern, for example
# issue-84-rent-june-2008-sales-march-2008.pdf. Issue 100's name on DCJ's
# site says "sales-march-2011" although its sales quarter is March 2012.
DCJ_NAMES <- c("100" = "issue-100-rent-june-2012-sales-march-2011.pdf")
dcj_file <- function(issue) {
  if (as.character(issue) %in% names(DCJ_NAMES)) return(DCJ_NAMES[[as.character(issue)]])
  months <- c("march", "june", "september", "december")
  qn <- 2017 * 4 - (119 - issue)            # rent quarter, counted as year * 4 + quarter - 1
  sq <- qn - 1                              # sales quarter
  sprintf("issue-%d-rent-%s-%d-sales-%s-%d.pdf", issue,
          months[qn %% 4 + 1], qn %/% 4, months[sq %% 4 + 1], sq %/% 4)
}

fetch_nsw <- function() {
  dir <- file.path("raw_rev", "nsw", "dcj_pdf")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  inv <- vector("list", length(DCJ_ISSUES))
  for (k in seq_along(DCJ_ISSUES)) {
    n <- DCJ_ISSUES[k]; f <- dcj_file(n); dest <- file.path(dir, f)
    if (file_kind(dest) == "pdf") {
      status <- "already saved"
    } else {
      d <- download_try(paste0(DCJ_BASE, f))
      if (is.na(d$path)) {
        status <- paste("failed:", d$msg)
      } else if (d$kind != "pdf") {
        status <- paste("failed: received", d$kind); unlink(d$path)
      } else {
        file.copy(d$path, dest, overwrite = TRUE); unlink(d$path); status <- "saved"
      }
    }
    inv[[k]] <- data.frame(issue = n, file = f, url = paste0(DCJ_BASE, f),
                           bytes = if (file.exists(dest)) file.size(dest) else NA_real_,
                           status = status)
    message(sprintf("NSW issue %d: %s", n, status))
  }
  inv <- do.call(rbind, inv)
  write.csv(inv, file.path(dir, "inventory.csv"), row.names = FALSE)
  nf <- sum(grepl("^failed", inv$status))
  message("NSW: ", sum(inv$status == "saved"), " saved now, ",
          sum(inv$status == "already saved"), " already saved, ", nf, " failed",
          if (nf) paste0(". Failed issues may have another file name; find them on ",
                         "https://dcj.nsw.gov.au/about-us/families-and-communities-statistics/",
                         "housing-rent-and-sales/previous-rent-and-sales-reports.html ",
                         "and save them in ", dir, " under the name listed in inventory.csv."))
}

if ("sa" %in% todo)  fetch_sa()
if ("nsw" %in% todo) fetch_nsw()
