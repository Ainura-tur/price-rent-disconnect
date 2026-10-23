#!/usr/bin/env Rscript
# =============================================================================
# run_all.R  --  master script: the full pipeline, from the raw data to the
#                paper's tables and figures
#
# Runs each script below in its own R session (Rscript), as if you ran it by
# hand from the repository root, so no script sees another's objects or
# packages. A step whose input step fails in this run is skipped, so it cannot
# quietly read old files. A step left out of the run is assumed to have its
# outputs from an earlier run.
#
#   step  script                         needs     writes
#   01    01_build_panel.R               raw/      clean/panel_combined
#   01a   01a_build_test_panel.R         01        clean/panel_tests
#   02    02_estimate.R                  01        estimates and robustness checks
#   02a   02a_migration_granularity.R    01        granular migration checks
#   02b   02b_break_predictive_tests.R   01a       break and Granger tests
#   02c   02c_main_tables_sa.R           01a       Tables 2, 4 and 5; Appendix C checks
#   02d   02d_unit_root_tests.R          01a       unit-root table
#   03    03_figures.R                   02        earlier figures, local projections
#   03b   03b_local_projections.R        01a       local projections, adjusted credit
#   03c   03c_lp_iv_apra.R               01a       instrumented local projections
#   03d   03d_cr_test_apra.R             01a       CR test of the APRA instruments
#   03e   03e_cr_table_figure.R          03c, 03d  Table 6 and Figure 7
#   03f   03f_descriptive_figures.R      01a       Figures 2, 3, 4 and 8
#
# 03f must run after 03_figures.R: both write fig_divergence.png and three
# other figures under the same names, and the paper uses 03f's versions.
# 03d needs lib/ivcrtest. With a matching cache in output/cr_cache/ it takes
# seconds; without one, up to a few hours (see its header).
#
# Steps 01 and 01a read raw/. Before a run that includes either, verify_raw.R
# checks that raw/ holds the 14 input files under their expected names, and
# the run stops before any step starts if it finds a problem.
#
# Logs. Each step writes its own log in run_logs/, with its full output. This
# script writes run_logs/run_all_<date>_<time>.log with the step summary, and
# run_logs/run_history.csv gets one row per log.
#
# Usage, from the repository root:
#   Rscript run_all.R            every step
#   Rscript run_all.R 02c 02d    only these steps, by code or file name
# In RStudio, source this file. To run a subset, set RUN below.
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the end of the file closes the log.
source("00_run_log.R")
start_run_log("run_all")

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

# ---- Steps ----------------------------------------------------------------------
PIPELINE <- data.frame(
  script = c("01_build_panel.R",             "01a_build_test_panel.R",
             "02_estimate.R",                "02a_migration_granularity.R",
             "02b_break_predictive_tests.R", "02c_main_tables_sa.R",
             "02d_unit_root_tests.R",        "03_figures.R",
             "03b_local_projections.R",      "03c_lp_iv_apra.R",
             "03d_cr_test_apra.R",           "03e_cr_table_figure.R",
             "03f_descriptive_figures.R"),
  needs  = c("", "01", "01", "01", "01a", "01a", "01a", "02", "01a", "01a", "01a",
             "03c 03d", "01a"),
  stringsAsFactors = FALSE)
PIPELINE$code <- sub("_.*", "", PIPELINE$script)   # 01, 01a, 02, ..., 03f

# Steps to run: all of them, unless the command line names some. To run a
# subset from RStudio, set RUN here, for example
#   RUN <- setdiff(PIPELINE$code, "03d")     # every step but the CR test
#   RUN <- c("02c", "02d")                   # these two only
RUN  <- PIPELINE$code
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) RUN <- sub("_.*", "", basename(args))
unknown <- setdiff(RUN, PIPELINE$code)
if (length(unknown))
  stop("Unknown step: ", paste(unknown, collapse = ", "), ". The steps are ",
       paste(PIPELINE$code, collapse = ", "), ".")

# ---- Helpers --------------------------------------------------------------------
RSCRIPT <- file.path(R.home("bin"),
                     if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
now  <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
span <- function(secs) if (secs < 120) sprintf("%.1f s", secs) else sprintf("%.1f min", secs / 60)
rule <- strrep("-", 58)

# The run_history.csv row that a step's own log wrote: the latest row for that
# script that started at or after `since`. NULL if the step wrote no log.
step_log <- function(script, since) {
  f <- file.path(RUN_LOG_DIR, "run_history.csv")
  h <- if (file.exists(f))
    tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(h) || !nrow(h)) return(NULL)
  h <- h[h$script == sub("\\.R$", "", script) & h$started >= since, , drop = FALSE]
  if (nrow(h)) h[nrow(h), ] else NULL
}

# Run one step in its own R session and report how it ended. Its output goes
# to the console and to its own log; a non-zero exit status means it failed.
run_step <- function(script) {
  cat("\n", rule, "\n>>> STEP: ", script, " ( ", now(), " )\n", rule, "\n", sep = "")
  since <- now()
  t0    <- Sys.time()
  status <- if (file.exists(script)) system2(RSCRIPT, script) else {
    cat("!!! ", script, " is not in this folder.\n", sep = "")
    127L
  }
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok   <- identical(as.integer(status), 0L)
  log  <- step_log(script, since)
  cat(sprintf("<<< %s %s in %s%s\n", script, if (ok) "completed" else "FAILED", span(secs),
              if (is.null(log)) "" else paste0("; log: ", file.path(RUN_LOG_DIR, log$log))))
  list(ok = ok, secs = secs, log = log)
}

# ---- Run ------------------------------------------------------------------------
cat("=============================================================\n")
cat("  Rent/price divergence -- full pipeline\n")
cat("  Started:", now(), "| R:", R.version.string, "\n")
cat("  Steps:  ", paste(PIPELINE$code[PIPELINE$code %in% RUN], collapse = " "), "\n")
cat("=============================================================\n")

# Pre-flight check of raw/. verify_raw.R ends with quit(), so it runs in its
# own R session; a non-zero exit status stops the run here.
if (any(c("01", "01a") %in% RUN) && file.exists("verify_raw.R")) {
  cat("\n>>> PRE-FLIGHT: verify_raw.R\n")
  if (!identical(as.integer(system2(RSCRIPT, "verify_raw.R")), 0L))
    stop("verify_raw.R found missing or misnamed files in raw/ (see above). ",
         "Fix raw/ and run again.", call. = FALSE)
}

# One row per step: TRUE if it completed, FALSE if it failed or was skipped,
# NA if it was not run.
done   <- setNames(rep(NA, nrow(PIPELINE)), PIPELINE$code)
tag    <- setNames(rep("--", nrow(PIPELINE)), PIPELINE$code)
timing <- setNames(rep("", nrow(PIPELINE)), PIPELINE$code)
detail <- setNames(rep("not run", nrow(PIPELINE)), PIPELINE$code)

for (k in seq_len(nrow(PIPELINE))) {
  code   <- PIPELINE$code[k]
  script <- PIPELINE$script[k]
  if (!code %in% RUN) next
  needs   <- strsplit(PIPELINE$needs[k], " ", fixed = TRUE)[[1]]
  blocked <- needs[needs %in% names(done)[!is.na(done) & !done]]
  if (length(blocked)) {
    why <- paste(PIPELINE$script[match(blocked, PIPELINE$code)], collapse = " and ")
    cat("\n", rule, "\n>>> SKIP: ", script, ", because ", why, " did not complete\n", rule,
        "\n", sep = "")
    done[code] <- FALSE; tag[code] <- "SKIP"; detail[code] <- paste("needs", why)
    next
  }
  res <- run_step(script)
  done[code]   <- res$ok
  tag[code]    <- if (res$ok) "OK" else "FAIL"
  timing[code] <- span(res$secs)
  detail[code] <- if (is.null(res$log)) "no log written" else {
    w <- res$log$warnings
    paste0(res$log$log, if (!is.na(w) && w > 0) sprintf(", %d warning%s", w, if (w == 1) "" else "s"))
  }
}

# ---- Summary --------------------------------------------------------------------
failed <- PIPELINE$script[!is.na(done) & !done]
cat("\n=============================================================\n  SUMMARY\n")
for (k in seq_len(nrow(PIPELINE))) {
  code <- PIPELINE$code[k]
  cat(sprintf("  [%-4s] %-28s %9s  %s\n", tag[code], PIPELINE$script[k], timing[code],
              detail[code]))
}
cat("\n  Outputs: clean/ and output/. Logs: ", RUN_LOG_DIR, "/, one per step.\n", sep = "")
if (length(failed))
  cat("\n!! Failed or skipped: ", paste(failed, collapse = ", "),
      ".\n   The logs of the failed steps show the errors.\n", sep = "")
cat("=============================================================\n")

end_run_log()
# From the command line, report failure in the exit status. In RStudio, stop
# here without ending the session.
if (length(failed) && !interactive()) quit(save = "no", status = 1)
