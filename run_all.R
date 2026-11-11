#!/usr/bin/env Rscript
# =============================================================================
# run_all.R  --  master script for the rent/price divergence pipeline
#
# Pipeline (each stage reads the panel from 01 and earlier stages' outputs):
#   00_setup.R       constants, paths, namespace, helpers      (sourced, not a step)
#   01_build_panel.R price reconstruction + combined + extended panel
#   01b_new_lease_rents.R  rents on new leases from the state bond reports in
#                    raw_rev/ (Online Appendix F); skipped if raw_rev/ is absent
#   02_estimate.R    Tables 3-4, Table C.1, rolling R^2, Appendix E robustness
#   02a_migration_granularity.R  migration-disaggregation robustness
#   02c_sa_additions.R  price momentum, dynamic rent equation (omega), local
#                    projections (Figure 5, Figure E.1), channel test, checks
#   02d_sa_tables.R  Tables C.2-C.3, stacked tests, seasonal, Sydney, cash rate,
#                    Tables 1-2 summary statistics, Table E.1 unit-root tests
#   02f_new_lease_tests.R  new-lease rents against the model (Figure 6,
#                    Tables F.1-F.2); reads clean_rev/new_lease_rents.csv
#                    (committed; 01b rebuilds it)
#   03_figures.R     Figure 1 and descriptive figures (Figures 2-4)
#   03f_missing_figures.R  APRA instruments: Table D.1, Figures D.1-D.2
#   03g_cr_endpoints.R     MCUB valid-range endpoints (needs the ivcrtest package)
#   04_model_matching.R    model estimation, decomposition, counterfactual
#   04b_identification.R   distance profiles and sensitivity (identification)
#   04d_purchase_volumes.R investor purchase volumes and loan numbers (Section
#                          6.3, Table 6); uses R/fast_solver.R once it matches
#                          04's solver
#   05_bootstrap.R         block bootstrap of the model, 22 chunks of draws, each
#                          in its own R process (six at once on Windows)
#   05c_small_sample.R     small-sample Monte Carlo built on the bootstrap draws
#   05b_bootstrap_summary.R  Tables 5 and 7 intervals, bands on Figures 7 and 9
#                    These three take most of the run time, so they run only
#                    with --bootstrap.
#   04c_inflation.R  credit conditions and measured inflation (Section 6.7,
#                    Figure 10); reads the bootstrap draws, so it runs after them,
#                    or in a later run that finds a full set in output/
#   06_paper_figures.R  redraws Figures 5, 7-9, D.1-D.2 and E.1-E.2 in one style
#                    from saved results; runs last, because earlier stages write
#                    older versions of the same files
#
# Usage:  Rscript run_all.R               all stages except the bootstrap
#         Rscript run_all.R --bootstrap   all stages including the bootstrap
# From the R console, with the project folder as the working directory:
#         system2(file.path(R.home("bin"), "Rscript"), c("run_all.R", "--bootstrap"))
# =============================================================================
source("00_setup.R")   # constants, paths, namespace protection, helpers

options(warn = 1)      # print warnings as they happen, so they reach the log

# Rscript from this R installation, so the run does not depend on PATH.
RSCRIPT <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

# Bootstrap chunks to run side by side. Windows cannot fork, so this is how the
# bootstrap uses several cores there. On macOS and Linux 05_bootstrap.R forks
# inside each chunk, so the chunks run one at a time.
BOOT_PROCS <- if (.Platform$OS.type == "windows") min(6L, parallel::detectCores(), na.rm = TRUE) else 1L

# Run R on these arguments in its own process. Returns what it printed and its
# exit status.
run_r <- function(args, rscript = RSCRIPT) {
  out <- suppressWarnings(system2(rscript, as.character(args), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  list(out = out, status = if (is.null(st)) 0L else as.integer(st))
}

# ---- run one stage in a clean env, time it, capture errors ------------------
run_step <- function(script) {
  cat("\n----------------------------------------------------------\n")
  cat(">>> STEP:", script, "(", format(Sys.time()), ")\n")
  cat("----------------------------------------------------------\n")
  if (!file.exists(script)) { cat("!! SKIPPED -- not found:", script, "\n"); return(FALSE) }
  t0 <- Sys.time()
  ok <- tryCatch({ source(script, echo = FALSE, local = new.env()); TRUE },
                 error = function(e) { cat("!! ERROR in", script, ":\n   ", conditionMessage(e), "\n"); FALSE })
  cat(sprintf("<<< %s %s in %.1f s\n", script, if (ok) "completed" else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  flush(con)   # so the log can be read while the run continues
  ok
}

# ---- the pipeline body, wrapped so on.exit() and sink() behave correctly ----
# Everything that should be logged lives inside main(). on.exit() only works
# inside a function: at the top level of a script it fires immediately and
# closes the sink before any results are written (which truncates the log to a
# single "session ended" line). Wrapping the body fixes that and lets the driver
# below capture every step's output to the log.
main <- function() {
  # Rents on new leases (Online Appendix F): 01b builds
  # clean_rev/new_lease_rents.csv from the state bond reports in raw_rev/,
  # which the repository does not hold (README.md lists the sources). The
  # repository includes that file, so without raw_rev/ 01b is skipped and 02f
  # runs on the committed copy.
  steps <- "01_build_panel.R"
  if (dir.exists("raw_rev")) steps <- c(steps, "01b_new_lease_rents.R")
  else cat("\n(raw_rev/ not found; skipping 01b_new_lease_rents.R, which rebuilds clean_rev/new_lease_rents.csv from the bond reports listed in README.md.)\n")
  steps <- c(steps, "02_estimate.R", "02a_migration_granularity.R",
             "02c_sa_additions.R", "02d_sa_tables.R")
  if (dir.exists("raw_rev") || file.exists(file.path("clean_rev", "new_lease_rents.csv")))
    steps <- c(steps, "02f_new_lease_tests.R")
  else cat("\n(clean_rev/new_lease_rents.csv not found; skipping 02f_new_lease_tests.R.)\n")
  steps <- c(steps, "03_figures.R", "03f_missing_figures.R")
  if (requireNamespace("ivcrtest", quietly = TRUE)) steps <- c(steps, "03g_cr_endpoints.R")
  else cat("\n(ivcrtest not installed; skipping 03g_cr_endpoints.R. Install with\n",
           " remotes::install_github('Ainura-tur/ivcrtest').)\n")
  steps <- c(steps, "04_model_matching.R", "04b_identification.R", "04d_purchase_volumes.R")
  
  # Pre-flight: verify raw/ inputs before running anything.
  # verify_raw.R is read-only and calls quit() with status 0/1. We run it in a
  # separate R process so its quit() does not end THIS session, and gate the run
  # on its exit status: a missing or mis-named input stops the pipeline here with
  # a clear message rather than failing deep inside stage 01.
  if (file.exists("verify_raw.R")) {
    cat("\n>>> PRE-FLIGHT: verify_raw.R\n")
    r <- run_r("verify_raw.R")
    writeLines(r$out)
    if (r$status != 0) {
      cat("\n!! Pre-flight check failed (see above). Fix raw/ inputs and re-run.\n")
      return(invisible(FALSE))
    }
  } else {
    cat("\n(verify_raw.R not found; skipping pre-flight input check.)\n")
  }
  
  results <- vapply(steps, run_step, logical(1))

  # Block bootstrap: each chunk of 30 draws runs in its own R process and is
  # saved as output/boot_b<block>_<first>_<last>.rds, so an interrupted run
  # resumes where it stopped. 399 draws with blocks of 8 quarters and 120 each
  # with blocks of 4 and 12 (Section 5.6).
  if (RUN_BOOT) {
    chunks <- list()
    for (bt in list(c(8, 399), c(4, 120), c(12, 120)))
      for (a in seq(1, bt[2], by = 30)) chunks[[length(chunks) + 1L]] <- c(bt[1], a, min(a + 29, bt[2]))
    chunk_file <- function(x) file.path("output", sprintf("boot_b%d_%d_%d.rds", x[1], x[2], x[3]))
    todo <- Filter(function(x) !file.exists(chunk_file(x)), chunks)
    cat(sprintf("\nBootstrap chunks reused from an earlier run: %d of %d\n",
                length(chunks) - length(todo), length(chunks)))

    # Run a batch of chunks, side by side if a cluster is given, log what they
    # print, and check that each one saved its file.
    boot_ok <- TRUE
    run_batch <- function(batch, cl = NULL) {
      cat(sprintf(">>> bootstrap %s ( %s )\n", format(Sys.time()),
                  paste(sapply(batch, function(x) sprintf("block %d draws %d-%d", x[1], x[2], x[3])), collapse = ", ")))
      args <- lapply(batch, function(x) c("05_bootstrap.R", x))
      res <- if (is.null(cl)) lapply(args, run_r)
             else tryCatch(parallel::clusterApply(cl, args, run_r, rscript = RSCRIPT),
                           error = function(e) { cat("!!", conditionMessage(e), "\n"); NULL })
      for (i in seq_along(batch)) {
        if (!is.null(res)) writeLines(res[[i]]$out)
        if (is.null(res) || res[[i]]$status != 0 || !file.exists(chunk_file(batch[[i]]))) {
          cat(sprintf("!! bootstrap chunk failed: %s. Run again to retry it.\n", chunk_file(batch[[i]])))
          boot_ok <<- FALSE
        }
      }
      flush(con)
    }
    # 05_bootstrap.R builds output/boot_panel.rds on first use, so one chunk runs
    # first, alone, and the others read that file.
    if (length(todo) && !file.exists(file.path("output", "boot_panel.rds"))) {
      run_batch(todo[1]); todo <- todo[-1]
    }
    cl <- NULL
    if (BOOT_PROCS > 1L && length(todo) > 1L) {
      cl <- tryCatch({
        k <- parallel::makeCluster(BOOT_PROCS)
        parallel::clusterCall(k, setwd, getwd())
        k
      }, error = function(e) {
        cat("Could not start", BOOT_PROCS, "workers, so the chunks will run one at a time:", conditionMessage(e), "\n")
        NULL
      })
    }
    if (boot_ok && length(todo))
      for (batch in split(todo, ceiling(seq_along(todo) / max(1L, length(cl))))) run_batch(batch, cl)
    if (!is.null(cl)) parallel::stopCluster(cl)
    results <- c(results, "05_bootstrap.R" = boot_ok)
    # 05c sources 04_model_matching.R, which redraws Figures 7 and 9 without bands,
    # so 05b, which draws them with bands, must run after it. Both need every chunk.
    if (boot_ok) {
      results <- c(results, "05c_small_sample.R" = run_step("05c_small_sample.R"))
      results <- c(results, "05b_bootstrap_summary.R" = run_step("05b_bootstrap_summary.R"))
    }
    steps <- names(results)
  }

  # 04c reads the block-8 bootstrap draws despite its number, so it runs once a
  # full set exists. 06 redraws eight figures that 02c, 03_figures, 03f, 04,
  # 04b, 05c and 05b also write, so it runs last.
  have_draws <- length(list.files("output", pattern = "^boot_b8_\\d+_\\d+\\.rds$")) > 0
  if (have_draws && (!RUN_BOOT || isTRUE(results[["05_bootstrap.R"]])))
    results <- c(results, "04c_inflation.R" = run_step("04c_inflation.R"))
  else cat("\n(no full set of bootstrap draws in output/; skipping 04c_inflation.R. Run with --bootstrap.)\n")
  results <- c(results, "06_paper_figures.R" = run_step("06_paper_figures.R"))
  steps <- names(results)

  cat("\n=============================================================\n  SUMMARY\n")
  for (i in seq_along(steps)) cat(sprintf("  [%s] %s\n", if (results[i]) "OK " else "FAIL", steps[i]))
  cat("\n  Outputs: clean/panel_combined.{rds,csv}; output/est_*, robust_*, sa_*,\n")
  cat("           sa2_*, lpiv_apra.csv, new_lease_*, model_*, purchase_*, boot_* and fig_*.png;\n")
  cat("           clean_rev/new_lease_*.csv from 01b. See README.md\n")
  cat("           for which file holds each table and figure.\n")
  
  invisible(all(results))
}

# ---- logging + driver -------------------------------------------------------
# A timestamped log is written to logs/ and kept as the run-of-record. Output
# and messages are both sinked with split = TRUE, so they appear on the console
# and in the file at once. sessionInfo() is printed into the log and also
# persisted as its own artifact so package versions can be diffed across runs.
RUN_BOOT <- "--bootstrap" %in% commandArgs(trailingOnly = TRUE)
stamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create("logs", showWarnings = FALSE)
log_file <- file.path("logs", paste0("run_log_", stamp, ".txt"))
con      <- file(log_file, open = "wt")

run_t0 <- Sys.time()
ok_all <- NA
tryCatch({
  sink(con, split = TRUE)
  sink(con, type = "message")
  
  cat("=============================================================\n")
  cat("  Rent/price divergence -- pipeline\n")
  cat("  Started:", format(run_t0), "| R:", R.version.string, "\n")
  cat("  Host:   ", Sys.info()[["nodename"]], "| user:", Sys.info()[["user"]], "\n")
  cat("  WD:     ", getwd(), "\n")
  cat("  Log:    ", log_file, "\n")
  cat("=============================================================\n")
  cat("\n---- sessionInfo() ----\n"); print(sessionInfo())
  writeLines(capture.output(sessionInfo()),
             file.path("logs", paste0("sessionInfo_", stamp, ".txt")))

  # Which copy of each script ran, for the record.
  cat("\n---- script checksums (first 8 characters of the MD5) ----\n")
  scripts <- c(sort(list.files(pattern = "[.]R$")), sort(list.files("R", pattern = "[.]R$", full.names = TRUE)))
  cat(sprintf("  %s  %s\n", substr(unname(tools::md5sum(scripts)), 1, 8), scripts), sep = "")
  
  ok_all <- main()   # <- all step output is captured here, inside the sink
  
  cat(sprintf("\n  Finished: %s | elapsed %.1f min | Log: %s\n",
              format(Sys.time()),
              as.numeric(difftime(Sys.time(), run_t0, units = "mins")),
              log_file))
  cat("=============================================================\n")
  if (!isTRUE(ok_all)) cat("\n!! One or more steps failed; see log.\n")
}, finally = {
  # Release message and output sinks (in reverse order) and close the file.
  # This runs whether the body completed, errored, or returned early.
  sink(type = "message")
  sink()
  close(con)
})

# Set the process exit status only when run non-interactively (Rscript).
if (!interactive()) quit(status = if (isTRUE(ok_all)) 0L else 1L)