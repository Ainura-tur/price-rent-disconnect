#!/usr/bin/env Rscript
# =============================================================================
# run_all.R  --  master script for the rent/price divergence pipeline
#
# Consolidated four-stage pipeline (was nine numbered scripts):
#   00_setup.R       constants, paths, namespace, helpers      (sourced, not a step)
#   01_build_panel.R price reconstruction + combined + extended panel
#   02_estimate.R    rent/price equations, divergence, IV, rolling R^2
#   02a_migration_granularity.R  migration-disaggregation robustness (numerator
#                    horse-race, overseas/interstate wedge + leave-one-city-out,
#                    price placebo); depends on the panel from 01
#   03_figures.R     divergence figures, local projections, transmission diagram
#
# Usage:  Rscript run_all.R         (from project root, raw/ populated)
# =============================================================================

source("00_setup.R")   # constants, paths, namespace protection, helpers

# ---- logging ----------------------------------------------------------------
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create("logs", showWarnings = FALSE)
log_file <- file.path("logs", paste0("run_log_", stamp, ".txt"))
con <- file(log_file, open = "wt")
sink(con, split = TRUE); sink(con, type = "message")
on.exit({ sink(type = "message"); sink(); close(con) }, add = TRUE)

cat("=============================================================\n")
cat("  Rent/price divergence -- pipeline\n")
cat("  Started:", format(Sys.time()), "| R:", R.version.string, "\n")
cat("  Log:    ", log_file, "\n")
cat("=============================================================\n")
cat("\n---- sessionInfo() ----\n"); print(sessionInfo())

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
  ok
}

steps   <- c("01_build_panel.R", "02_estimate.R", "02a_migration_granularity.R", "03_figures.R")

# ---- Pre-flight: verify raw/ inputs before running anything ------------------
# verify_raw.R is read-only and calls quit() with status 0/1. We run it in a
# separate R process so its quit() does not end THIS session, and gate the run
# on its exit status: a missing or mis-named input stops the pipeline here with
# a clear message rather than failing deep inside stage 01.
if (file.exists("verify_raw.R")) {
  cat("\n>>> PRE-FLIGHT: verify_raw.R\n")
  rc <- system2("Rscript", "verify_raw.R")
  if (rc != 0) {
    cat("\n!! Pre-flight check failed (see above). Fix raw/ inputs and re-run.\n")
    quit(status = 1)
  }
} else {
  cat("\n(verify_raw.R not found; skipping pre-flight input check.)\n")
}

results <- vapply(steps, run_step, logical(1))

cat("\n=============================================================\n  SUMMARY\n")
for (i in seq_along(steps)) cat(sprintf("  [%s] %s\n", if (results[i]) "OK " else "FAIL", steps[i]))
cat("\n  Outputs: clean/panel_combined.{rds,csv}; output/est_*.txt,\n")
cat("           granular_*.txt (migration-disaggregation robustness),\n")
cat("           rolling_financial_r2.csv, local_projection_irf.{csv,png}, fig_*.png\n")
cat("  Finished:", format(Sys.time()), "| Log:", log_file, "\n")
cat("=============================================================\n")
if (!all(results)) { cat("\n!! One or more steps failed; see log.\n"); quit(status = 1) }