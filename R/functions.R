# =============================================================================
# R/functions.R  --  helpers for _targets.R
#
# Two jobs:
#   1. run_stage(): source an existing stage script in a clean environment and
#      return the path(s) it produced, so targets can hash the output and decide
#      what is downstream-stale. The `inputs` argument is not used inside the
#      function; it exists so the target EXPRESSION references upstream targets,
#      which is how targets infers the dependency edges. Listing the inputs in
#      the call is what wires the graph.
#   2. The *_output_files() / raw_input_paths() listers: enumerate the files each
#      stage reads or writes, so the graph tracks them as file targets.
#
# This keeps the analysis scripts untouched: they still source 00_setup.R and do
# their own reads/writes. We only wrap them.
# =============================================================================

# Source a stage script in a fresh environment (mirrors run_all.R's new.env()
# isolation), then return the artefact path(s) it wrote. Returning the paths with
# format="file" upstream lets targets hash them and propagate staleness.
#
#   script   : path to the stage script (sourced as-is)
#   inputs   : upstream target(s) this stage depends on. NOT used in the body;
#              naming them in the target expression is what creates the edge.
#   produces : output path(s) that MUST exist after the run (hashed by targets).
#   optional : output path(s) that MAY exist (conditional outputs, e.g. the
#              transmission diagram); included in the return when present, but
#              their absence is not an error.
run_stage <- function(script, inputs = NULL, produces, optional = character(0)) {
  force(inputs)                       # ensure the dependency is realised first
  stopifnot(file.exists(script))
  tryCatch(
    source(script, echo = FALSE, local = new.env()),
    error = function(e)
      stop(sprintf("Stage '%s' failed: %s", script, conditionMessage(e)))
  )
  missing <- produces[!file.exists(produces)]
  if (length(missing))
    stop(sprintf("Stage '%s' did not produce required: %s",
                 script, paste(missing, collapse = ", ")))
  present_optional <- optional[file.exists(optional)]
  c(produces, present_optional)
}

# ---- Raw inputs consumed by stage 01 ----------------------------------------
# Every file under raw/ that 01_build_panel.R reads. Changing any one of these
# invalidates the panel and everything downstream. (The three modern-API names
# also live in 00_setup.R as F_LEND/F_OMAD/F_D2; listed here by basename so the
# graph tracks them without sourcing setup.)
raw_input_paths <- function(raw = "raw") {
  files <- c(
    # legacy ABS xlsx workbooks
    "641601.xlsx", "310102.xlsx", "87520039.xlsx",
    "6401010.xlsx", "634502b.xlsx", "62020010.xlsx",
    # ABS data-API csv
    "ABS_RES_DWELL_ST_1_0_0__1_2_3_4_5_Q.csv",
    "ABS_LEND_HOUSING_1_1_______10_20_1_2_3_4_5_6_7_8_AUS_Q.csv",
    "ABS_OMAD_VISA___11_12_15_22_23_24_25_1009_1010_1020_1030_1040_1041_2203_2208_01_02_03_1_2_3_4_5_AUS_Q.csv",
    "ABS_ANA_SFD_1_0_0_C_FCE_RDSH__20_2_3_4_5_6_1_Q.csv",
    "ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_20_2_3_4_5_6_1_Q.csv",
    # non-ABS
    "bis_dp_search_export_20260615-045639.csv",
    "d02hist.xlsx",
    "panel_main.csv"
  )
  paths <- file.path(raw, files)
  present <- paths[file.exists(paths)]
  missing <- setdiff(paths, present)
  # Hard-stop the graph build if any expected input is absent, mirroring the
  # verify_raw.R / run_all.R pre-flight. A missing or mis-named input should fail
  # here with a clear list, not deep inside stage 01. (Run `Rscript verify_raw.R`
  # for the rename-vs-missing diagnosis; ABS downloads need underscore-renaming,
  # see raw/README.md.)
  if (length(missing))
    stop("Missing raw input(s) under '", raw, "/':\n  ",
         paste(basename(missing), collapse = "\n  "),
         "\nRun `Rscript verify_raw.R` to check names; see raw/README.md.")
  present
}

# ---- Stage 02 outputs (estimation) ------------------------------------------
# Listed so the graph treats the estimation result files as this stage's product.
# We return the subset that exists after the run (run_stage checks a key file).
est_output_files <- function(out = "output") {
  file.path(out, c(
    "est_rent.txt", "est_rent_mg.txt",
    "est_price.txt", "est_price_predetermined.txt",
    "est_rent_divergence.txt", "est_rent_divergence_pre.txt",
    "est_price_divergence.txt", "est_price_divergence_pre.txt",
    "est_price_divergence_covid.txt", "est_price_divergence_sydney.txt",
    "robust_tightness.txt", "robust_rent_stability.txt",
    "robust_credit_share.txt", "robust_credit_share_control.txt",
    "robust_switchflow_control.txt", "robust_switchflow_diagnostic.txt",
    "robust_cointegration.txt", "robust_lp_lags.txt", "robust_wildboot.txt",
    "mech_power_diagnostic.txt",
    "mech_rent_share_level.txt",
    "mech_rent_tightness_x_student.txt", "mech_rent_tightness_x_temp.txt",
    "mech_price_tightness_x_student.txt",
    "nlin_rent_tightness_spline.txt",
    "nlin_rent_tightness_threshold.txt", "nlin_price_tightness_threshold.txt",
    "alt_rent_trend.txt", "alt_price_spline.txt", "alt_cointegration_westerlund.txt",
    "alt_rent_rolling_coef.csv",
    "rolling_financial_r2.csv"
  ))
}

# ---- Stage 02a outputs (migration granularity) ------------------------------
granular_output_files <- function(out = "output") {
  file.path(out, c(
    "granular_rent_tightness.txt",
    "granular_rent_tightness_no_chk.txt",
    "granular_rent_tightness_no_stu.txt",
    "granular_rent_tightness_rw.txt",
    "granular_rent_split_flows.txt",
    "granular_rent_split_waldeq.txt",
    "granular_rent_split_loo.txt",
    "granular_price_placebo.txt",
    "granular_price_placebo_alone.txt"
  ))
}

# ---- Stage 03 outputs (figures + LP) ----------------------------------------
# Figure 1 (the policy-transmission diagram) is drawn in TikZ in the manuscript
# .tex, not generated by R, so no diagram outputs are tracked here.
figure_output_files <- function(out = "output") {
  file.path(out, c(
    "local_projection_irf.csv", "local_projection_irf.png",
    "lp_rent_lag_sensitivity.csv", "fig_lp_rent_lag_sensitivity.png",
    "fig_divergence.png", "fig_price_rent_gap.png",
    "fig_real_rent.png", "fig_rental_trajectories.png",
    "fig_rent_rolling_coef.png"
  ))
}

# If a stage gains conditional outputs (produced only under some flag), pass only
# the always-present key file(s) to run_stage()'s `produces`, and use its
# `optional` argument for the rest so their absence is not an error.
