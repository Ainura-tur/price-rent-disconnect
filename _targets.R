# =============================================================================
# _targets.R  --  dependency-tracked pipeline for the rent/price divergence study
#
# Runs the same four-stage analysis as run_all.R, but as a {targets} dependency
# graph: tar_make() reruns ONLY the stages whose inputs changed. Edit the rent
# equation and only 02 + the figures that read its outputs rebuild; the panel
# build (01) and the migration-granularity stage (02a) are skipped because their
# inputs are unchanged. Drop a new raw file and 01 (and everything downstream)
# goes stale automatically.
#
# DESIGN: this wraps the EXISTING stage scripts rather than rewriting their logic
# into pure functions. Each stage script is unchanged and still does its own
# reads/writes; the targets layer tracks (a) the raw input files each stage
# consumes and (b) the artefact each stage produces, so the graph is correct
# without touching the analysis code. The functions are in R/functions.R.
#
# Usage:
#   tar_make()             run/refresh whatever is out of date
#   tar_visnetwork()       see the dependency graph and what's stale
#   tar_outdated()         list targets that would rebuild
#   tar_read(est_rent)     pull a result object back into the session
#
# Quarterly refresh: edit TARGET_END / F_* in 00_setup.R, drop new files in raw/,
# update cash_path in 01_build_panel.R, then tar_make(). Only the affected
# branch of the graph recomputes.
# =============================================================================

library(targets)

# Stage scripts source 00_setup.R themselves and assume the project root as the
# working directory (raw/, clean/, output/ are relative). targets runs targets
# in the project root by default, so the relative paths resolve unchanged.
tar_source("R/functions.R")   # stage wrappers + file-tracking helpers

tar_option_set(
  packages = c("dplyr","tidyr","readr","readxl","zoo","purrr",
               "plm","lmtest","sandwich","car","splines","ggplot2"),
  format   = "rds"
)

list(

  # --- Inputs: track the raw files AND the stage scripts ---------------------
  # tar_target(..., format = "file") makes targets hash these paths; if a file's
  # contents change, every downstream target that depends on it goes stale. The
  # stage scripts are tracked too, so editing the rent equation in 02 reruns 02
  # (and its dependents) even though the panel is unchanged.
  tar_target(setup_file,    "00_setup.R",                 format = "file"),
  tar_target(raw_inputs,    raw_input_paths(),            format = "file"),
  tar_target(script_01,     "01_build_panel.R",           format = "file"),
  tar_target(script_02,     "02_estimate.R",              format = "file"),
  tar_target(script_02a,    "02a_migration_granularity.R",format = "file"),
  tar_target(script_03,     "03_figures.R",               format = "file"),
  tar_target(script_02c,    "02c_sa_additions.R",         format = "file"),
  tar_target(script_02d,    "02d_sa_tables.R",            format = "file"),
  tar_target(script_03f,    "03f_missing_figures.R",      format = "file"),
  tar_target(script_04,     "04_model_matching.R",        format = "file"),

  # --- Stage 01: build the panel ---------------------------------------------
  # Depends on the raw inputs, 00_setup.R, and its own script. Returns the path
  # to the panel RDS (format="file"), so 02/02a/03 depend on the FILE.
  tar_target(
    panel_combined,
    run_stage("01_build_panel.R",
              inputs   = c(setup_file, raw_inputs, script_01),
              produces = "clean/panel_combined.rds"),
    format = "file"
  ),

  # --- Stage 02: rent/price equations, divergence, IV, robustness ------------
  tar_target(
    estimation,
    run_stage("02_estimate.R",
              inputs   = c(panel_combined, setup_file, script_02),
              produces = est_output_files()),
    format = "file"
  ),

  # --- Stage 02a: migration-disaggregation robustness ------------------------
  # Reads the same panel; independent of stage 02. Reruns only if the panel,
  # setup, or its own script changes.
  tar_target(
    migration_granularity,
    run_stage("02a_migration_granularity.R",
              inputs   = c(panel_combined, setup_file, script_02a),
              produces = granular_output_files()),
    format = "file"
  ),

  # --- Stage 02c: dynamic rent equation, local projections, channel test -----
  tar_target(
    sa_additions,
    run_stage("02c_sa_additions.R",
              inputs   = c(panel_combined, setup_file, script_02c),
              produces = file.path("output", c("sa_C_lp.csv", "sa_B_omega.csv",
                                               "fig_lp_irf_adjusted.png", "fig_lp_postimpact.png"))),
    format = "file"
  ),

  # --- Stage 02d: appendix tables on seasonally adjusted credit ---------------
  tar_target(
    sa_tables,
    run_stage("02d_sa_tables.R",
              inputs   = c(panel_combined, setup_file, script_02d),
              produces = file.path("output", c("sa2_S2_breaks.txt", "sa2_S3_granger.txt"))),
    format = "file"
  ),

  # --- Stage 03f: APRA instruments and the correlation-restriction test -------
  tar_target(
    apra_iv,
    run_stage("03f_missing_figures.R",
              inputs   = c(panel_combined, setup_file, script_03f),
              produces = file.path("output", c("lpiv_apra.csv", "fig_lpiv_main.png",
                                               "fig_cr_valid_range.png"))),
    format = "file"
  ),

  # --- Stage 04: model estimation by impulse-response matching ----------------
  # The block bootstrap (05_bootstrap.R, 05b_bootstrap_summary.R) takes several
  # hours and is run by `Rscript run_all.R --bootstrap`, not by this graph.
  tar_target(
    model,
    run_stage("04_model_matching.R",
              inputs   = c(sa_additions, setup_file, script_04),
              produces = file.path("output", c("model_estimates.csv", "model_decomposition.csv",
                                               "model_counterfactual.csv", "fig_model_fit.png",
                                               "fig_model_counterfactual.png"))),
    format = "file"
  ),
  tar_target(script_04b, "04b_identification.R", format = "file"),
  tar_target(
    model_ident,
    run_stage("04b_identification.R",
              inputs   = c(model, script_04b),
              produces = file.path("output", c("model_profile.csv", "model_sensitivity.csv",
                                               "fig_model_profile.png"))),
    format = "file"
  ),

  # --- Stage 03: figures + local projections ---------------------------------
  # Figure 1 (transmission diagram) is native TikZ in the .tex, not produced here.
  tar_target(
    figures,
    run_stage("03_figures.R",
              inputs   = c(panel_combined, setup_file, script_03),
              produces = figure_output_files()),
    format = "file"
  )
)
