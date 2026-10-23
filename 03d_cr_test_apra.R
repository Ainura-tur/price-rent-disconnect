# =============================================================================
# 03d_cr_test_apra.R  --  correlation-restriction test of the APRA instruments
#
# Tests whether the APRA instruments of 03c_lp_iv_apra.R can be valid, with the
# correlation-restriction (CR) test of Dzhumashev and Tursunalieva, "A test for
# instrumental variable validity using a partially identified correlation
# restriction", Econometric Reviews (2026), doi 10.1080/07474938.2026.2741548.
#
# The idea. An instrument z is valid only if it is uncorrelated with the error
# u of the outcome equation. That correlation is not identified, but it is a
# known function of three correlations we observe (credit with the outcome,
# credit with z, the outcome with z) and of one we do not: credit's own
# correlation with u. Restricting that correlation to a domain D gives an
# identified set for corr(z, u). The test rejects validity when zero lies
# outside the MCUB confidence interval for the set (Bei 2024).
#
# Here, for each local projection of 03c and each horizon h = 4, 8, 12:
#   y  the cumulative change in log price or rent from t-1 to t+h
#   x  the cumulative change in investor credit over the same quarters
#   z  the APRA instrument (national timing, or 2014 exposure times timing)
# all residualised on the fixed effects and controls that 03c uses, so the
# test applies to exactly the IV estimates in lpiv_results.csv. Where the
# residualised z lowers credit, it is multiplied by -1, as iv_cr_test() does
# (Lemma 2.3); zero inclusion does not change.
#
# Domain: D = [0, 0.8]. Credit is taken to be positively correlated with the
# unobserved shocks to prices and rents: expected gains raise investor
# borrowing and prices together. 0.8 is the bound used in the paper.
#
# The test routines come unchanged from the ivcrtest folder of the CR
# replication package (scripts/lib/ivcrtest). This script follows
# check_compatibility() step by step, with two changes, both because the
# local projections overlap in time:
#   1. The covariance of the three correlations comes from a moving block
#      bootstrap over quarters: blocks of BLOCK_LEN consecutive quarters, with
#      all five cities in a quarter kept together. The package resamples
#      single observations. With cumulative changes over h quarters, that
#      understates the sampling error: the block standard errors of the
#      correlations are 1.4 to 4.3 times larger.
#   2. The Imbens-Manski/Stoye endpoint bootstrap uses the same block draws.
#      As in the package, the first B_SIGMA of the B_IM draws also give the
#      covariance.
# Everything else is the package's: the MCUB interval (CIhybrid), its tuning
# (alpha 0.05, alpha_c 0.8 alpha, eta 0.001, B 500, Blarge 5000, K 80), the
# inverted p-value p_adm (pvalue_mcub_zero), the naive endpoint interval, the
# switching points of Section 3.5 (cr_switch_point), the pointwise
# compatibility range (cr_pointwise_range) and the diagnostics (sign
# condition, endpoint gap, discretisation, correlation screen).
#
# Specifications (96 in all):
#   headline   the two designs in the 03c figures (national / any and
#              exposure / io_cap), both credit measures, price and rent,
#              h = 4, 8, 12, full sample: MCUB interval, p_adm and diagnostics;
#              for the main credit measure also the switching points and the
#              interval under other resampling schemes
#   others     the other four instruments, full sample: MCUB interval
#   samples    the headline designs, main measure, without the 2015-16
#              reclassification quarters and without Sydney: MCUB interval
#
# Reads clean/panel_tests (.rds or .csv), written by 01a_build_test_panel.R,
# and, if present, output/lpiv_results.csv from 03c (for the first-stage F).
# Needs the ivcrtest folder: copy scripts/lib/ivcrtest from the CR replication
# package to lib/ivcrtest in this repository, or point IVCR_DIR to it.
# Run from the folder that holds clean/ (the repository root):
#   Rscript 03d_cr_test_apra.R
# It does not need 00_setup.R.
#
# Run time. Each MCUB interval takes 10-20 seconds; p_adm and the switching
# points need many of them. A headline specification with switching points
# takes about 12 minutes, one with p_adm only about 4, the others under 20
# seconds: about three and a half hours on one core, about half an hour on
# eight. The script uses all cores but one, and it saves each finished
# specification in output/cr_cache/, so an interrupted run resumes where it
# stopped. Delete that folder to start over. RUN_PVALUE, RUN_SWITCH and
# RUN_SENS switch off the expensive parts.
#
# Writes to output/:
#   cr_results.csv     every specification, all intervals and diagnostics
#   cr_summary.txt     the tables
# and to run_logs/, beside the run log:
#   03d_cr_progress.log  one line per finished specification, across runs
# =============================================================================

# Log this run in run_logs/ (see 00_run_log.R); the last line closes the log.
source("00_run_log.R")
start_run_log("03d_cr_test_apra")

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

for (p in c("MASS", "Matrix", "parallel"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Please install the ", p, " package.")

# ---- Settings ----------------------------------------------------------------
CLEAN    <- "clean"
OUTDIR   <- "output"
IVCR_DIR <- "lib/ivcrtest"   # the ivcrtest folder of the CR replication package

H_TEST  <- c(4, 8, 12)
NLAG    <- 4                 # control lags, as in 03b and 03c
D_RXU   <- c(0, 0.8)         # admissible domain for corr(credit, u)

# Test tuning: the package defaults used in the paper. With K = 80 on D, the
# tol_r screen drops 4 to 20 percent of grid pairs here (the package warns
# above 5 percent). It does not matter: in the 12 October run, the 14
# decisive intervals (zero outside, or p_adm below 0.2) keep their verdicts
# with tol_r = 1e-4 and 1e-5, and their ends move by at most 0.003.
ALPHA   <- 0.05
K_GRID  <- 80L
B_MCUB  <- 500L
BLARGE  <- 5000L
ETA     <- 0.001
TOL     <- 1e-3
TOL_R   <- 1e-3
MCUB_SEED <- 123L            # iv_cr_test() default

# Bootstrap: draw counts and seed as in check_compatibility() for the first
# instrument (B_boot 800, B_im 999, seed 1000 + 1); blocks as described above.
B_SIGMA   <- 800L
B_IM      <- 999L
BOOT_SEED <- 1001L
BLOCK_LEN <- 8L              # quarters; standard errors level off between 6 and 13
SENS_BLOCKS <- c(4L, 13L)    # other block lengths for the sensitivity table

RUN_PVALUE <- TRUE           # p_adm for the headline specifications
RUN_SWITCH <- TRUE           # switching points, headline designs, main measure
RUN_SENS   <- TRUE           # MCUB interval under other resampling schemes
N_CORES    <- max(1L, parallel::detectCores() - 1L, na.rm = TRUE)

MEASURES <- c(main = "dl_inv_sa", share = "d_inv_share_sa")
LEVELS   <- c(price = "prop_price", rent = "rents")
GROWTH   <- c(price = "dl_price",   rent = "dl_rent")
INSTRUMENTS <- list(national = c(any = "apra_any", benchmark = "apra_inv_on", io_cap = "apra_io_on"),
                    exposure = c(any = "z_any",    benchmark = "z_inv",       io_cap = "z_io"))
EXPOSURE_YEAR <- 2014
SAMPLES <- list(full       = function(d) d,
                no_reclass = function(d) filter(d, flag_reclass == 0),
                no_sydney  = function(d) filter(d, city != "Sydney"))
HEADLINE <- c("national / any", "exposure / io_cap")   # the designs in the 03c figures

# Absolute paths, so the parallel workers write to the same place.
dir.create(file.path(OUTDIR, "cr_cache"), showWarnings = FALSE, recursive = TRUE)
dir.create(RUN_LOG_DIR, showWarnings = FALSE)
CACHE_DIR <- normalizePath(file.path(OUTDIR, "cr_cache"))
LOG_FILE  <- file.path(normalizePath(RUN_LOG_DIR), "03d_cr_progress.log")

# ---- CR routines: the ivcrtest package ------------------------------------------
IVCR_FILES <- normalizePath(sort(list.files(IVCR_DIR, pattern = "\\.R$", full.names = TRUE)))
if (!length(IVCR_FILES))
  stop("No R files in ", IVCR_DIR, ". Copy scripts/lib/ivcrtest from the CR replication ",
       "package there, or set IVCR_DIR to that folder.")
for (f in IVCR_FILES) source(f)
need_fns <- c("CIhybrid", "pvalue_mcub_zero", "ci_naive_endpoint", "solve_c_im",
              "cr_switch_point", "cr_pointwise_range", "estimate_cov_corr_boot",
              "cr_Amat", "cr_delta", "cr_omega_delta", "cr_grid_map",
              "cr_sign_condition", "cr_endpoint_gap", "cr_discretization", "cr_corr_screen")
miss <- need_fns[!vapply(need_fns, exists, logical(1))]
if (length(miss))
  stop("The ivcrtest folder lacks: ", paste(miss, collapse = ", "), ". Use the version in the ",
       "CR replication package (it has check_compatibility.R, cr_valid_range.R, reparam_delta.R ...).")

# ---- Data: as in 03c ---------------------------------------------------------------
read_panel <- function(name) {
  f_rds <- file.path(CLEAN, paste0(name, ".rds"))
  f_csv <- file.path(CLEAN, paste0(name, ".csv"))
  if (file.exists(f_rds)) return(readRDS(f_rds))
  if (file.exists(f_csv))
    return(read_csv(f_csv, show_col_types = FALSE) |> mutate(quarter = as.yearqtr(quarter)))
  stop("Neither ", f_rds, " nor ", f_csv, " was found. Run 01a_build_test_panel.R first.")
}
tp <- read_panel("panel_tests") |> arrange(city, quarter)

need <- c(LEVELS, GROWTH, MEASURES, "apra_inv_on", "apra_io_on", "inv_share_sa",
          "tight_l1", "d_cash_l1", "city", "qid", "year", "flag_reclass",
          paste0(rep(c(MEASURES, GROWTH), each = NLAG), "_l", seq_len(NLAG)))
missing_cols <- setdiff(need, names(tp))
if (length(missing_cols))
  stop("panel_tests lacks: ", paste(missing_cols, collapse = ", "), ". Rerun 01a_build_test_panel.R.")

# Exposure: each city's average seasonally adjusted investor share in 2014,
# centred on the five-city mean, times each regime indicator.
expo <- tp |>
  filter(year == EXPOSURE_YEAR) |>
  group_by(city) |>
  summarise(exposure = mean(inv_share_sa), .groups = "drop")
tp <- tp |>
  left_join(expo |> mutate(exposure = exposure - mean(exposure)), by = "city") |>
  mutate(apra_any = as.integer(apra_inv_on == 1 | apra_io_on == 1),
         z_any = exposure * apra_any,
         z_inv = exposure * apra_inv_on,
         z_io  = exposure * apra_io_on)

# Cumulative outcome changes y(t+h) - y(t-1) and cumulative credit changes
# (the sum of the measure from t to t+h), all within city.
for (o in names(LEVELS)) for (h in H_TEST) {
  tp <- tp |>
    group_by(city) |>
    mutate("cum_{o}_{h}" := dplyr::lead(log(.data[[LEVELS[[o]]]]), h) -
                            dplyr::lag(log(.data[[LEVELS[[o]]]]), 1)) |>
    ungroup()
}
for (m in names(MEASURES)) {
  x <- MEASURES[[m]]
  tp <- tp |> group_by(city) |> mutate("cx_{m}_0" := .data[[x]]) |> ungroup()
  for (h in seq_len(max(H_TEST))) {
    tp <- tp |>
      group_by(city) |>
      mutate("cx_{m}_{h}" := .data[[paste0("cx_", m, "_", h - 1)]] + dplyr::lead(.data[[x]], h)) |>
      ungroup()
  }
}

# Remove city effects (or city and quarter effects) from every column.
resid_fe <- function(M, city, time, two_way) {
  if (!two_way) return(M - apply(M, 2, function(v) ave(v, city)))
  qr.resid(qr(model.matrix(~ factor(city) + factor(time))), M)
}

# The residualised (x, y, z) of one 03c specification: fixed effects removed,
# then the controls partialled out (Frisch-Waugh), as iv_cr_test() does with H.
cr_data <- function(d, design, inst, measure, outcome, h) {
  x_now <- MEASURES[[measure]]
  endo  <- paste0("cx_", measure, "_", h)
  y     <- paste0("cum_", outcome, "_", h)
  ctrl  <- c(paste0(x_now, "_l", seq_len(NLAG)),
             paste0(GROWTH[[outcome]], "_l", seq_len(NLAG)), "tight_l1")
  if (design == "national") ctrl <- c(ctrl, "d_cash_l1")
  cols <- unique(c(y, endo, ctrl, inst))
  d <- d[stats::complete.cases(d[, cols]), ]
  R <- resid_fe(as.matrix(d[, cols]), d$city, d$qid, two_way = design == "exposure")
  W <- qr(R[, ctrl, drop = FALSE])
  out <- data.frame(qid = d$qid,
                    x = qr.resid(W, R[, endo]),
                    y = qr.resid(W, R[, y]),
                    z = qr.resid(W, R[, inst]))
  flipped <- stats::cor(out$x, out$z) < 0
  if (flipped) out$z <- -out$z
  attr(out, "flipped") <- flipped
  out
}

# ---- Bootstrap and test helpers ------------------------------------------------------
# Moving block bootstrap over quarters: each draw strings together blocks of
# len consecutive quarters (start points uniform) until it has as many
# quarters as the sample, and returns the rows of those quarters, so all
# cities in a quarter move together. len = 1 resamples single quarters.
block_draws <- function(qid, len, B, seed) {
  q    <- sort(unique(qid))
  nq   <- length(q)
  rows <- split(seq_along(qid), factor(match(qid, q), levels = seq_len(nq)))
  nb   <- ceiling(nq / len)
  set.seed(seed)
  lapply(seq_len(B), function(b) {
    start <- sample.int(nq - len + 1L, nb, replace = TRUE)
    qs    <- as.vector(outer(0:(len - 1L), start, "+"))[seq_len(nq)]
    unlist(rows[qs], use.names = FALSE)
  })
}

boot_rho <- function(df, i) c(stats::cor(df$x[i], df$y[i]), stats::cor(df$x[i], df$z[i]),
                              stats::cor(df$y[i], df$z[i]))

# Covariance of (rho_xy, rho_xz, rho_yz) from given draws, finished as
# estimate_cov_corr_boot() finishes it.
sigma_from_draws <- function(df, idx) {
  Rb <- t(vapply(idx, function(i) boot_rho(df, i), numeric(3)))
  S  <- stats::cov(Rb, use = "complete.obs")
  S  <- (S + t(S)) / 2
  as.matrix(Matrix::nearPD(S, corr = FALSE)$mat)
}

# MCUB interval for one covariance, with the tuning of check_compatibility().
mcub_ci <- function(df, Sigma_rho, rxu_range = D_RXU) {
  rho_hat <- c(stats::cor(df$x, df$y), stats::cor(df$x, df$z), stats::cor(df$y, df$z))
  A <- cr_Amat(seq(rxu_range[1], rxu_range[2], length.out = K_GRID))
  CIhybrid(cr_delta(rho_hat), cr_omega_delta(rho_hat, Sigma_rho), A, A,
           alpha = ALPHA, alphac = 0.8 * ALPHA, eta = ETA, B = B_MCUB, Blarge = BLARGE,
           tol = TOL, tol_r = TOL_R, index = NULL, g = cr_grid_map(A), seed = MCUB_SEED)$CI_h
}

zero_in <- function(ci) if (any(!is.finite(ci))) NA else (ci[1] <= 0 && 0 <= ci[2])

# One CR check. Follows check_compatibility(route = "reparam") step by step;
# only the resampling differs (idx_sigma for the covariance, idx_im for the
# IM/Stoye endpoints).
cr_check <- function(df, idx_sigma, idx_im, do_pvalue, do_switch) {
  rho_hat   <- c(rho_xy = stats::cor(df$x, df$y), rho_xz = stats::cor(df$x, df$z),
                 rho_yz = stats::cor(df$y, df$z))
  Sigma_rho <- sigma_from_draws(df, idx_sigma)
  r_grid <- seq(D_RXU[1], D_RXU[2], length.out = K_GRID)
  Amat   <- cr_Amat(r_grid)
  Omega  <- cr_omega_delta(rho_hat, Sigma_rho)
  dhat   <- cr_delta(rho_hat)
  g_map  <- cr_grid_map(Amat)
  lambda_hat <- as.numeric(Amat %*% dhat)
  plug_in    <- c(min(lambda_hat), max(lambda_hat))

  sign_diag   <- cr_sign_condition(rho_hat, D_RXU)
  gap_diag    <- cr_endpoint_gap(rho_hat, Sigma_rho, D_RXU, K = K_GRID)
  disc_diag   <- cr_discretization(rho_hat, D_RXU, K = K_GRID)
  screen_diag <- cr_corr_screen(rho_hat, Sigma_rho, D_RXU, K = K_GRID, tol_r = TOL_R)

  naive <- ci_naive_endpoint(df$x, df$y, df$z, rxu_range = D_RXU, K = K_GRID,
                             alpha = ALPHA, Sigma_rho = Sigma_rho)$CI

  mcub <- CIhybrid(dhat, Omega, Amat, Amat, alpha = ALPHA, alphac = 0.8 * ALPHA, eta = ETA,
                   B = B_MCUB, Blarge = BLARGE, tol = TOL, tol_r = TOL_R, index = NULL,
                   g = g_map, seed = MCUB_SEED)$CI_h

  # As in check_compatibility(), a failure in a later step is noted and the
  # interval kept.
  notes <- character(0)
  note  <- function(step) function(e) { notes <<- c(notes, paste0(step, ": ", conditionMessage(e))); NULL }

  p <- list(p_adm = NA_real_, label = NA_character_)
  if (do_pvalue)
    p <- tryCatch(pvalue_mcub_zero(dhat, Omega, Amat, Amat, g_map, eta = ETA,
                                   B = max(1L, B_MCUB %/% 2L), Blarge = max(1L, BLARGE %/% 2L),
                                   tol = TOL, tol_r = TOL_R, seed = MCUB_SEED),
                  error = note("pvalue")) %||% p

  # IM/Stoye, as im_stoye_inference(se_method = "bootstrap"), on the block
  # draws. The same draws give how often the grid extrema sit at the corners
  # of D (cr_curvature_check.R).
  ends <- t(vapply(idx_im, function(i) {
    l <- as.numeric(Amat %*% cr_delta(boot_rho(df, i)))
    c(min(l, na.rm = TRUE), max(l, na.rm = TRUE),
      which.min(l) %in% c(1L, K_GRID) && which.max(l) %in% c(1L, K_GRID))
  }, numeric(3)))
  se_l <- stats::sd(ends[, 1], na.rm = TRUE)
  se_u <- stats::sd(ends[, 2], na.rm = TRUE)
  se_max <- max(se_l, se_u)
  if (!is.finite(se_max) || se_max <= 0) {
    CI_IM <- plug_in
  } else {
    c_im  <- solve_c_im(ALPHA, (plug_in[2] - plug_in[1]) / se_max)
    CI_IM <- c(plug_in[1] - c_im * se_l, plug_in[2] + c_im * se_u)
  }

  # Switching points of Section 3.5 and the pointwise compatibility range,
  # both on the same covariance.
  sw <- list(below = list(tau = NA_real_, status = NA_character_),
             above = list(tau = NA_real_, status = NA_character_))
  if (do_switch) for (dir in c("below", "above"))
    sw[[dir]] <- tryCatch(cr_switch_point(df, rho_bar = D_RXU[2], k = 1, direction = dir,
                                          alpha = ALPHA, K = K_GRID, Sigma_rho = Sigma_rho,
                                          B = B_MCUB, Blarge = BLARGE, eta = ETA, tol = TOL,
                                          tol_r = TOL_R),
                          error = note(paste("switch", dir))) %||% sw[[dir]]
  pw <- tryCatch(cr_pointwise_range(df, rxu_range = D_RXU, k = 1, alpha = ALPHA, Sigma_rho = Sigma_rho),
                 error = note("pointwise")) %||% list(range = c(NA_real_, NA_real_), contiguous = NA)

  ev <- eigen(Omega, symmetric = TRUE, only.values = TRUE)$values
  data.frame(
    n = nrow(df), quarters = length(unique(df$qid)),
    rho_xy = rho_hat[[1]], rho_xz = rho_hat[[2]], rho_yz = rho_hat[[3]],
    se_xy = sqrt(Sigma_rho[1, 1]), se_xz = sqrt(Sigma_rho[2, 2]), se_yz = sqrt(Sigma_rho[3, 3]),
    beta_iv = stats::cov(df$z, df$y) / stats::cov(df$z, df$x),
    plug_lo = plug_in[1], plug_hi = plug_in[2],
    mcub_lo = mcub[1], mcub_hi = mcub[2], zero_mcub = zero_in(mcub),
    p_adm = p$p_adm, p_adm_label = p$label,
    im_lo = CI_IM[1], im_hi = CI_IM[2], zero_im = zero_in(CI_IM),
    naive_lo = naive[1], naive_hi = naive[2], zero_naive = zero_in(naive),
    tau_below = sw$below$tau, below_status = sw$below$status,
    tau_above = sw$above$tau, above_status = sw$above$status,
    pw_lo = pw$range[1], pw_hi = pw$range[2], pw_contiguous = pw$contiguous,
    h_r = sign_diag$h_r, r_star = sign_diag$r_star, sign_condition = sign_diag$holds,
    r_star_in_D = sign_diag$r_star_in_D, mono_margin = sign_diag$monotonicity_margin,
    corner_freq = mean(ends[, 3], na.rm = TRUE),
    G_hat = gap_diag$G, hausdorff = disc_diag$hausdorff, L_delta_K = disc_diag$L_delta_K,
    share_screened = screen_diag$share_screened,
    eig_min_omega = min(ev), cond_omega = max(ev) / min(ev),
    notes = paste(notes, collapse = "; "),
    stringsAsFactors = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Everything for one specification; errors are recorded, not raised.
run_job <- function(job) {
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch({
    df  <- job$df
    idx <- block_draws(df$qid, BLOCK_LEN, B_IM, BOOT_SEED)
    out <- cr_check(df, idx[seq_len(B_SIGMA)], idx, job$do_pvalue, job$do_switch)
    # The package's own resampling (single observations), for comparison.
    S_iid <- estimate_cov_corr_boot(df$x, df$y, df$z, B = B_SIGMA, seed = BOOT_SEED)
    out$se_xy_iid <- sqrt(S_iid[1, 1]); out$se_xz_iid <- sqrt(S_iid[2, 2]); out$se_yz_iid <- sqrt(S_iid[3, 3])
    if (job$do_sens) {
      sens <- list(iid = S_iid)
      for (L in SENS_BLOCKS)
        sens[[paste0("block", L)]] <- sigma_from_draws(df, block_draws(df$qid, L, B_SIGMA, BOOT_SEED))
      for (nm in names(sens)) {
        ci <- tryCatch(mcub_ci(df, sens[[nm]]), error = function(e) c(NA_real_, NA_real_))
        out[[paste0("sens_", nm, "_lo")]]   <- ci[1]
        out[[paste0("sens_", nm, "_hi")]]   <- ci[2]
        out[[paste0("sens_", nm, "_zero")]] <- zero_in(ci)
      }
    }
    out$flipped <- isTRUE(attr(df, "flipped"))
    out$status  <- "ok"
    out
  }, error = function(e) data.frame(status = paste("error:", conditionMessage(e)),
                                    stringsAsFactors = FALSE))
  res$id <- job$id
  res$seconds <- round(proc.time()[["elapsed"]] - t0, 1)
  saveRDS(list(sig = job$sig, res = res), file.path(CACHE_DIR, paste0(job$id, ".rds")))
  cat(sprintf("%s  %-55s %7.1f s  %s\n", format(Sys.time(), "%H:%M:%S"), job$id, res$seconds,
              res$status), file = LOG_FILE, append = TRUE)
  res
}

# ---- Specifications ------------------------------------------------------------------
specs <- bind_rows(
  bind_rows(lapply(names(INSTRUMENTS), function(des)
    expand_grid(design = des, instrument = names(INSTRUMENTS[[des]]), measure = names(MEASURES),
                outcome = names(LEVELS), sample = "full", h = H_TEST))),
  expand_grid(spec = HEADLINE, measure = "main", outcome = names(LEVELS),
              sample = c("no_reclass", "no_sydney"), h = H_TEST) |>
    separate(spec, c("design", "instrument"), sep = " / ")
) |>
  mutate(headline   = sample == "full" & paste(design, "/", instrument) %in% HEADLINE,
         do_pvalue  = RUN_PVALUE & headline,
         do_switch  = RUN_SWITCH & headline & measure == "main",
         do_sens    = RUN_SENS & headline & measure == "main",
         id = paste(sample, design, instrument, measure, outcome, paste0("h", h), sep = "_"))

# Anything that changes a result invalidates its cached copy.
settings_sig <- list(D = D_RXU, alpha = ALPHA, K = K_GRID, B = B_MCUB, Blarge = BLARGE, eta = ETA,
                     tol = TOL, tol_r = TOL_R, mcub_seed = MCUB_SEED, b_sigma = B_SIGMA, b_im = B_IM,
                     boot_seed = BOOT_SEED, block = BLOCK_LEN, sens_blocks = SENS_BLOCKS,
                     nlag = NLAG, pkg = unname(tools::md5sum(IVCR_FILES)))

jobs <- lapply(seq_len(nrow(specs)), function(i) {
  s  <- specs[i, ]
  df <- cr_data(SAMPLES[[s$sample]](tp), s$design, INSTRUMENTS[[s$design]][[s$instrument]],
                s$measure, s$outcome, s$h)
  list(id = s$id, df = df, do_pvalue = s$do_pvalue, do_switch = s$do_switch, do_sens = s$do_sens,
       sig = c(settings_sig, list(data = signif(c(nrow(df), colSums(abs(df[, c("x", "y", "z")]))), 10),
                                  flags = c(s$do_pvalue, s$do_switch, s$do_sens))))
})
names(jobs) <- specs$id

cached <- lapply(jobs, function(j) {
  f <- file.path(CACHE_DIR, paste0(j$id, ".rds"))
  if (!file.exists(f)) return(NULL)
  obj <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(obj) || !identical(obj$sig, j$sig) || !identical(obj$res$status, "ok")) return(NULL)
  obj$res
})
todo <- jobs[vapply(cached, is.null, logical(1))]

# Rough cost in MCUB intervals: p_adm up to 20 at half the draws, switching
# points up to 36, the sensitivity 3.
fits <- function(j) 1 + 8 * j$do_pvalue + 36 * j$do_switch + 3 * j$do_sens
n_fits <- sum(vapply(todo, fits, numeric(1)))
n_use  <- max(1L, min(N_CORES, length(todo)))
message(sprintf("CR test: %d specifications, %d already done, %d to run", length(jobs),
                length(jobs) - length(todo), length(todo)),
        if (length(todo)) sprintf(" (about %d MCUB intervals, %.0f minutes at 16 seconds each on %d core%s).\nProgress: %s",
                                  round(n_fits), n_fits * 16 / 60 / n_use, n_use, ifelse(n_use > 1, "s", ""), LOG_FILE)
        else ".")

# ---- Run ------------------------------------------------------------------------------
if (length(todo)) {
  cat(sprintf("--- %s: %d specifications to run\n", format(Sys.time(), "%Y-%m-%d %H:%M"), length(todo)),
      file = LOG_FILE, append = TRUE)
  todo <- todo[order(-vapply(todo, fits, numeric(1)))]   # longest first
  n_workers <- min(N_CORES, length(todo))
  if (n_workers > 1) {
    cl <- parallel::makeCluster(n_workers)
    new_res <- tryCatch({
      parallel::clusterExport(cl, c("IVCR_FILES", "D_RXU", "ALPHA", "K_GRID", "B_MCUB", "BLARGE",
                                    "ETA", "TOL", "TOL_R", "MCUB_SEED", "B_SIGMA", "B_IM", "BOOT_SEED",
                                    "BLOCK_LEN", "SENS_BLOCKS", "CACHE_DIR", "LOG_FILE",
                                    "block_draws", "boot_rho", "sigma_from_draws", "mcub_ci",
                                    "zero_in", "cr_check", "run_job", "%||%"), envir = globalenv())
      parallel::clusterEvalQ(cl, { for (f in IVCR_FILES) source(f); NULL })
      parallel::parLapplyLB(cl, todo, run_job, chunk.size = 1)
    }, finally = parallel::stopCluster(cl))
  } else {
    new_res <- lapply(todo, function(j) { message("  ", j$id); run_job(j) })
  }
  names(new_res) <- names(todo)
  cached[names(new_res)] <- new_res
}

res <- bind_rows(cached) |>
  left_join(select(specs, id, design, instrument, measure, outcome, sample, h, headline), by = "id") |>
  relocate(id, design, instrument, measure, outcome, sample, h, headline)
bad <- res |> filter(status != "ok")
if (nrow(bad)) {
  message("Failed specifications:")
  print(as.data.frame(select(bad, id, status)), row.names = FALSE)
}

# First-stage F and Anderson-Rubin p-value from 03c, when its results are here.
f_lpiv <- file.path(OUTDIR, "lpiv_results.csv")
if (file.exists(f_lpiv)) {
  lpiv <- read_csv(f_lpiv, show_col_types = FALSE) |>
    select(design, instrument, measure, outcome, sample, h, fs_F, ar_p, iv_b_03c = iv_b)
  res <- left_join(res, lpiv, by = c("design", "instrument", "measure", "outcome", "sample", "h"))
} else {
  res$fs_F <- NA_real_; res$ar_p <- NA_real_; res$iv_b_03c <- NA_real_
}
write_csv(res, file.path(OUTDIR, "cr_results.csv"))

# ---- Summary -------------------------------------------------------------------------------
ci_txt  <- function(lo, hi) ifelse(is.finite(lo) & is.finite(hi), sprintf("[%6.3f, %6.3f]", lo, hi), "")
pw_txt  <- function(lo, hi) ifelse(is.finite(lo) & is.finite(hi), sprintf("[%5.3f, %5.3f]", lo, hi), "none")
mark    <- function(v) ifelse(is.na(v), "", ifelse(v, "yes", "no"))
f3      <- function(v) ifelse(is.finite(v), sprintf("%.3f", v), "")
f1      <- function(v) ifelse(is.finite(v), sprintf("%.1f", v), "")
# Valid range from the switching points: domains (0, r) pass once r reaches
# the lower end; domains (r, 0.8) fail once r passes the upper end. The
# package scans r from 0.005 to 0.75; a switch beyond that prints as 0 or 0.8
# (or <0.005 and >0.75 when it lies on the failing side).
range_txt <- function(ta, sa, tb, sb, zero) {
  lo <- ifelse(is.na(sa), NA, ifelse(sa == "interior", sprintf("%.3f", abs(ta)),
               ifelse(sa == "accepts throughout", "0", ">0.75")))
  hi <- ifelse(is.na(sb), NA, ifelse(sb == "interior", sprintf("%.3f", abs(tb)),
               ifelse(sb == "accepts throughout", sprintf("%.1f", D_RXU[2]), "<0.005")))
  ifelse(is.na(lo) | is.na(hi), "",
         ifelse(!zero & sa == "rejects throughout" & sb == "rejects throughout", "none",
                paste0("[", lo, ", ", hi, "]")))
}
ok <- res |> filter(status == "ok") |> arrange(match(paste(design, instrument), c("national any", "exposure io_cap")),
                                               design, instrument, measure, outcome, h)

# The sink starts inside tryCatch, so source(echo = TRUE) does not copy the
# code into the summary.
con <- file(file.path(OUTDIR, "cr_summary.txt"), open = "wt")
old_width <- options(width = 250)
tryCatch({
  sink(con, split = TRUE)
  cat("Correlation-restriction test of the APRA instruments,", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("Each row tests one local projection of 03c: is corr(z, u) = 0 compatible with the data when\n",
      "corr(credit, u) lies in D = [", D_RXU[1], ", ", D_RXU[2], "]? 'zero' = yes when zero lies in the MCUB ",
      round(100 * (1 - ALPHA)), " percent interval\n",
      "for corr(z, u), so validity is not rejected. y, x and z are residualised on the fixed effects and\n",
      "controls of 03c; z is multiplied by -1 where it lowers credit. Covariance of the correlations:\n",
      "moving block bootstrap over quarters, blocks of ", BLOCK_LEN, " quarters, all cities in a quarter together.\n",
      "rho_xz = corr(credit, z); beta_IV = the IV estimate of 03c; F = first-stage F from 03c.\n",
      "p_adm = smallest level in (0.01, 0.50) at which zero leaves the interval ('>0.50': zero stays in).\n",
      "valid range [a, b] (switching points, main measure): the instrument passes when corr(credit, u)\n",
      "may lie in [a, b]; domains (0, r) pass once r >= a, domains (r, ", D_RXU[2], ") fail once r > b;\n",
      "'none': no domain passes. pointwise = values t of corr(credit, u) at which zero lies in the\n",
      "95 percent normal interval for g(t), a fast approximation to the valid range.\n", sep = "")

  cat("\n1. Headline designs, full sample\n")
  t1 <- ok |>
    filter(headline) |>
    transmute(design, instrument, measure, outcome, h, F = f1(fs_F), rho_xz = f3(rho_xz),
              beta_IV = f3(beta_iv), plug_in = ci_txt(plug_lo, plug_hi), MCUB = ci_txt(mcub_lo, mcub_hi),
              zero = mark(zero_mcub), p_adm = ifelse(is.na(p_adm_label), "", p_adm_label),
              valid_range = range_txt(tau_above, above_status, tau_below, below_status, zero_mcub),
              pointwise = pw_txt(pw_lo, pw_hi))
  print(as.data.frame(t1), row.names = FALSE)

  cat("\n2. Other instruments, full sample\n")
  t2 <- ok |>
    filter(sample == "full", !headline) |>
    transmute(design, instrument, measure, outcome, h, F = f1(fs_F), rho_xz = f3(rho_xz),
              beta_IV = f3(beta_iv), plug_in = ci_txt(plug_lo, plug_hi), MCUB = ci_txt(mcub_lo, mcub_hi),
              zero = mark(zero_mcub), pointwise = pw_txt(pw_lo, pw_hi))
  print(as.data.frame(t2), row.names = FALSE)

  cat("\n3. Headline designs, main measure, other samples\n")
  t3 <- ok |>
    filter(sample != "full") |>
    transmute(sample, design, instrument, outcome, h, F = f1(fs_F), rho_xz = f3(rho_xz),
              beta_IV = f3(beta_iv), plug_in = ci_txt(plug_lo, plug_hi), MCUB = ci_txt(mcub_lo, mcub_hi),
              zero = mark(zero_mcub), pointwise = pw_txt(pw_lo, pw_hi)) |>
    arrange(sample, design, outcome, h)
  print(as.data.frame(t3), row.names = FALSE)

  if (RUN_SENS && "sens_iid_lo" %in% names(ok)) {
    cat("\n4. MCUB interval under other resampling schemes (headline designs, main measure)\n",
        "   iid = single observations, as in the package; blockL = blocks of L quarters\n", sep = "")
    sc <- function(lo, hi, z) paste(ci_txt(lo, hi), mark(z))
    t4 <- ok |>
      filter(headline, measure == "main") |>
      transmute(design, instrument, outcome, h,
                iid = sc(sens_iid_lo, sens_iid_hi, sens_iid_zero),
                !!paste0("block", SENS_BLOCKS[1]) := sc(.data[[paste0("sens_block", SENS_BLOCKS[1], "_lo")]],
                                                       .data[[paste0("sens_block", SENS_BLOCKS[1], "_hi")]],
                                                       .data[[paste0("sens_block", SENS_BLOCKS[1], "_zero")]]),
                !!paste0("block", BLOCK_LEN, " (main)") := sc(mcub_lo, mcub_hi, zero_mcub),
                !!paste0("block", SENS_BLOCKS[2]) := sc(.data[[paste0("sens_block", SENS_BLOCKS[2], "_lo")]],
                                                       .data[[paste0("sens_block", SENS_BLOCKS[2], "_hi")]],
                                                       .data[[paste0("sens_block", SENS_BLOCKS[2], "_zero")]]))
    print(as.data.frame(t4), row.names = FALSE)
  }

  cat("\n5. Diagnostics (headline designs, full sample)\n",
      "   se ratio = block over single-observation bootstrap standard error of rho_xy, rho_xz, rho_yz.\n",
      "   sign = Lemma 2.3 condition (h_r > 0 on a positive D): then g rises over D and the set's\n",
      "   ends sit at the corners of D. r* = turning point of g (outside D: g is monotone on D).\n",
      "   corners = share of bootstrap draws with both grid extrema at the corners of D.\n",
      "   G = endpoint gap (Section 3.4); hausdorff = grid error of the set; screened = share of grid\n",
      "   pairs that tol_r drops from the truncated-normal bounds (the package warns above 0.05).\n", sep = "")
  t5 <- ok |>
    filter(headline) |>
    transmute(design, instrument, measure, outcome, h,
              se_ratio = sprintf("%.1f %.1f %.1f", se_xy / se_xy_iid, se_xz / se_xz_iid, se_yz / se_yz_iid),
              h_r = f3(h_r), sign = mark(sign_condition), r_star = f3(r_star), r_in_D = mark(r_star_in_D),
              margin = f3(mono_margin), corners = sprintf("%.2f", corner_freq), G = sprintf("%.2f", G_hat),
              hausdorff = sprintf("%.1e", hausdorff), screened = sprintf("%.3f", share_screened),
              cond_Omega = sprintf("%.1f", cond_omega))
  print(as.data.frame(t5), row.names = FALSE)

  cat("\n6. IM/Stoye and naive endpoint intervals (headline designs, full sample)\n")
  t6 <- ok |>
    filter(headline) |>
    transmute(design, instrument, measure, outcome, h, MCUB = paste(ci_txt(mcub_lo, mcub_hi), mark(zero_mcub)),
              IM_Stoye = paste(ci_txt(im_lo, im_hi), mark(zero_im)),
              naive = paste(ci_txt(naive_lo, naive_hi), mark(zero_naive)))
  print(as.data.frame(t6), row.names = FALSE)

  noted <- ok |> filter(!is.na(notes), notes != "")
  if (nrow(noted)) {
    cat("\nSteps that failed (the interval is kept):\n")
    print(as.data.frame(select(noted, id, notes)), row.names = FALSE)
  }
  if (any(is.finite(res$iv_b_03c)))
    cat(sprintf("\nCheck: beta_IV here and the IV estimate in lpiv_results.csv differ by at most %.1e.\n",
                max(abs(res$beta_iv - res$iv_b_03c), na.rm = TRUE)))
  cat(sprintf("Run time of the specifications run now or cached: %.0f minutes of computing.\n",
              sum(res$seconds, na.rm = TRUE) / 60))
}, finally = {
  sink()
  close(con)
  options(old_width)
})

end_run_log()
