# =============================================================================
# 04d_purchase_volumes.R -- investor purchase volumes (Section 6.3, Table 6)
#
# Measured investor credit is iota = phi + p + X (Equation 12). The baseline in
# 04_model_matching.R holds purchases X fixed, so every movement in commitments
# that prices do not explain counts as a change in lending conditions phi. Here
# purchases take a share s of the non-price credit response on impact, either
# in proportion to lending conditions at every horizon or with their own decay
# rate rho_X. Each scenario re-estimates all seven parameters with the targets,
# weights, bounds and search of 04, then the other six with theta = 1. The
# scenarios are assumptions, not estimates of purchase volumes. The data bear
# on s through investor loan numbers, which the ABS reports by state only from
# 2019:Q3; the section on loan numbers regresses their growth on growth in the
# non-price part of commitments.
#
# Adapted from Ratbek Dzhumashev's purchase_volume_sensitivity.R (November
# 2026). It reads the pipeline's own output/ rather than frozen copies, starts
# afresh on every run, and writes Table 6. The model definitions come from
# 04_model_matching.R, so they cannot drift from the baseline, and
# R/fast_solver.R must reproduce 04's solver before it replaces it.
#
# Reads:  output/sa_B_omega.csv, output/sa_C_lp.csv (02c),
#         output/model_estimates.csv (04), 04_model_matching.R, R/fast_solver.R,
#         clean/panel_combined.rds (01), and the ABS lending file F_LEND in raw/
# Writes: output/purchase_sensitivity.csv, output/purchase_paths.csv,
#         output/purchase_volumes.txt, output/table_purchase_volumes.tex
# =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({
  library(readr)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

set.seed(20261109)
H <- 12; TT <- 200; VARPI <- 0.99; DELTA_K <- 0.01
SIGMA <- 0.1006      # one standard deviation of SA investor-credit growth, as in 04
J0    <- 8           # construction lag, as in 04
OMEGA <- read_csv(file.path(OUTDIR, "sa_B_omega.csv"), show_col_types = FALSE)$omega

# ---- Targets and weights, as in 04 ---------------------------------------------
lp <- read_csv(file.path(OUTDIR, "sa_C_lp.csv"), show_col_types = FALSE)
tp <- lp |> filter(spec == "Price, baseline") |> arrange(h)
tr <- lp |> filter(spec == "Rent, baseline")  |> arrange(h)
tc <- lp |> filter(spec == "Credit, baseline", h >= 1) |> arrange(h)
m_data <- c(tp$beta, tr$beta, tc$beta); m_se <- c(tp$se, tr$se, tc$se)
W <- 1 / m_se^2
stopifnot(length(m_data) == 38)

# ---- Model definitions from 04 (definitions only; 04's estimation does not run) -
defs <- c("solve_path", "credit_path", "model_irf", "lo", "hi", "pn", "LOGP", "SU",
          "to_u", "to_z", "fit_ls", "fit_best", "rand_start", "basins")
for (ex in parse("04_model_matching.R")) {
  if (is.call(ex) && identical(ex[[1]], as.name("<-")) &&
      as.character(ex[[2]])[1] %in% defs) eval(ex)
}
stopifnot(all(vapply(defs, exists, logical(1), envir = environment(), inherits = FALSE)))

original_theta <- read_csv(file.path(OUTDIR, "model_estimates.csv"), show_col_types = FALSE)$estimate
stopifnot(length(original_theta) == 7)

# ---- Faster solver, checked against 04's ----------------------------------------
source(file.path("R", "fast_solver.R"), local = TRUE)
original_solver <- solve_path
gap <- vapply(1:10, function(i) {
  a <- list(phi = 0.96^(0:(TT - 1)), eps = 0.01, kappa = runif(1, 0.02, 1),
            eta = runif(1, 0, 1), chi = runif(1, 0, 0.05), J = J0, theta = runif(1, 0.02, 1))
  max(abs(unlist(do.call(original_solver, a)) - unlist(do.call(solve_path_fast, a))))
}, numeric(1))
stopifnot(max(gap) < 1e-9)
base <- model_irf(original_theta, J0)   # 04's mapping and solver, for the check below
solve_path <- solve_path_fast

# ---- Measured credit with a purchase response -----------------------------------
# share: purchases' share of the non-price credit response on impact.
scenario <- list(kind = "baseline", share = 0, rho = 0)
model_irf <- function(th, J, omega = OMEGA, ...) {
  cp  <- credit_path(th[2], th[3])
  sol <- solve_path(cp$phi, th[1], th[5], th[6], th[7], J, th[4], omega)
  amp <- scenario$share / (1 - scenario$share)
  x   <- if (scenario$kind == "proportional") amp * cp$phi else amp * scenario$rho^(0:(TT - 1))
  scale <- SIGMA / (cp$phi[1] + sol$p[1] + x[1])
  cr  <- (cp$phi + sol$p + x) * scale
  list(p = sol$p[1:(H + 1)] * scale, r = sol$r[1:(H + 1)] * scale, c = cr[2:(H + 1)],
       phi = cp$phi * scale, x = x * scale, p_long = sol$p * scale)
}
chk <- model_irf(original_theta, J0)
stopifnot(max(abs(unlist(base[c("p", "r", "c")]) - unlist(chk[c("p", "r", "c")]))) < 1e-12)
cat(sprintf("Fast solver matches 04 (largest gap %.1e); baseline distance %.4f\n",
            max(gap), sum(W * (c(chk$p, chk$r, chk$c) - m_data)^2)))

# ---- Scenarios ------------------------------------------------------------------
grid <- rbind(data.frame(kind = "baseline", share = 0, rho = 0),
              data.frame(kind = "proportional", share = c(0.25, 0.50, 0.75), rho = NA),
              expand.grid(kind = "dynamic", share = c(0.25, 0.50, 0.75),
                          rho = c(0.25, 0.75, 0.95), stringsAsFactors = FALSE))
results <- list(); paths <- list(); best <- original_theta
for (i in seq_len(nrow(grid))) {
  scenario <- as.list(grid[i, ])
  # The ten starting points of 04 plus the previous scenario's best fit.
  f <- fit_best(c(basins(original_theta), list(best)), J0)
  g <- fit_best(lapply(c(basins(f$par), list(original_theta)), replace, 4, 1), J0,
                free = setdiff(1:7, 4))
  stopifnot(is.finite(f$value), is.finite(g$value))
  best <- f$par
  # The unrestricted fit nests theta = 1; restart from there if the search missed it.
  if (g$value < f$value - 1e-5) f <- fit_best(list(f$par, g$par), J0)
  m <- model_irf(f$par, J0)
  results[[i]] <- cbind(grid[i, ], distance = f$value, theta1_distance = g$value,
                        as.data.frame(as.list(setNames(f$par, pn))))
  paths[[i]] <- data.frame(scenario = i, h = 0:H, p = m$p, r = m$r, credit = c(SIGMA, m$c),
                           phi = m$phi[1:(H + 1)], purchases = m$x[1:(H + 1)])
  cat(sprintf("%-12s s = %.2f, rho_X = %s: theta %.3f, distance %6.2f, with theta = 1 %6.2f\n",
              scenario$kind, scenario$share,
              if (scenario$kind == "dynamic") sprintf("%.2f", scenario$rho) else "  - ",
              f$par[4], f$value, g$value))
}
res <- do.call(rbind, results); rownames(res) <- NULL
write.csv(res, file.path(OUTDIR, "purchase_sensitivity.csv"), row.names = FALSE)
write.csv(do.call(rbind, paths), file.path(OUTDIR, "purchase_paths.csv"), row.names = FALSE)

# ---- Investor loan numbers ------------------------------------------------------
# Measured commitments are phi + p + X in logs, so commitment growth minus price
# growth is the non-price part, phi + X. Loan numbers measure purchases X. A
# slope near one, with a high R-squared, means purchases carry nearly all of the
# non-price movement in commitments, which points to s close to one. Same
# lending series as 04 (seasonally adjusted, investor, excluding refinancing).
lend <- read_csv(F_LEND, show_col_types = FALSE) |>
  filter(`Loan Purpose` == "Total dwellings excluding refinancing", `Housing Purpose` == "Investor",
         `Adjustment Type` == "Seasonally Adjusted", Measure %in% c("Number", "Value"),
         Region %in% names(STATE2CITY)) |>
  mutate(city = recode(Region, !!!STATE2CITY), quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)),
         v = as.numeric(OBS_VALUE)) |>
  select(city, quarter, Measure, v) |>
  distinct(city, quarter, Measure, .keep_all = TRUE) |>
  tidyr::pivot_wider(names_from = Measure, values_from = v)
ln <- readRDS(file.path(CLEAN, "panel_combined.rds")) |> select(city, quarter, prop_price) |>
  inner_join(lend, by = c("city", "quarter")) |>
  filter(quarter <= TARGET_END, !is.na(Number), !is.na(Value)) |>
  arrange(city, quarter) |> group_by(city) |>
  mutate(d_num = log(Number) - lag(log(Number)),
         d_nonprice = (log(Value) - lag(log(Value))) - (log(prop_price) - lag(log(prop_price)))) |>
  ungroup() |> filter(is.finite(d_num), is.finite(d_nonprice))
m_ln <- plm::plm(d_num ~ d_nonprice, data = plm::pdata.frame(ln, index = c("city", "quarter")),
                 model = "within")
ln_city <- ln |> group_by(city) |>
  summarise(slope = cov(d_num, d_nonprice) / var(d_nonprice), .groups = "drop")

# ---- Numbers quoted in Section 6.3 ----------------------------------------------
prop <- res[res$kind == "proportional", ]
sep  <- res[res$kind == "dynamic", ]
eps0 <- res$eps[res$kind == "baseline"]
near <- sep[sep$distance < 15, ]
far  <- sep[sep$distance >= 15, ]
summ <- c(
  sprintf("Baseline: theta %.3f, distance %.2f, with theta = 1 %.2f, epsilon %.4f",
          res$theta[1], res$distance[1], res$theta1_distance[1], eps0),
  sprintf("Proportional: theta %s; distance %s; epsilon %s (baseline epsilon / (1 - s): %s)",
          paste(sprintf("%.3f", prop$theta), collapse = ", "),
          paste(sprintf("%.2f", prop$distance), collapse = ", "),
          paste(sprintf("%.4f", prop$eps), collapse = ", "),
          paste(sprintf("%.4f", eps0 / (1 - prop$share)), collapse = ", ")),
  sprintf("Separate decay: theta from %.3f to %.3f", min(sep$theta), max(sep$theta)),
  sprintf("  with distance below 15 (%d scenarios): theta from %.3f to %.3f",
          nrow(near), min(near$theta), max(near$theta)),
  sprintf("  with distance of 15 or more (%d scenarios): distances %s",
          nrow(far), paste(sprintf("%.2f", far$distance), collapse = ", ")),
  sprintf("Smallest increase in distance with theta = 1, all scenarios: %.2f",
          min(res$theta1_distance - res$distance)),
  sprintf("Investor loan numbers, %s to %s (%d city-quarters): growth in loan numbers on growth in commitments minus price growth, city fixed effects",
          format(min(ln$quarter)), format(max(ln$quarter)), nrow(ln)),
  sprintf("  slope %.4f (Driscoll-Kraay s.e. %.4f), within R-squared %.3f",
          coef(m_ln)[["d_nonprice"]], dk_se(m_ln)[["d_nonprice"]], summary(m_ln)$r.squared[["rsq"]]),
  sprintf("  slope by city: %s", paste(sprintf("%s %.2f", ln_city$city, ln_city$slope), collapse = ", ")))
writeLines(summ, file.path(OUTDIR, "purchase_volumes.txt"))
cat("\n", paste(summ, collapse = "\n"), "\n", sep = "")

# ---- Table 6 --------------------------------------------------------------------
f2 <- function(x) formatC(x, format = "f", digits = 2)
f3 <- function(x) formatC(x, format = "f", digits = 3)
lab <- c(baseline = "Fixed purchases", proportional = "Proportional", dynamic = "Separate decay")
rows <- sprintf("%s & %s & %s & %s & %s & %s \\\\", lab[res$kind], f2(res$share),
                ifelse(res$kind == "dynamic", f2(res$rho), "---"), f3(res$theta),
                f2(res$distance), f2(res$theta1_distance))
tex <- c(
  "% Generated by 04d_purchase_volumes.R (output/table_purchase_volumes.tex).",
  "\\begin{table}[t]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Price adjustment with alternative investor purchase responses}",
  "\\label{tab:purchase_sensitivity}",
  "\\small",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lccrrr@{}}",
  "\\toprule",
  "Purchase response & $s$ & $\\rho_X$ & $\\hat\\theta$ & Distance & Distance with $\\theta=1$ \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular*}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  "\\item The baseline fixes investor purchases. $s$ is purchases' share of the",
  "non-price commitments response on impact, not their share of total credit.",
  "Proportional scenarios assign the same fraction at every horizon; separate-decay",
  "scenarios impose the displayed persistence. Each fit uses multiple",
  "starting values and the original weighted least-squares criterion. The last",
  "column re-estimates the remaining parameters under instantaneous adjustment.",
  "Distances are descriptive and are not $\\chi^2$ statistics. The baseline",
  "bootstrap interval is not a confidence interval for these scenarios.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}")
writeLines(tex, file.path(OUTDIR, "table_purchase_volumes.tex"))
cat("\nDone.\n")
