# =============================================================================
# 02f_new_lease_tests.R -- rents on new leases against the model
#
# Rents on new bonds measure the market rent r* of the model. The CPI rent
# series measures rents on all current leases, the measured rent r of
# Equation 5, r_t = (1 - omega) r_{t-1} + omega r*_t. For the four cities with
# bond data (Sydney, Melbourne, Brisbane, Adelaide; Perth publishes none) this
# script asks whether CPI rents follow new-lease rents as Equation 5 says, and
# how fast. The paper keeps omega = 0.35 from the dynamic rent equation
# (Table 5, 90 percent interval [0.244, 0.658]); these tests report the direct
# estimate beside it.
#
#   N1  Equation 17, Delta r_t = (1 - omega) Delta r_{t-1} + omega Delta r*_t,
#       with observed new-lease growth: pooled within OLS, unrestricted and
#       with the two coefficients summing to one; city by city; and within
#       2SLS with the drivers of r* (lagged tightness, wage and employment
#       growth) as instruments. Hansen's J then tests that the drivers reach
#       CPI rents only through r*, the exclusion restriction of Equation 5.
#   N2  The same on four-quarter changes, which damps quarterly noise.
#   N3  Log levels: CPI rents on the Equation 5 path of the new-lease index,
#       with a city constant and trend, over a grid of omega.
#   N4  Lead profiles and volatility of annual growth: observed, and implied
#       by Equation 5 at omega = 0.35 and at the direct estimate.
#   N5  Equations 19 and 20 with new-lease growth on the left, beside CPI rents
#       on the same sample: does r* follow the fundamentals, with little
#       persistence and long-run coefficients like those of CPI rents?
#   N6  Equation 22 for log new-lease rents, log CPI rents and log prices on
#       the same cities and quarters, and the CPI response that Equation 5
#       implies from the new-lease response, at omega = 0.35, at the direct
#       estimate, and at the omega that fits best, with the long-run pass-through
#       fixed at one (as in Equation 5) and free.
#   N7  Moving block bootstrap of N6 (blocks of 8 quarters, cities kept
#       together, as for Table 5): intervals for the best-fitting omega, how
#       often 0.35 fits worse than the direct estimate, and intervals for the
#       gaps between the new-lease response and the CPI and price responses.
#
# Reads:  clean/panel_combined.rds (01_build_panel.R)
#         clean_rev/new_lease_rents.csv (01b_new_lease_rents.R)
# Writes: output/new_lease_tests.txt, output/new_lease_lp.csv,
#         output/new_lease_boot.csv, output/fig_new_lease_lp.png
# SEs:    Driscoll-Kraay (Bartlett, 4 lags) as in 02_estimate.R; 6 lags for the
#         overlapping four-quarter changes; Newey-West for single cities.
# Version 2, 5 November 2026: adds prices to N6, the bootstrap (N7), the test
# that the unrestricted IV coefficients sum to one, and the horizon from which
# each response stays significant; the figure has two panels. N1 to N5 and the
# rent responses in N6 are unchanged.
# =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({
  library(dplyr); library(zoo); library(plm); library(lmtest); library(sandwich); library(ggplot2)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

CLEAN_REV <- "clean_rev"                  # where 01b_new_lease_rents.R writes
NL_CITIES <- c("Sydney", "Melbourne", "Brisbane", "Adelaide")
OMEGA_T5  <- 0.35                          # Table 5 (fixed in estimation)
OMEGA_CI  <- c(0.244, 0.658)               # Table 5, 90 percent bootstrap interval
LP_H      <- 12                            # Equation 22 horizons 0..LP_H
LP_LAGS   <- 4                             # Equation 22 control lags
BOOT_B     <- 399                          # bootstrap draws for N7 (0 skips N7)
BOOT_BLOCK <- 8                            # block length in quarters, as for Table 5
BOOT_SEED  <- 20261105

# ---- Helpers ---------------------------------------------------------------------

# Driscoll-Kraay covariance, as dk_vcov() in 02_estimate.R.
dk_v  <- function(m, L = 4) plm::vcovSCC(m, type = "HC0", maxlag = L)
dk_ct <- function(m, L = 4) lmtest::coeftest(m, vcov = dk_v(m, L))

# Lags and leads within a city that are NA unless the other quarter is exactly
# k quarters away (guards against gaps in either series).
lagq <- function(x, q, k = 1) {
  q <- as.numeric(q)
  ifelse(abs(q - dplyr::lag(q, k) - k / 4) < 1e-6, dplyr::lag(x, k), NA_real_)
}
leadq <- function(x, q, k) {
  if (k == 0) return(x)
  q <- as.numeric(q)
  ifelse(abs(dplyr::lead(q, k) - q - k / 4) < 1e-6, dplyr::lead(x, k), NA_real_)
}

# Long-run variance of quarter sums of the score matrix M (rows = city-quarters),
# Bartlett weights: the Driscoll-Kraay "meat", as in plm::vcovSCC(type = "HC0").
dk_meat <- function(M, qid, L) {
  G <- rowsum(M, qid)
  S <- crossprod(G)
  for (l in seq_len(L)) {
    A <- crossprod(G[-seq_len(l), , drop = FALSE], G[seq_len(nrow(G) - l), , drop = FALSE])
    S <- S + (1 - l / (L + 1)) * (A + t(A))
  }
  S
}

# Within (city-demeaned) 2SLS with Driscoll-Kraay errors, and Hansen's J from
# two-step GMM with the same long-run weighting. x: all regressors; z: all
# instruments (exogenous regressors appear in both).
iv_within <- function(df, y, x, z, L = 4) {
  df <- as.data.frame(df)
  df <- df[stats::complete.cases(df[, unique(c(y, x, z))]), ]
  dm <- function(v) v - ave(v, df$city)
  Y <- dm(df[[y]])
  X <- matrix(sapply(x, function(v) dm(df[[v]])), ncol = length(x), dimnames = list(NULL, x))
  Z <- matrix(sapply(z, function(v) dm(df[[v]])), ncol = length(z), dimnames = list(NULL, z))
  Xh <- Z %*% solve(crossprod(Z), crossprod(Z, X))
  B  <- solve(crossprod(Xh, X))
  b  <- drop(B %*% crossprod(Xh, Y))
  e  <- drop(Y - X %*% b)
  V  <- B %*% dk_meat(Xh * e, df$qid, L) %*% t(B)
  W  <- solve(dk_meat(Z * e, df$qid, L))
  ZX <- crossprod(Z, X)
  bg <- drop(solve(t(ZX) %*% W %*% ZX, t(ZX) %*% W %*% crossprod(Z, Y)))
  g  <- crossprod(Z, drop(Y - X %*% bg))
  J  <- drop(t(g) %*% W %*% g)
  dfJ <- ncol(Z) - ncol(X)
  list(coef = b, se = sqrt(diag(V)), V = V, J = J, dfJ = dfJ,
       pJ = if (dfJ > 0) pchisq(J, dfJ, lower.tail = FALSE) else NA_real_, n = nrow(df))
}

# Driscoll-Kraay Wald F that the listed coefficients of a plm model are zero.
dk_F <- function(m, terms, L = 4) {
  b <- coef(m)[terms]; V <- dk_v(m, L)[terms, terms, drop = FALSE]
  drop(t(b) %*% solve(V) %*% b) / length(terms)
}

# Equation 5 path of a measured rent from a market rent (logs), from the start
# of the market-rent series.
eq5_path <- function(rs, om) {
  r <- numeric(length(rs)); r[1] <- rs[1]
  for (i in seq_along(rs)[-1]) r[i] <- (1 - om) * r[i - 1] + om * rs[i]
  r
}

fmt_ci <- function(b, se) sprintf("%.3f (s.e. %.3f)", b, se)
qlab <- function(q) format(as.yearqtr(q), "%Y:Q%q")

# ---- Data ----------------------------------------------------------------------

panel <- readRDS(file.path(CLEAN, "panel_combined.rds")) |> arrange(city, quarter)

nl_all <- read.csv(file.path(CLEAN_REV, "new_lease_rents.csv")) |>
  mutate(quarter = as.yearqtr(year + (qtr - 1) / 4)) |>
  filter(city %in% NL_CITIES) |>
  transmute(city, quarter, nl_index = index, nl_index_sa = index_sa) |>
  arrange(city, quarter)
cities <- intersect(NL_CITIES, unique(nl_all$city))
if (!length(cities)) stop("No new-lease rents for ", paste(NL_CITIES, collapse = ", "), " in ",
                          file.path(CLEAN_REV, "new_lease_rents.csv"))

# Shock scale for Equation 22: one standard deviation of investor-credit growth
# (seasonally adjusted) over the five-city panel, as in the paper.
sd_shock <- panel |> group_by(city) |>
  mutate(d_linv = log(inv_commit_sa) - lagq(log(inv_commit_sa), quarter)) |>
  ungroup() |> pull(d_linv) |> sd(na.rm = TRUE)

d <- panel |>
  filter(city %in% cities) |>
  select(city, quarter, rents, prop_price, price_rent, tightness, wpi, employment, cash_rate, inv_commit_sa) |>
  inner_join(nl_all, by = c("city", "quarter")) |>
  arrange(city, quarter) |>
  group_by(city) |>
  mutate(l_cpi = log(rents), l_nl = log(nl_index_sa), l_price = log(prop_price),
         pp    = l_price - lagq(l_price, quarter),              # price growth (N6 controls)
         pi    = l_cpi - lagq(l_cpi, quarter),                  # Delta r, CPI rents
         pis   = l_nl - lagq(l_nl, quarter),                    # Delta r*, new-lease rents (s.a.)
         pi_l1 = lagq(pi, quarter), pis_l1 = lagq(pis, quarter),
         dpi   = pi - pi_l1, gap = pis - pi_l1,                 # Equation 17 with the sum imposed
         g4    = l_cpi - lagq(l_cpi, quarter, 4),
         g4s   = l_nl - lagq(l_nl, quarter, 4),
         g4_l1 = lagq(g4, quarter), y4 = g4 - g4_l1, x4 = g4s - g4_l1,
         tight_l1    = lagq(tightness, quarter),
         l_pr_gap_l1 = lagq(log(price_rent), quarter),
         d_lwpi = log(wpi) - lagq(log(wpi), quarter),
         d_lemp = log(employment) - lagq(log(employment), quarter),
         d_linv = log(inv_commit_sa) - lagq(log(inv_commit_sa), quarter),
         d_cash_l1 = lagq(cash_rate - lagq(cash_rate, quarter), quarter)) |>
  ungroup()
d$qid <- as.integer(factor(as.numeric(d$quarter)))
P <- pdata.frame(as.data.frame(d), index = c("city", "qid"))

sink(file.path(OUTDIR, "new_lease_tests.txt"), split = TRUE)
cat("02f_new_lease_tests.R, run", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Benchmark: omega =", OMEGA_T5, "(Table 5), 90 percent interval",
    sprintf("[%.3f, %.3f]", OMEGA_CI[1], OMEGA_CI[2]), "\n")
cat("\n== Sample: CPI rents (panel) joined to new-lease rents (01b), by city ==\n")
for (cty in cities) {
  x <- d |> filter(city == cty, is.finite(dpi), is.finite(gap))
  cat(sprintf("  %-10s %s to %s, %d quarters with Equation 17 data\n", cty,
              qlab(min(x$quarter)), qlab(max(x$quarter)), nrow(x)))
}

# ---- N1. Equation 17 -------------------------------------------------------------

cat("\n== N1. Equation 17: Delta r_t = (1 - omega) Delta r_{t-1} + omega Delta r*_t ==\n")
m_u <- plm(pi ~ pi_l1 + pis, data = P, model = "within")
ct_u <- dk_ct(m_u); V_u <- dk_v(m_u)
s_u <- sum(coef(m_u)[c("pi_l1", "pis")])
se_s <- sqrt(sum(V_u[c("pi_l1", "pis"), c("pi_l1", "pis")]))
cat("Pooled within OLS, unrestricted (DK):\n"); print(round(ct_u, 4))
cat(sprintf("  Sum of the two coefficients %.3f (s.e. %.3f); test sum = 1: p = %.3f\n",
            s_u, se_s, 2 * pnorm(-abs((s_u - 1) / se_s))))

m_r <- plm(dpi ~ gap, data = P, model = "within")
ct_r <- dk_ct(m_r)
omega_hat <- unname(coef(m_r)["gap"]); omega_se <- ct_r["gap", 2]
cat(sprintf("Pooled within OLS, sum imposed: omega = %s, N = %d\n", fmt_ci(omega_hat, omega_se), nobs(m_r)))

by_city <- bind_rows(lapply(cities, function(cty) {
  x <- d |> filter(city == cty, is.finite(dpi), is.finite(gap))
  m <- lm(dpi ~ gap, data = x)
  data.frame(city = cty, from = qlab(min(x$quarter)), to = qlab(max(x$quarter)), n = nrow(x),
             omega = unname(coef(m)["gap"]),
             se = sqrt(sandwich::NeweyWest(m, lag = 4, prewhite = FALSE)["gap", "gap"]))
}))
cat("City by city, sum imposed, with constant (Newey-West, 4 lags):\n")
print(by_city |> mutate(across(c(omega, se), ~ round(.x, 3))), row.names = FALSE)

inst <- c("pi_l1", "tight_l1", "d_lwpi", "d_lemp")
iv_r <- iv_within(d, "dpi", "gap", inst)
m_fs <- plm(gap ~ pi_l1 + tight_l1 + d_lwpi + d_lemp, data = P, model = "within")
cat(sprintf("Within 2SLS, sum imposed, instruments lagged CPI rent growth, lagged tightness,\n  wage and employment growth: omega = %s, N = %d\n",
            fmt_ci(iv_r$coef[["gap"]], iv_r$se[["gap"]]), iv_r$n))
cat(sprintf("  First stage DK Wald F (4 instruments) = %.1f; Hansen J = %.2f, df = %d, p = %.3f\n",
            dk_F(m_fs, inst), iv_r$J, iv_r$dfJ, iv_r$pJ))
iv_u <- iv_within(d, "pi", c("pi_l1", "pis"), inst)
m_fs_u <- plm(pis ~ pi_l1 + tight_l1 + d_lwpi + d_lemp, data = P, model = "within")
cat(sprintf("Within 2SLS, unrestricted (new-lease growth instrumented): lagged CPI growth %s,\n  new-lease growth %s; first stage DK Wald F (3 excluded) = %.1f; Hansen J = %.2f, df = %d, p = %.3f\n",
            fmt_ci(iv_u$coef[["pi_l1"]], iv_u$se[["pi_l1"]]), fmt_ci(iv_u$coef[["pis"]], iv_u$se[["pis"]]),
            dk_F(m_fs_u, c("tight_l1", "d_lwpi", "d_lemp")), iv_u$J, iv_u$dfJ, iv_u$pJ))
s_iv  <- sum(iv_u$coef[c("pi_l1", "pis")])
se_iv <- sqrt(sum(iv_u$V[c("pi_l1", "pis"), c("pi_l1", "pis")]))
cat(sprintf("  Sum of the two IV coefficients %.3f (s.e. %.3f); test sum = 1: p = %.3f\n",
            s_iv, se_iv, 2 * pnorm(-abs((s_iv - 1) / se_iv))))

# ---- N2. Four-quarter changes ------------------------------------------------------

cat("\n== N2. Equation 17 on four-quarter changes, sum imposed ==\n")
m4 <- plm(y4 ~ x4, data = P, model = "within")
ct4 <- dk_ct(m4, L = 6)
cat(sprintf("Pooled within OLS (DK, 6 lags): omega = %s, N = %d\n", fmt_ci(coef(m4)[["x4"]], ct4["x4", 2]), nobs(m4)))
by_city4 <- bind_rows(lapply(cities, function(cty) {
  x <- d |> filter(city == cty, is.finite(y4), is.finite(x4))
  m <- lm(y4 ~ x4, data = x)
  data.frame(city = cty, n = nrow(x), omega = unname(coef(m)["x4"]),
             se = sqrt(sandwich::NeweyWest(m, lag = 6, prewhite = FALSE)["x4", "x4"]))
}))
cat("City by city, with constant (Newey-West, 6 lags):\n")
print(by_city4 |> mutate(across(c(omega, se), ~ round(.x, 3))), row.names = FALSE)

# ---- N3. Log levels ---------------------------------------------------------------

cat("\n== N3. Log CPI rents on the Equation 5 path of log new-lease rents (s.a.) ==\n")
cat("City constant and trend; the first 8 quarters of each path are burn-in.\n")
grid <- seq(0.02, 1, by = 0.01)
lev <- bind_rows(lapply(cities, function(cty) {
  h <- nl_all |> filter(city == cty) |> arrange(quarter)
  win <- d |> filter(city == cty) |> select(quarter, l_cpi)
  bind_rows(lapply(grid, function(om) {
    pth <- data.frame(quarter = h$quarter, path = eq5_path(log(h$nl_index_sa), om))[-(1:8), ]
    x <- inner_join(win, pth, by = "quarter") |> mutate(trend = as.numeric(quarter))
    m <- lm(l_cpi ~ path + trend, data = x)
    data.frame(city = cty, omega = om, ssr = sum(resid(m)^2), slope = unname(coef(m)["path"]), n = nrow(x))
  }))
}))
best_city <- lev |> group_by(city) |> slice(which.min(ssr)) |> ungroup()
pooled <- lev |> group_by(omega) |> summarise(ssr = sum(ssr), .groups = "drop")
cat("Best omega by city (slope on the path at that omega):\n")
print(best_city |> transmute(city, omega, slope = round(slope, 3), n), row.names = FALSE)
cat(sprintf("Pooled (sum of city SSRs): best omega %.2f; SSR at omega = %.2f is %.1f times the minimum\n",
            pooled$omega[which.min(pooled$ssr)], OMEGA_T5,
            pooled$ssr[abs(pooled$omega - OMEGA_T5) < 1e-9] / min(pooled$ssr)))

# ---- N4. Lead profiles and volatility -------------------------------------------------

cat("\n== N4. Annual CPI rent growth against annual new-lease growth k quarters earlier ==\n")
cat(sprintf("Observed, and implied by Equation 5 at omega = %.2f (Table 5) and %.2f (N1 pooled)\n",
            OMEGA_T5, omega_hat))
prof <- function(g, gn) sapply(0:6, function(k) cor(dplyr::lag(gn, k), g, use = "complete.obs"))
lead_tab <- bind_rows(lapply(cities, function(cty) {
  h <- nl_all |> filter(city == cty) |> arrange(quarter) |>
    mutate(rs = log(nl_index), r_t5 = eq5_path(rs, OMEGA_T5), r_hat = eq5_path(rs, omega_hat))
  a <- d |> filter(city == cty) |> select(quarter, l_cpi) |>
    inner_join(h |> select(quarter, rs, r_t5, r_hat), by = "quarter") |> arrange(quarter) |>
    mutate(gc = l_cpi - lag(l_cpi, 4), gn = rs - lag(rs, 4),
           g_t5 = r_t5 - lag(r_t5, 4), g_hat = r_hat - lag(r_hat, 4))
  ok <- is.finite(a$gc)
  out <- list(observed = prof(a$gc, a$gn), t5 = prof(a$g_t5, a$gn), hat = prof(a$g_hat, a$gn))
  bind_rows(lapply(names(out), function(nm) data.frame(
    city = cty, series = nm, peak_k = which.max(out[[nm]]) - 1,
    sd_pts = 100 * sd(c(observed = list(a$gc), t5 = list(a$g_t5), hat = list(a$g_hat))[[nm]][ok], na.rm = TRUE),
    profile = paste(sprintf("%.2f", out[[nm]]), collapse = " "))))
}))
lead_tab$series <- recode(lead_tab$series, observed = "observed",
                          t5 = sprintf("omega %.2f", OMEGA_T5), hat = sprintf("omega %.2f", omega_hat))
print(lead_tab |> mutate(sd_pts = round(sd_pts, 1)), row.names = FALSE)
cat("peak_k: lag of the highest correlation; sd_pts: standard deviation of annual CPI rent growth, points\n")

# ---- N5. Equations 19 and 20 for market rents ---------------------------------------

cat("\n== N5. Equations 19 and 20 with CPI and with new-lease rent growth (same sample) ==\n")
rhs <- "l_pr_gap_l1 + tight_l1 + d_lwpi + d_lemp"
fits <- list(
  cpi_static = plm(as.formula(paste("pi ~", rhs)), data = P, model = "within"),
  nl_static  = plm(as.formula(paste("pis ~", rhs)), data = P, model = "within"),
  cpi_dyn    = plm(as.formula(paste("pi ~ pi_l1 +", rhs)), data = P, model = "within"),
  nl_dyn     = plm(as.formula(paste("pis ~ pis_l1 +", rhs)), data = P, model = "within"))
for (nm in names(fits)) {
  cat(sprintf("\n%s (DK), N = %d:\n", nm, nobs(fits[[nm]]))); print(round(dk_ct(fits[[nm]]), 4))
}
long_run <- function(m, lagv) {
  b <- coef(m); V <- dk_v(m); rho <- b[[lagv]]
  bind_rows(lapply(c("tight_l1", "d_lwpi", "d_lemp"), function(k) {
    gr <- c(1 / (1 - rho), b[[k]] / (1 - rho)^2)
    data.frame(term = k, long_run = b[[k]] / (1 - rho),
               se = sqrt(drop(t(gr) %*% V[c(k, lagv), c(k, lagv)] %*% gr)))
  }))
}
lr <- bind_rows(long_run(fits$cpi_dyn, "pi_l1") |> mutate(rents = "CPI"),
                long_run(fits$nl_dyn, "pis_l1") |> mutate(rents = "new-lease"))
cat("\nLong-run coefficients b/(1 - rho) from the dynamic form (delta-method DK s.e.):\n")
print(lr |> mutate(across(c(long_run, se), ~ round(.x, 3))), row.names = FALSE)
cat(sprintf("Persistence rho: CPI rents %.3f, new-lease rents %.3f\n",
            coef(fits$cpi_dyn)[["pi_l1"]], coef(fits$nl_dyn)[["pis_l1"]]))

# ---- N6. Equation 22 for new-lease rents, CPI rents and prices -------------------------

cat("\n== N6. Equation 22: cumulative response to a one-SD credit innovation ==\n")
cat(sprintf("Shock: %.3f log points (SD of investor-credit growth, five cities); %d control lags;\n",
            sd_shock, LP_LAGS))
cat("all three outcomes on the same city-quarters at each horizon.\n")
dl <- d |> group_by(city) |> arrange(quarter, .by_group = TRUE)
for (j in seq_len(LP_LAGS)) {
  dl <- dl |> mutate("d_linv_l{j}" := lagq(d_linv, quarter, j),
                     "pi_l{j}x"    := lagq(pi, quarter, j),
                     "pis_l{j}x"   := lagq(pis, quarter, j),
                     "pp_l{j}x"    := lagq(pp, quarter, j))
}
dl <- ungroup(dl)
base_ctrl <- c(paste0("d_linv_l", seq_len(LP_LAGS)), "tight_l1", "d_cash_l1")
own_lags  <- list(lhs_c = paste0("pi_l",  seq_len(LP_LAGS), "x"),    # each outcome's own growth lags
                  lhs_n = paste0("pis_l", seq_len(LP_LAGS), "x"),
                  lhs_p = paste0("pp_l",  seq_len(LP_LAGS), "x"))
# One regression frame per horizon. Rows need all three outcomes, so the rent
# responses use the rows of version 1 unless some lack price data (reported).
lp_frames <- lapply(0:LP_H, function(h) {
  x <- dl |> group_by(city) |>
    mutate(lhs_c = leadq(l_cpi, quarter, h) - lagq(l_cpi, quarter),
           lhs_n = leadq(l_nl, quarter, h) - lagq(l_nl, quarter),
           lhs_p = leadq(l_price, quarter, h) - lagq(l_price, quarter)) |> ungroup()
  need_r <- c("lhs_c", "lhs_n", "d_linv", base_ctrl, own_lags$lhs_c, own_lags$lhs_n)
  need   <- c(need_r, "lhs_p", own_lags$lhs_p)
  x <- as.data.frame(x)
  out <- x[stats::complete.cases(x[, need]), c("city", "qid", need)]
  attr(out, "lost") <- sum(stats::complete.cases(x[, need_r])) - nrow(out)
  out
})
lost <- sapply(lp_frames, attr, "lost")
if (any(lost > 0)) {
  cat("City-quarters dropped for lack of price data, by horizon:", paste(lost, collapse = " "), "\n")
}
lp_one <- function(x, lhs) {
  Px <- pdata.frame(x, index = c("city", "qid"))
  f <- as.formula(paste(lhs, "~ d_linv +", paste(c(base_ctrl, own_lags[[lhs]]), collapse = " + ")))
  m <- plm(f, data = Px, model = "within")
  c(b = sd_shock * coef(m)[["d_linv"]], se = sd_shock * sqrt(dk_v(m)["d_linv", "d_linv"]))
}
lp <- bind_rows(lapply(0:LP_H, function(h) {
  x <- lp_frames[[h + 1]]
  rc <- lp_one(x, "lhs_c"); rn <- lp_one(x, "lhs_n"); rp <- lp_one(x, "lhs_p")
  data.frame(h = h, n = nrow(x), cpi = rc[["b"]], cpi_se = rc[["se"]], nl = rn[["b"]], nl_se = rn[["se"]],
             price = rp[["b"]], price_se = rp[["se"]])
}))
imp <- function(b, om) sapply(seq_along(b), function(i) om * sum((1 - om)^(i - seq_len(i)) * b[seq_len(i)]))
lp <- lp |> mutate(cpi_from_nl_t5 = imp(nl, OMEGA_T5), cpi_from_nl_hat = imp(nl, omega_hat))
print(lp |> mutate(across(-c(h, n), ~ round(.x, 4))), row.names = FALSE)

# Horizon from which a response stays significant at 10 percent through LP_H
# (NA if it is not significant at LP_H).
sig_from <- function(b, se) {
  ns <- which(abs(b / se) <= qnorm(0.95))
  if (!length(ns)) return(0L)
  if (max(ns) == length(b)) NA_integer_ else max(ns)
}
cat(sprintf("Significant at 10 percent from horizon k through %d: CPI rents %s, new-lease rents %s, prices %s\n",
            LP_H, sig_from(lp$cpi, lp$cpi_se), sig_from(lp$nl, lp$nl_se), sig_from(lp$price, lp$price_se)))
for (h in c(4, 8, 12)) {
  cat(sprintf("  h = %2d: new-lease rents %.4f, CPI rents %.4f, prices %.4f; CPI/new-lease %.2f, new-lease/price %.2f\n",
              h, lp$nl[h + 1], lp$cpi[h + 1], lp$price[h + 1], lp$cpi[h + 1] / lp$nl[h + 1],
              lp$nl[h + 1] / lp$price[h + 1]))
}

# Equation 5 applied to the new-lease response: the CPI response it predicts at
# each omega on a grid (A_w %*% nl), and the omega that fits best.
grid_w <- seq(0.02, 1, by = 0.01)
Hn <- LP_H + 1
A_w <- lapply(grid_w, function(om) outer(seq_len(Hn), seq_len(Hn), function(i, j) ifelse(j <= i, om * (1 - om)^(i - j), 0)))
rms_grid <- function(cpi, nl) sapply(A_w, function(A) sqrt(mean((cpi - drop(A %*% nl))^2)))
rms_full <- rms_grid(lp$cpi, lp$nl)
best_full <- grid_w[which.min(rms_full)]
cat(sprintf("Root mean squared gap, CPI response minus Equation 5 from new-lease response: omega %.2f %.4f, omega %.2f %.4f\n",
            OMEGA_T5, sqrt(mean((lp$cpi - lp$cpi_from_nl_t5)^2)),
            omega_hat, sqrt(mean((lp$cpi - lp$cpi_from_nl_hat)^2))))
cat(sprintf("Best-fitting omega on the grid %.2f-%.2f: %.2f (RMS gap %.4f); at the Table 5 bounds %.3f and %.3f: %.4f and %.4f\n",
            min(grid_w), max(grid_w), best_full, min(rms_full), OMEGA_CI[1], OMEGA_CI[2],
            sqrt(mean((lp$cpi - imp(lp$nl, OMEGA_CI[1]))^2)), sqrt(mean((lp$cpi - imp(lp$nl, OMEGA_CI[2]))^2))))
# The fit above assumes that CPI rents absorb new-lease rents one for one in the
# long run, as Equation 5 does. N1 (unrestricted) and N3 suggest they absorb
# less. With a free scale lambda (CPI response = lambda x prediction), speed and
# scale trade off, so this fit says how much the timing alone pins down omega.
fit_scale <- function(cpi, nl) {
  t(sapply(A_w, function(A) {
    p <- drop(A %*% nl); lam <- sum(p * cpi) / sum(p * p)
    c(lam = lam, rms = sqrt(mean((cpi - lam * p)^2)))
  }))
}
fs_full <- fit_scale(lp$cpi, lp$nl)
i_t5 <- which(abs(grid_w - OMEGA_T5) < 1e-9); i_bs <- which.min(fs_full[, "rms"])
best_full_s <- grid_w[i_bs]
cat(sprintf("With a free scale: best omega %.2f, lambda %.2f (RMS gap %.4f); at omega = %.2f, lambda %.2f (RMS gap %.4f)\n",
            best_full_s, fs_full[i_bs, "lam"], fs_full[i_bs, "rms"], OMEGA_T5, fs_full[i_t5, "lam"], fs_full[i_t5, "rms"]))
cat("  RMS gap with a free scale at omega = 0.10, 0.15, 0.20, 0.25, 0.35, 0.50:",
    sprintf("%.4f", fs_full[match(c(10, 15, 20, 25, 35, 50), round(100 * grid_w)), "rms"]), "\n")
write.csv(lp, file.path(OUTDIR, "new_lease_lp.csv"), row.names = FALSE)

# ---- N7. Block bootstrap of N6 ---------------------------------------------------------

# Within (city-demeaned) OLS coefficient on the first column of X; the same
# estimator as plm(model = "within") in N6, without the standard errors.
within_coef <- function(y, X, g) {
  g  <- as.integer(factor(g))
  ng <- tabulate(g)
  yd <- y - (rowsum(y, g)[, 1] / ng)[g]
  Xd <- X - (rowsum(X, g) / ng)[g, , drop = FALSE]
  stats::lm.fit(Xd, yd)$coefficients[[1]]
}
boot <- NULL
if (BOOT_B > 0) {
  cat(sprintf("\n== N7. Moving block bootstrap of N6: %d draws, blocks of %d quarters, cities kept together ==\n",
              BOOT_B, BOOT_BLOCK))
  cat("Each draw resamples the quarters of the h = 0 sample and re-estimates the three projections\n")
  cat("at every horizon; the shock scale stays at its full-sample value.\n")
  Xcols <- lapply(own_lags, function(o) c("d_linv", base_ctrl, o))
  chk <- sd_shock * within_coef(lp_frames[[1]]$lhs_c, as.matrix(lp_frames[[1]][, Xcols$lhs_c]), lp_frames[[1]]$city)
  if (abs(chk - lp$cpi[1]) > 1e-10) stop("Bootstrap estimator does not reproduce N6 at h = 0")
  set.seed(BOOT_SEED)
  Q  <- sort(unique(lp_frames[[1]]$qid)); TQ <- length(Q); nb <- ceiling(TQ / BOOT_BLOCK)
  rows_q <- lapply(lp_frames, function(x) split(seq_len(nrow(x)), x$qid))
  draws <- array(NA_real_, c(BOOT_B, Hn, 3), dimnames = list(NULL, NULL, c("cpi", "nl", "price")))
  key <- c(cpi = "lhs_c", nl = "lhs_n", price = "lhs_p")
  for (b in seq_len(BOOT_B)) {
    st <- sample.int(TQ - BOOT_BLOCK + 1, nb, replace = TRUE)
    qq <- Q[as.vector(outer(0:(BOOT_BLOCK - 1), st, "+"))][seq_len(TQ)]
    for (h in seq_len(Hn)) {
      xb <- lp_frames[[h]][unlist(rows_q[[h]][as.character(qq)], use.names = FALSE), , drop = FALSE]
      for (k in names(key)) {
        draws[b, h, k] <- sd_shock * within_coef(xb[[key[[k]]]], as.matrix(xb[, Xcols[[key[[k]]]]]), xb$city)
      }
    }
  }
  ok <- stats::complete.cases(matrix(draws, BOOT_B))
  D  <- draws[ok, , , drop = FALSE]
  q90 <- function(v) quantile(v, c(0.05, 0.95), names = FALSE)
  best_w  <- apply(D, 1, function(z) grid_w[which.min(rms_grid(z[, "cpi"], z[, "nl"]))])
  best_ws <- apply(D, 1, function(z) grid_w[which.min(fit_scale(z[, "cpi"], z[, "nl"])[, "rms"])])
  rms_t5  <- apply(D, 1, function(z) sqrt(mean((z[, "cpi"] - imp(z[, "nl"], OMEGA_T5))^2)))
  rms_hat <- apply(D, 1, function(z) sqrt(mean((z[, "cpi"] - imp(z[, "nl"], omega_hat))^2)))
  cat(sprintf("Usable draws: %d of %d\n", sum(ok), BOOT_B))
  cat(sprintf("Best-fitting omega: full sample %.2f; bootstrap median %.2f, 90 percent interval [%.2f, %.2f]\n",
              best_full, median(best_w), q90(best_w)[1], q90(best_w)[2]))
  cat(sprintf("  Share of draws with best omega below %.3f (Table 5 lower bound) %.2f, below %.2f %.2f\n",
              OMEGA_CI[1], mean(best_w < OMEGA_CI[1]), OMEGA_T5, mean(best_w < OMEGA_T5)))
  cat(sprintf("  Share of draws in which omega = %.2f fits worse than omega = %.2f: %.2f\n",
              OMEGA_T5, omega_hat, mean(rms_t5 > rms_hat)))
  cat(sprintf("Best-fitting omega with a free scale: full sample %.2f; bootstrap median %.2f, 90 percent interval [%.2f, %.2f]\n",
              best_full_s, median(best_ws), q90(best_ws)[1], q90(best_ws)[2]))
  cat(sprintf("  Share of draws with that omega below %.3f %.2f, below %.2f %.2f\n",
              OMEGA_CI[1], mean(best_ws < OMEGA_CI[1]), OMEGA_T5, mean(best_ws < OMEGA_T5)))
  cat("Gaps between responses, full sample [90 percent bootstrap interval] (share of draws above zero):\n")
  for (h in c(4, 8, 12)) {
    dnc <- D[, h + 1, "nl"] - D[, h + 1, "cpi"]; dnp <- D[, h + 1, "nl"] - D[, h + 1, "price"]
    cat(sprintf("  h = %2d: new-lease minus CPI %.4f [%.4f, %.4f] (%.2f); new-lease minus price %.4f [%.4f, %.4f] (%.2f)\n",
                h, lp$nl[h + 1] - lp$cpi[h + 1], q90(dnc)[1], q90(dnc)[2], mean(dnc > 0),
                lp$nl[h + 1] - lp$price[h + 1], q90(dnp)[1], q90(dnp)[2], mean(dnp > 0)))
  }
  boot <- data.frame(draw = which(ok), best_omega = best_w, best_omega_scale = best_ws,
                     rms_t5 = rms_t5, rms_hat = rms_hat,
                     setNames(as.data.frame(D[, , "cpi"]),   paste0("cpi_h", 0:LP_H)),
                     setNames(as.data.frame(D[, , "nl"]),    paste0("nl_h", 0:LP_H)),
                     setNames(as.data.frame(D[, , "price"]), paste0("price_h", 0:LP_H)))
  write.csv(boot, file.path(OUTDIR, "new_lease_boot.csv"), row.names = FALSE)
}

# ---- Summary ---------------------------------------------------------------------------

cat("\n== Summary ==\n")
cat(sprintf("Direct omega (Equation 17, quarterly, pooled): %s; four-quarter: %.3f; levels: %.2f\n",
            fmt_ci(omega_hat, omega_se), coef(m4)[["x4"]], pooled$omega[which.min(pooled$ssr)]))
cat(sprintf("IV: %s, Hansen J p = %.3f. Table 5: %.2f [%.3f, %.3f]\n",
            fmt_ci(iv_r$coef[["gap"]], iv_r$se[["gap"]]), iv_r$pJ, OMEGA_T5, OMEGA_CI[1], OMEGA_CI[2]))
obs_peaks <- lead_tab |> filter(series == "observed")
cat("Observed lead peaks (quarters):", paste(sprintf("%s %d", obs_peaks$city, obs_peaks$peak_k), collapse = ", "), "\n")
cat(sprintf("Credit innovation, h = 12: prices %.4f, new-lease rents %.4f, CPI rents %.4f\n",
            lp$price[Hn], lp$nl[Hn], lp$cpi[Hn]))
cat(sprintf("Best-fitting omega from the responses: %.2f%s; with a free scale %.2f%s\n", best_full,
            if (is.null(boot)) "" else sprintf(" [%.2f, %.2f]", q90(boot$best_omega)[1], q90(boot$best_omega)[2]),
            best_full_s,
            if (is.null(boot)) "" else sprintf(" [%.2f, %.2f]", q90(boot$best_omega_scale)[1], q90(boot$best_omega_scale)[2])))
sink()

# ---- Figure ----------------------------------------------------------------------------

# Panel (a): the three responses with 90 percent Driscoll-Kraay bands, as in
# Figure 5. Panel (b): the CPI response beside the CPI response that Equation 5
# predicts from the new-lease response. The predictions share the new-lease
# colour and differ by line type. Prices and CPI rents keep their colours from
# Figure 10; the new-lease amber passes colour-vision checks against both.
lab   <- c(a = "(a) Prices and rents", b = "(b) CPI rents and lease repricing")
s_lab <- c(p = "Prices", n = "New-lease rents", c = "CPI rents",
           t5  = sprintf("Predicted CPI rents, \u03c9 = %.2f", OMEGA_T5),
           hat = sprintf("Predicted CPI rents, \u03c9 = %.2f", omega_hat))
mk <- function(panel, s, v, se = NULL) {
  data.frame(panel = lab[[panel]], series = s_lab[[s]], h = lp$h, value = v,
             lo = if (is.null(se)) NA_real_ else v - qnorm(0.95) * se,
             hi = if (is.null(se)) NA_real_ else v + qnorm(0.95) * se)
}
fig <- bind_rows(mk("a", "p", lp$price, lp$price_se), mk("a", "n", lp$nl, lp$nl_se),
                 mk("a", "c", lp$cpi, lp$cpi_se), mk("b", "c", lp$cpi, lp$cpi_se),
                 mk("b", "t5", lp$cpi_from_nl_t5), mk("b", "hat", lp$cpi_from_nl_hat))
fig$panel  <- factor(fig$panel, levels = unname(lab))
fig$series <- factor(fig$series, levels = unname(s_lab))
pal <- setNames(c("#c0392b", "#c87f0a", "#1b6f7a", "#c87f0a", "#c87f0a"), s_lab)
lty <- setNames(c("solid", "solid", "solid", "dashed", "dotted"), s_lab)
g <- ggplot(fig, aes(h, value)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_ribbon(data = fig[!is.na(fig$lo), ], aes(ymin = lo, ymax = hi, fill = series),
              alpha = 0.13, colour = NA, show.legend = FALSE) +
  geom_line(aes(colour = series, linetype = series), linewidth = 0.9) +   # as Figure 10
  facet_wrap(~panel) +
  scale_x_continuous(breaks = seq(0, LP_H, 4)) +
  scale_colour_manual(values = pal) + scale_linetype_manual(values = lty) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = "Quarters after the credit innovation", y = "Cumulative response, log points",
       colour = NULL, linetype = NULL) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.spacing.x = grid::unit(1.4, "lines"), legend.key.width = grid::unit(2.2, "lines"))
# 6.5 inches wide, like Figure 10, so its text prints at about 8 pt at \textwidth.
ggsave(file.path(OUTDIR, "fig_new_lease_lp.png"), g, width = 6.5, height = 3.8, dpi = 600)
cat("Wrote", file.path(OUTDIR, "new_lease_tests.txt"), ", new_lease_lp.csv, new_lease_boot.csv and fig_new_lease_lp.png\n")
