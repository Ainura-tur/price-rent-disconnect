# =============================================================================
# 04_model_matching.R -- quantitative evaluation of the model
#
#   M1  Estimate model parameters by matching the model's impulse responses to
#       the panel local projections of prices and rents (Figure 5).
#   M2  Decompose the price-rent wedge: lease repricing, tenure speed, supply.
#   M3  Counterfactual: a tightening of credit conditions held for 16 quarters.
#   M4  State dependence: the model's predicted credit x cash-rate interaction
#       against the estimate and its confidence interval.
#
# Reads:  output/sa_C_lp.csv (from 02c_sa_additions.R), panel, lending
# Writes: output/model_*.csv, output/model_*.txt, output/fig_model_fit.png,
#         output/fig_model_counterfactual.png
# =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({
  library(plm); library(lmtest); library(sandwich); library(ggplot2)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

set.seed(20261030)
H  <- 12
TT <- 200            # simulation length for the forward-looking price
OMEGA  <- read_csv(file.path(OUTDIR, "sa_B_omega.csv"), show_col_types = FALSE)$omega   # repricing rate from the dynamic rent equation (02c)
VARPI  <- 0.99       # weight on next quarter's price (gross yield ~1% a quarter)
DELTA_K <- 0.01      # decay of a deviation in the rental stock, per quarter
SIGMA  <- 0.1006     # one standard deviation of SA investor-credit growth

# ---- Targets: baseline local projections -----------------------------------
lp <- read_csv(file.path(OUTDIR, "sa_C_lp.csv"), show_col_types = FALSE)
tp <- lp |> filter(spec == "Price, baseline") |> arrange(h)
tr <- lp |> filter(spec == "Rent, baseline")  |> arrange(h)
tc <- lp |> filter(spec == "Credit, baseline", h >= 1) |> arrange(h)
# Credit at h = 0 equals one SD by construction in both data and model, so it is
# not a moment.
m_data <- c(tp$beta, tr$beta, tc$beta); m_se <- c(tp$se, tr$se, tc$se)
W <- 1 / m_se^2

# ---- Model -------------------------------------------------------------------
# Credit conditions: growth g_t = rho_g^t after a unit shock and level
# phi_t = rho_d phi_{t-1} + g_t, so credit conditions can keep rising after the
# shock. Measured credit (Eq. 11, purchases held fixed) is iota_t = phi_t + p_t.
# Asset value: v_t = eps*phi_t + (1-varpi) r_{t+1} + varpi v_{t+1}   (Eq. 10)
# Transaction price: p_t = (1-theta) p_{t-1} + theta v_t
# Tenure: s_t = (1-kappa) s_{t-1} + kappa*eta*p_{t-1}                (Eq. 4)
# Stock:  k_t = (1-delta_k) k_{t-1} + chi*p_{t-J}                    (Eqs. 12-13)
# Rents:  rstar_t = s_t - k_t;  r_t = (1-omega) r_{t-1} + omega rstar_t (Eq. 5)
solve_path <- function(phi, eps, kappa, eta, chi, J, theta = 1, omega = OMEGA,
                       varpi = VARPI, delta_k = DELTA_K) {
  # The model is linear, so solve it in one step.
  # Asset value:  v_t = eps*phi_t + (1-varpi) r_{t+1} + varpi v_{t+1}   (Eq. 10)
  # Transaction price adjusts gradually: p_t = (1-theta) p_{t-1} + theta v_t
  # Rents are a backward filter of prices, r = R p (Eqs. 4, 5, 12, 13).
  n <- length(phi); j <- 0:(n - 1)
  a <- c(0, kappa * eta * (1 - kappa)^(j[-n]))                 # tenure kernel
  b <- ifelse(j >= J, chi * (1 - delta_k)^(j - J), 0)          # stock kernel
  dker <- as.numeric(stats::filter(a - b, 1 - omega, method = "recursive")) * omega
  low <- function(k) { M <- matrix(0, n, n); idx <- row(M) - col(M); L <- idx >= 0; M[L] <- k[idx[L] + 1]; M }
  R  <- low(dker)
  Th <- low(theta * (1 - theta)^j)
  RT <- R %*% Th                                   # rents as a filter of the asset value
  B <- varpi * diag(n) + (1 - varpi) * RT
  A <- diag(n); A[1:(n - 1), ] <- A[1:(n - 1), ] - B[2:n, ]
  v <- solve(A, eps * phi)
  p <- Th %*% v
  list(p = as.numeric(p), r = as.numeric(R %*% p), v = as.numeric(v))
}
credit_path <- function(rho_g, rho_d, n = TT) {
  g <- rho_g^(0:(n - 1)); phi <- numeric(n)
  for (t in 1:n) phi[t] <- (if (t > 1) rho_d * phi[t - 1] else 0) + g[t]
  list(g = g, phi = phi)
}
# Impulse responses to a credit innovation scaled so that measured credit growth
# on impact (Eq. 11: growth of phi plus the price change) equals one SD.
model_irf <- function(th, J, omega = OMEGA, chi_override = NULL, theta_override = NULL,
                      kappa_override = NULL) {
  eps <- th[1]; rho_g <- th[2]; rho_d <- th[3]; theta <- th[4]; kappa <- th[5]
  eta <- th[6]; chi <- th[7]
  if (!is.null(chi_override))   chi   <- chi_override
  if (!is.null(theta_override)) theta <- theta_override
  if (!is.null(kappa_override)) kappa <- kappa_override
  cp <- credit_path(rho_g, rho_d)
  sol <- solve_path(cp$phi, eps, kappa, eta, chi, J, theta, omega)
  scale <- SIGMA / (cp$phi[1] + sol$p[1])       # measured credit growth on impact = 1 SD
  cr <- (cp$phi + sol$p) * scale
  list(p = sol$p[1:(H + 1)] * scale, r = sol$r[1:(H + 1)] * scale, c = cr[2:(H + 1)],
       p_long = sol$p * scale, r_long = sol$r * scale, scale = scale, phi = cp$phi * scale)
}
# Bounds. eta stops at 1. With eta above 1 + chi/delta_k, a price rise lifts long-run
# rents, and so the asset value, by more than itself. Every root of the model then
# lies inside the unit circle and there is no unique bounded solution: solve_path()
# still returns numbers, but they change with TT and mean nothing.
lo <- c(1e-5, 0.0, 0.50, 0.02, 0.02, 0.0, 0.0)
hi <- c(1.0, 0.95, 0.995, 1.0, 1.0, 1.0, 0.05)
pn <- c("eps", "rho_g", "rho_d", "theta", "kappa", "eta", "chi")
obj <- function(th, J, omega = OMEGA) {
  m <- model_irf(th, J, omega); sum(W * (c(m$p, m$r, m$c) - m_data)^2)
}
# The distance is a sum of squared weighted residuals, so the search takes bounded
# Gauss-Newton steps (nlminb with the Jacobian of the residuals). It works on the
# logs of eps, theta and kappa: a long curved valley runs along eps*theta, and in
# logs that valley is straight. SU is the typical scale of each search variable.
# free lists the parameters to estimate; the others stay at their values in st.
LOGP <- c(1, 4, 5); SU <- c(1, 0.4, 0.2, 1, 1, 0.7, 0.01)
to_u <- function(z) { z[LOGP] <- log(z[LOGP]); z }
to_z <- function(u) { u[LOGP] <- exp(u[LOGP]); u }
fit_ls <- function(st, J, omega = OMEGA, free = 1:7) {
  u0 <- to_u(st); lu <- to_u(lo); hu <- to_u(hi)
  resid <- function(u) { m <- model_irf(to_z(u), J, omega); sqrt(W) * (c(m$p, m$r, m$c) - m_data) }
  full  <- function(x) { u <- u0; u[free] <- x; u }
  last  <- NULL
  jac <- function(x) {
    if (!is.null(last) && identical(last$x, x)) return(last)
    u <- full(x); r0 <- resid(u)
    Jm <- sapply(free, function(j) {
      h <- 1e-5 * SU[j]; y <- u; y[j] <- if (u[j] + h <= hu[j]) u[j] + h else u[j] - h
      (resid(y) - r0) / (y[j] - u[j]) })
    last <<- list(x = x, r = r0, J = Jm); last
  }
  f <- nlminb(u0[free], function(x) sum(resid(full(x))^2),
              gradient = function(x) { g <- jac(x); 2 * drop(crossprod(g$J, g$r)) },
              hessian  = function(x) { g <- jac(x); 2 * crossprod(g$J) },
              lower = lu[free], upper = hu[free], scale = 1 / SU[free],
              control = list(iter.max = 600, eval.max = 1000, rel.tol = 1e-9, x.tol = 1e-8))
  z <- to_z(full(f$par)); names(z) <- pn
  list(value = f$objective, par = z)
}
# Best of a list of starts. Skips a start that fails.
fit_best <- function(starts, J, omega = OMEGA, free = 1:7) {
  fs <- lapply(starts, function(s) tryCatch(fit_ls(s, J, omega, free), error = function(e) list(value = Inf, par = s)))
  fs[[which.min(sapply(fs, `[[`, "value"))]]
}
rand_start <- function() { st <- lo + runif(7) * (hi - lo); st[1] <- runif(1, 1e-3, 0.2); st }
# Points that cover the basins of the distance: slow and fast tenure switching, a
# positive supply response, and slower or faster price adjustment with a matching eps.
basins <- function(th) list(
  th, replace(th, 7, 0.02), replace(th, 5:6, c(0.1, 1)), replace(th, 5:6, c(0.8, 0.35)),
  c(0.036, 0.37, 0.966, 0.15, 0.2, 0.7, 0), c(0.07, 0.37, 0.964, 0.10, 0.2, 0.7, 0),
  c(0.166, 0.38, 0.96, 0.05, 0.2, 0.7, 0), c(0.005, 0.38, 0.96, 0.50, 0.2, 0.7, 0),
  c(0.02, 0.38, 0.96, 0.40, 1, 0.1, 0), c(0.02, 0.38, 0.96, 0.16, 1, 0.13, 0))
fit_J <- function(J, omega = OMEGA) {
  fit_best(c(basins(c(0.012, 0.39, 0.96, 0.23, 0.18, 0.67, 0)), lapply(1:8, function(i) rand_start())), J, omega)
}

# ---- M1. Estimation ------------------------------------------------------------
Js <- c(4, 8, 12)
fits <- lapply(Js, fit_J)
prof <- data.frame(J = Js, objective = sapply(fits, `[[`, "value"), chi = sapply(fits, function(f) f$par[7]),
                   theta = sapply(fits, function(f) f$par[4]))
cat("== M1 profile over construction lag J ==\n"); print(prof)
J0 <- 8
f0 <- fits[[which(Js == J0)]]
th <- f0$par; names(th) <- pn
# Standard errors: classical minimum distance with diagonal weights,
# V = (G'WG)^{-1}; ignores correlation across horizons, so approximate.
G <- sapply(seq_along(th), function(j) {
  h <- 1e-5 * max(1, abs(th[j])); a <- th; b <- th
  a[j] <- min(hi[j], th[j] + h); b[j] <- max(lo[j], th[j] - h)
  (unlist(model_irf(a, J0)[c("p", "r", "c")]) - unlist(model_irf(b, J0)[c("p", "r", "c")])) / (a[j] - b[j])
})
V <- tryCatch(solve(t(G) %*% diag(W) %*% G), error = function(e) MASS::ginv(t(G) %*% diag(W) %*% G))
se <- sqrt(pmax(diag(V), 0)); names(se) <- pn
at_bound <- th <= lo + 1e-6 | th >= hi - 1e-6
est <- data.frame(param = pn, estimate = th, se = se, at_bound = at_bound)
mf <- model_irf(th, J0)
okh <- mf$r[2] / mf$p[1]
chi2 <- f0$value; dfree <- length(m_data) - length(th)
cat(sprintf("\n== M1 estimates (J = 8, omega = %.3f) ==\n", OMEGA)); print(est, digits = 3)
cat(sprintf("Objective %.2f on %d moments, %d parameters; p(chi2_%d) = %.3f\n",
            chi2, length(m_data), length(th), dfree, 1 - pchisq(chi2, dfree)))
cat(sprintf("Implied kappa*eta = %.3f; omega*kappa*eta = %.3f; model rent/price at h=12 = %.2f\n",
            th["kappa"] * th["eta"], OMEGA * th["kappa"] * th["eta"], mf$r[13] / mf$p[13]))
fitdat <- data.frame(h = c(0:H, 0:H, 1:H), outcome = c(rep(c("Price", "Rent"), each = H + 1), rep("Credit", H)),
                     data = m_data, se = m_se, model = c(mf$p, mf$r, mf$c))
cat(sprintf("Model credit h4 %.3f h12 %.3f; data %.3f %.3f\n", mf$c[4], mf$c[12], tc$beta[4], tc$beta[12]))
cat(sprintf("Moments inside the 90%% band of the projections: %d of %d\n",
            sum(abs(fitdat$model - fitdat$data) <= 1.645 * fitdat$se), nrow(fitdat)))
write_csv(est, file.path(OUTDIR, "model_estimates.csv"))
write_csv(prof, file.path(OUTDIR, "model_profile_J.csv"))
write_csv(fitdat, file.path(OUTDIR, "model_fit.csv"))

pal <- c(Price = "#c0392b", Rent = "#1b6f7a")
ggsave(file.path(OUTDIR, "fig_model_fit.png"),
  ggplot(fitdat |> filter(outcome != "Credit"), aes(h, colour = outcome, fill = outcome)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    geom_ribbon(aes(ymin = data - 1.645 * se, ymax = data + 1.645 * se), alpha = 0.18, colour = NA) +
    geom_line(aes(y = data), linewidth = 0.9) +
    geom_line(aes(y = model), linewidth = 0.9, linetype = "dashed") +
    geom_point(aes(y = model), size = 1.4) +
    scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
    scale_x_continuous(breaks = 0:H) +
    labs(x = "Quarters after the innovation", y = "Log points", colour = NULL, fill = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom"),
  width = 8, height = 4.6, dpi = 300)

# ---- M2. Decomposition of the wedge -------------------------------------------
dec <- function(label, omega = OMEGA, kappa = th["kappa"], chi = th["chi"]) {
  m <- model_irf(th, J0, omega = omega, chi_override = chi, kappa_override = kappa)
  data.frame(case = label, h4 = m$r[5], h8 = m$r[9], h12 = m$r[13],
             wedge12 = m$p[13] - m$r[13])
}
M2 <- rbind(dec("Estimated model"),
            dec("Leases reprice at once (omega = 1)", omega = 1),
            dec("Tenure adjusts at once (kappa = 1)", kappa = 1),
            dec("Both at once", omega = 1, kappa = 1),
            dec("No supply response (chi = 0)", chi = 0))
M2$price12 <- mf$p[13]
mt <- model_irf(th, J0, theta_override = 1)
cat(sprintf("Prices fully forward-looking (theta = 1): price h0 %.4f, h3 %.4f, h12 %.4f; rent h12 %.4f\n",
            mt$p[1], mt$p[4], mt$p[13], mt$r[13]))
# theta = 1 with the other six parameters re-estimated, from the same kinds of start.
f_t1 <- {
  free <- setdiff(1:7, 4)
  fit_best(lapply(c(basins(th), lapply(1:8, function(i) rand_start())), replace, 4, 1), J0, free = free) }
cat(sprintf("Re-estimated with theta = 1: objective %.2f (against %.2f)\n", f_t1$value, f0$value))
cat("\n== M2 decomposition (rent responses and wedge) ==\n"); print(M2, digits = 3)
write_csv(M2, file.path(OUTDIR, "model_decomposition.csv"))

# ---- M3. Counterfactual tightening ---------------------------------------------
# Credit conditions fall by x for 16 quarters (2015:Q1-2018:Q4, the APRA period),
# then return; the end is foreseen. x equals the impact change in credit
# conditions behind a one-SD credit innovation in the baseline (mf$phi[1]), so
# the tightening is the mirror image of that innovation, held for four years.
cf <- function(x, len = 16) {
  phi <- c(rep(-x, len), rep(0, TT - len))
  sol <- solve_path(phi, th["eps"], th["kappa"], th["eta"], th["chi"], J0, th["theta"])
  list(p = sol$p, r = sol$r)
}
x0 <- mf$phi[1]
cs <- cf(x0)
M3 <- data.frame(h = 0:28, price = cs$p[1:29], rent = cs$r[1:29])
cat("\n== M3 counterfactual (selected quarters) ==\n")
print(M3[M3$h %in% c(0, 4, 8, 12, 15, 16, 20, 24, 28), ], digits = 3, row.names = FALSE)
cat(sprintf("Low points: price %.4f after %d quarters; rent %.4f after %d quarters\n",
            min(M3$price), M3$h[which.min(M3$price)], min(M3$rent), M3$h[which.min(M3$rent)]))
write_csv(M3, file.path(OUTDIR, "model_counterfactual.csv"))
ggsave(file.path(OUTDIR, "fig_model_counterfactual.png"),
  ggplot(tidyr::pivot_longer(M3, -h, names_to = "outcome") |>
           mutate(outcome = factor(tools::toTitleCase(outcome), levels = c("Price", "Rent"))),
         aes(h, value, colour = outcome)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
    annotate("rect", xmin = 0, xmax = 15, ymin = -Inf, ymax = Inf, alpha = 0.08) +
    geom_line(linewidth = 1) + scale_colour_manual(values = pal) +
    scale_x_continuous(breaks = seq(0, 28, 4)) +
    labs(x = "Quarters after the tightening begins", y = "Log points", colour = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom"),
  width = 8, height = 4.2, dpi = 300)

# ---- M4. State dependence -------------------------------------------------------
# Reduced-form test (Appendix): price growth on SA credit growth, its interaction
# with the lagged cash rate (demeaned), the lagged cash rate, and the controls.
lend <- read_csv(F_LEND, show_col_types = FALSE) |>
  filter(Measure == "Value", `Loan Purpose` == "Total dwellings excluding refinancing",
         `Housing Purpose` == "Investor", `Adjustment Type` == "Seasonally Adjusted",
         Region %in% names(STATE2CITY)) |>
  mutate(city = recode(Region, !!!STATE2CITY), quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)),
         v = as.numeric(OBS_VALUE)) |> distinct(city, quarter, .keep_all = TRUE) |>
  select(city, quarter, v)
Lg <- function(x, k = 1) dplyr::lag(x, k)
d <- readRDS(file.path(CLEAN, "panel_combined.rds")) |> left_join(lend, by = c("city", "quarter")) |>
  arrange(city, quarter) |> group_by(city) |>
  mutate(dp = log(prop_price) - Lg(log(prop_price)), di = log(v) - Lg(log(v)),
         dc = cash_rate - Lg(cash_rate), cash_l1 = Lg(cash_rate),
         dw = log(wpi) - Lg(log(wpi)), tight_l1 = Lg(tightness)) |> ungroup() |>
  filter(is.finite(tight_l1), is.finite(dp), is.finite(di), is.finite(dw))
cbar <- mean(d$cash_l1); d$cdev <- d$cash_l1 - cbar
dkv <- function(m) plm::vcovSCC(m, type = "HC0", maxlag = 4)
m4 <- plm(dp ~ di + di:cdev + cdev + dc + tight_l1 + dw, data = pdata.frame(d, index = c("city", "quarter")),
          model = "within")
b4 <- coef(m4); s4 <- sqrt(diag(dkv(m4)))
cat(sprintf("\n== M4 reduced form: slope at mean cash rate %.3f (s.e. %.3f); interaction %.4f (s.e. %.4f); mean cash rate %.2f ==\n",
            b4["di"], s4["di"], b4["di:cdev"], s4["di:cdev"], cbar))
# Model prediction: eps = lam*phi/(1-lam*phi), lam = 1 - Lambda(1+i), i quarterly.
# The reduced-form slope is proportional to eps, so predicted slope(i) =
# slope_at_mean * eps(i)/eps(mean), and the predicted interaction is its
# derivative at the mean, per percentage point of the annual cash rate.
phibar <- 0.8
epsf <- function(cash_annual, Lambda) { lam <- 1 - Lambda * (1 + cash_annual / 400); lam * phibar / (1 - lam * phibar) }
M4 <- bind_rows(lapply(c(0.10, 0.06, 0.04, 0.03), function(rho_annual) {
  Lambda <- 1 / (1 + rho_annual / 4)
  if (Lambda * (1 + 4.35 / 400) >= 1) return(NULL)
  s_lo <- b4["di"] * epsf(0.1, Lambda) / epsf(cbar, Lambda)
  s_hi <- b4["di"] * epsf(4.35, Lambda) / epsf(cbar, Lambda)
  dd <- 1e-4
  inter <- b4["di"] * (epsf(cbar + dd, Lambda) - epsf(cbar - dd, Lambda)) / (2 * dd) / epsf(cbar, Lambda)
  data.frame(discount_annual = rho_annual, ratio_low_high = epsf(0.1, Lambda) / epsf(4.35, Lambda),
             slope_at_0.1 = s_lo, slope_at_4.35 = s_hi, predicted_interaction = inter,
             z_vs_estimate = (inter - b4["di:cdev"]) / s4["di:cdev"])
}))
cat("Data: interaction 90% CI [", round(b4["di:cdev"] - 1.645 * s4["di:cdev"], 4), ",",
    round(b4["di:cdev"] + 1.645 * s4["di:cdev"], 4), "]\n")
print(M4, digits = 3, row.names = FALSE)
# Annual discount rate at which the predicted interaction reaches the lower end
# of the 90% CI; above this rate it lies inside the interval.
gap <- function(rho) {
  Lambda <- 1 / (1 + rho / 4); dd <- 1e-4
  unname(b4["di"] * (epsf(cbar + dd, Lambda) - epsf(cbar - dd, Lambda)) / (2 * dd) / epsf(cbar, Lambda) -
         (b4["di:cdev"] - 1.645 * s4["di:cdev"]))
}
rho_min <- uniroot(gap, c(0.05, 0.30), tol = 1e-8)$root
cat(sprintf("Predicted interaction inside the 90%% CI once the annual discount rate exceeds %.3f\n", rho_min))
write_csv(M4, file.path(OUTDIR, "model_state_dependence.csv"))
capture.output({print(est); print(prof); print(M2); print(M4); cat("rho_min", rho_min, "\n");
  cat("interaction", b4["di:cdev"], s4["di:cdev"], "slope", b4["di"], s4["di"], "cbar", cbar, "\n");
  cat("chi2", chi2, "df", dfree, "theta1", f_t1$value, "\n"); cat("x0", x0, "\n")},
  file = file.path(OUTDIR, "model_summary.txt"))
cat("\nDone.\n")
