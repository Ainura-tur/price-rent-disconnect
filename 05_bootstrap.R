# =============================================================================
# 05_bootstrap.R -- moving-block bootstrap of the structural estimates
#
# Each draw resamples quarters in blocks of BLOCK consecutive quarters, keeping
# the five cities in each quarter together (as for Table D.1), then:
#   1. re-estimates the price, rent and credit local projections (38 moments);
#   2. re-estimates the dynamic rent equation, giving omega for the draw;
#   3. re-estimates the model by impulse-response matching, with the full-sample
#      weights, from the full-sample estimates and the other starts in fit_draw();
#   4. recomputes the model impulse responses, the decomposition of the
#      twelve-quarter wedge, and the counterfactual tightening.
#
# Usage: Rscript 05_bootstrap.R <block> <first draw> <last draw>
# Writes output/boot_b<block>_<first>_<last>.rds; 05b_bootstrap_summary.R
# combines the pieces.
# =============================================================================

args  <- commandArgs(trailingOnly = TRUE)
BLOCK <- if (length(args) >= 1) as.integer(args[1]) else 8L
FIRST <- if (length(args) >= 2) as.integer(args[2]) else 1L
LAST  <- if (length(args) >= 3) as.integer(args[3]) else 2L

source("00_setup.R")
suppressPackageStartupMessages({ library(parallel) })

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

L <- function(x, k = 1) dplyr::lag(x, k)
H <- 12; TT <- 200; VARPI <- 0.99; DELTA_K <- 0.01; SIGMA <- 0.1006; J0 <- 8

# ---- Panel with all leads and lags (built once, cached) ------------------------
cache <- file.path(OUTDIR, "boot_panel.rds")
if (!file.exists(cache)) {
  lend <- read_csv(F_LEND, show_col_types = FALSE) |>
    filter(Measure == "Value", `Loan Purpose` == "Total dwellings excluding refinancing",
           `Housing Purpose` == "Investor", `Adjustment Type` == "Seasonally Adjusted",
           Region %in% names(STATE2CITY)) |>
    mutate(city = recode(Region, !!!STATE2CITY), quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)),
           inv_sa = as.numeric(OBS_VALUE)) |>
    distinct(city, quarter, .keep_all = TRUE) |> select(city, quarter, inv_sa)
  d <- readRDS(file.path(CLEAN, "panel_combined.rds")) |> left_join(lend, by = c("city", "quarter")) |>
    arrange(city, quarter) |> group_by(city) |>
    mutate(l_price = log(prop_price), l_rent = log(rents), l_inv = log(inv_sa),
           dp = l_price - L(l_price), dr = l_rent - L(l_rent), di = l_inv - L(l_inv),
           dw = log(wpi) - L(log(wpi)), dn = log(employment) - L(log(employment)),
           dc = cash_rate - L(cash_rate), dc_l1 = L(dc), tight_l1 = L(tightness),
           gap_l1 = L(log(price_rent)), dr_l1 = L(dr)) |> ungroup()
  for (j in 1:4) d <- d |> group_by(city) |>
    mutate("di_l{j}" := L(di, j), "dp_l{j}" := L(dp, j), "dr_l{j}" := L(dr, j)) |> ungroup()
  for (h in 0:H) d <- d |> group_by(city) |>
    mutate("yp_{h}" := dplyr::lead(l_price, h) - L(l_price),
           "yr_{h}" := dplyr::lead(l_rent, h) - L(l_rent),
           "yc_{h}" := dplyr::lead(l_inv, h) - L(l_inv)) |> ungroup()
  d <- d |> filter(is.finite(tight_l1), is.finite(dr), is.finite(dp), is.finite(dw), is.finite(di)) |>
    mutate(qid = as.integer(factor(quarter)))
  saveRDS(d, cache)
}
d <- readRDS(cache)
qs <- sort(unique(d$qid)); nq <- length(qs)
city_id <- as.integer(factor(d$city))
SD_SH <- sd(d$di)   # shock size fixed at the full-sample value

# Within (city fixed-effects) OLS coefficient on the first regressor.
within_coef <- function(y, X, g) {
  ok <- is.finite(y) & rowSums(!is.finite(X)) == 0
  y <- y[ok]; X <- X[ok, , drop = FALSE]; g <- g[ok]
  dm <- function(v) v - ave(v, g)
  b <- lm.fit(apply(X, 2, dm), dm(y))$coefficients
  b
}
ctrl <- function(own) c(paste0("di_l", 1:4), paste0(own, "_l", 1:4), "tight_l1", "dc_l1")
moments <- function(rows) {
  dd <- d[rows, ]; g <- paste(dd$city, rep(seq_along(rows)) * 0)  # city fixed effects
  g <- city_id[rows]
  lp1 <- function(y, own, h) {
    X <- as.matrix(dd[, c("di", ctrl(own))])
    within_coef(dd[[paste0(y, "_", h)]], X, g)[1] * SD_SH
  }
  p <- sapply(0:H, function(h) lp1("yp", "dp", h))
  r <- sapply(0:H, function(h) lp1("yr", "dr", h))
  cr <- sapply(1:H, function(h) lp1("yc", "dp", h))
  b <- within_coef(dd$dr, as.matrix(dd[, c("dr_l1", "gap_l1", "tight_l1", "dw", "dn")]), g)
  list(m = c(p, r, cr), omega = 1 - b[1])
}

# ---- Model (as in 04_model_matching.R) -----------------------------------------
solve_path <- function(phi, eps, kappa, eta, chi, J, theta, omega) {
  n <- length(phi); j <- 0:(n - 1)
  a <- c(0, kappa * eta * (1 - kappa)^(j[-n]))
  bb <- ifelse(j >= J, chi * (1 - DELTA_K)^(j - J), 0)
  dker <- as.numeric(stats::filter(a - bb, 1 - omega, method = "recursive")) * omega
  low <- function(k) { M <- matrix(0, n, n); idx <- row(M) - col(M); Lw <- idx >= 0; M[Lw] <- k[idx[Lw] + 1]; M }
  R <- low(dker); Th <- low(theta * (1 - theta)^j)
  B <- VARPI * diag(n) + (1 - VARPI) * (R %*% Th)
  A <- diag(n); A[1:(n - 1), ] <- A[1:(n - 1), ] - B[2:n, ]
  v <- solve(A, eps * phi); p <- Th %*% v
  list(p = as.numeric(p), r = as.numeric(R %*% p))
}
credit_path <- function(rho_g, rho_d, n = TT) {
  g <- rho_g^(0:(n - 1)); phi <- numeric(n)
  for (t in 1:n) phi[t] <- (if (t > 1) rho_d * phi[t - 1] else 0) + g[t]
  phi
}
model_irf <- function(th, omega, kappa = th[5], chi = th[7]) {
  phi <- credit_path(th[2], th[3])
  sol <- solve_path(phi, th[1], kappa, th[6], chi, J0, th[4], omega)
  sc <- SIGMA / (phi[1] + sol$p[1])
  list(p = sol$p[1:(H + 1)] * sc, r = sol$r[1:(H + 1)] * sc,
       c = ((phi + sol$p) * sc)[2:(H + 1)], phi0 = phi[1] * sc)
}
# Bounds as in 04_model_matching.R, which says why eta stops at 1.
lo <- c(1e-5, 0.0, 0.50, 0.02, 0.02, 0.0, 0.0)
hi <- c(1.0, 0.95, 0.995, 1.0, 1.0, 1.0, 0.05)
pn <- c("eps", "rho_g", "rho_d", "theta", "kappa", "eta", "chi")
# Search variables as in 04_model_matching.R: logs of eps, theta and kappa.
LOGP <- c(1, 4, 5); SU <- c(1, 0.4, 0.2, 1, 1, 0.7, 0.01)
to_u <- function(z) { z[LOGP] <- log(z[LOGP]); z }
to_z <- function(u) { u[LOGP] <- exp(u[LOGP]); u }

# Full-sample targets, weights and estimates
lp_full <- read_csv(file.path(OUTDIR, "sa_C_lp.csv"), show_col_types = FALSE)
se_full <- c(lp_full$se[lp_full$spec == "Price, baseline"], lp_full$se[lp_full$spec == "Rent, baseline"],
             lp_full$se[lp_full$spec == "Credit, baseline" & lp_full$h >= 1])
W <- 1 / se_full^2
th0 <- read_csv(file.path(OUTDIR, "model_estimates.csv"), show_col_types = FALSE)$estimate

# Bounded Gauss-Newton steps, as fit_ls() in 04_model_matching.R, for the draw's
# moments m. W stays at the full-sample weights.
fit_ls <- function(st, m, omega) {
  u0 <- to_u(st); lu <- to_u(lo); hu <- to_u(hi)
  resid <- function(u) { mm <- model_irf(to_z(u), omega); sqrt(W) * (c(mm$p, mm$r, mm$c) - m) }
  last  <- NULL
  jac <- function(u) {
    if (!is.null(last) && identical(last$x, u)) return(last)
    r0 <- resid(u)
    Jm <- sapply(1:7, function(j) {
      h <- 1e-5 * SU[j]; y <- u; y[j] <- if (u[j] + h <= hu[j]) u[j] + h else u[j] - h
      (resid(y) - r0) / (y[j] - u[j]) })
    last <<- list(x = u, r = r0, J = Jm); last
  }
  f <- nlminb(u0, function(u) sum(resid(u)^2),
              gradient = function(u) { g <- jac(u); 2 * drop(crossprod(g$J, g$r)) },
              hessian  = function(u) { g <- jac(u); 2 * crossprod(g$J) },
              lower = lu, upper = hu, scale = 1 / SU,
              control = list(iter.max = 600, eval.max = 1000, rel.tol = 1e-9, x.tol = 1e-8))
  list(value = f$objective, par = to_z(f$par))
}
# Starts: the estimates and the points in basins() of 04_model_matching.R, which
# cover the basins of the distance.
fit_draw <- function(m, omega) {
  starts <- list(th0, replace(th0, 7, 0.02), replace(th0, 5:6, c(0.1, 1)), replace(th0, 5:6, c(0.8, 0.35)),
                 c(0.036, 0.37, 0.966, 0.15, 0.2, 0.7, 0), c(0.07, 0.37, 0.964, 0.10, 0.2, 0.7, 0),
                 c(0.166, 0.38, 0.96, 0.05, 0.2, 0.7, 0), c(0.005, 0.38, 0.96, 0.50, 0.2, 0.7, 0),
                 c(0.02, 0.38, 0.96, 0.40, 1, 0.1, 0), c(0.02, 0.38, 0.96, 0.16, 1, 0.13, 0))
  fs <- lapply(starts, function(s) tryCatch(fit_ls(s, m, omega), error = function(e) list(value = Inf, par = s)))
  fs[[which.min(sapply(fs, `[[`, "value"))]]
}

# Counterfactual: credit conditions fall for 16 quarters by the impact change
# behind a one-SD innovation, then return.
counterfactual <- function(th, omega) {
  x <- model_irf(th, omega)$phi0
  phi <- c(rep(-x, 16), rep(0, TT - 16))
  sol <- solve_path(phi, th[1], th[5], th[6], th[7], J0, th[4], omega)
  c(sol$p[1:29], sol$r[1:29])
}

one_draw <- function(b) {
  set.seed(100000 + BLOCK * 1000 + b)
  nb <- ceiling(nq / BLOCK)
  starts <- sample(1:(nq - BLOCK + 1), nb, replace = TRUE)
  qsel <- unlist(lapply(starts, function(a) qs[a:(a + BLOCK - 1)]))[1:nq]
  rows <- unlist(lapply(qsel, function(q) which(d$qid == q)))
  mo <- moments(rows)
  f <- fit_draw(mo$m, mo$omega)
  th <- f$par
  mi <- model_irf(th, mo$omega)
  wedge <- function(mm) mm$p[13] - mm$r[13]
  w0 <- wedge(mi)
  w_om <- wedge(model_irf(th, 1)); w_ka <- wedge(model_irf(th, mo$omega, kappa = 1))
  c(setNames(th, pn), omega = mo$omega, objective = f$value,
    share_tenure = 1 - w_ka / w0, share_lease = 1 - w_om / w0, wedge12 = w0,
    rent_price_12 = mi$r[13] / mi$p[13],
    setNames(mi$p, paste0("mp", 0:H)), setNames(mi$r, paste0("mr", 0:H)),
    setNames(counterfactual(th, mo$omega), c(paste0("cfp", 0:28), paste0("cfr", 0:28))),
    setNames(mo$m, paste0("data", seq_along(mo$m))))
}

t0 <- Sys.time()
# Windows cannot fork, so the draws run one at a time there. Each draw sets its
# own seed, so the results do not depend on the number of cores.
res <- mclapply(FIRST:LAST, function(b) tryCatch(one_draw(b), error = function(e) NULL),
                mc.cores = if (.Platform$OS.type == "windows") 1L else detectCores())
res <- do.call(rbind, res[!sapply(res, is.null)])
saveRDS(res, file.path(OUTDIR, sprintf("boot_b%d_%d_%d.rds", BLOCK, FIRST, LAST)))
cat(sprintf("block %d, draws %d-%d: %d done in %.1f min\n", BLOCK, FIRST, LAST, nrow(res),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
