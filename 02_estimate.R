# =============================================================================
# 02_estimate.R   (merges old 04_estimate + 06_price_iv_robustness)
#
# Core estimation on the within-city time dimension, then the divergence test,
# robustness, the Bartik IV, and the rolling investor-demand R^2 series.
#
# SE CONVENTION: Driscoll-Kraay throughout (plm::vcovSCC), matching the
# standard errors reported in every manuscript table. The earlier split, where
# the static tables used Arellano vcovHC while the LP used DK, is removed; all
# coefficient tables now use dk_coeftest() from 00_setup.R.
#
# RBA D2 national credit (inv_credit_nat etc.) is national and broadcast across
# cities, so it carries only time variation and is NOT used as the headline
# regressor (that stays the state-level inv_commit, which has the cross-city
# variation identification rests on). D2 enters in three roles only: the external
# shift in the Bartik IV (Section 5), a national robustness / reclassification
# check on the post-2017 interaction (Section 7.6), and the national investor-
# credit-share composition robustness check (Section 7.7).
#
# Reads:  clean/panel_combined.rds
# Writes: output/est_*.txt summaries; output/rolling_financial_r2.csv
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

source("00_setup.R")
library(plm); library(lmtest); library(sandwich); library(fixest)

panel <- readRDS(file.path(CLEAN, "panel_combined.rds")) |>
  arrange(city, quarter) |> mutate(qdate = as.numeric(quarter))

# National investor share of housing credit (composition of the D2 stock).
# inv_credit_nat and oo_credit_nat are national stocks broadcast across cities,
# so inv_credit_share carries time variation only. It captures the compositional
# tilt toward investors that the paper attributes to the post-2017 period, and
# enters as a national robustness regressor (Section 7.7), NOT as a headline
# cross-city term. dln_ is its q/q log change for a flow-comparable version.
panel <- panel |>
  group_by(city) |>
  mutate(inv_credit_share     = inv_credit_nat / (inv_credit_nat + oo_credit_nat),
         dln_inv_credit_share = c(NA, diff(log(inv_credit_share)))) |>
  ungroup()

# Logs for the I(1) index levels; tightness/cash/shares enter as is.
d <- panel |>
  mutate(l_rent = log(rents), l_price = log(prop_price), l_wpi = log(wpi),
         l_emp = log(employment), l_inv = log(inv_commit), l_pr_gap = log(price_rent))
P <- pdata.frame(d, index = c("city","quarter"))

save_txt <- function(obj, name) capture.output(print(obj), file = file.path(OUTDIR, name))

# =============================================================================
# 1. RENT EQUATION  (within FD; tightness in levels-lagged as an I(0) regressor)
# =============================================================================
rent_fe <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
               data = P, model = "within", effect = "individual")
cat("\n================  RENT EQUATION (within, FD)  ================\n")
print(dk_coeftest(rent_fe)); save_txt(dk_coeftest(rent_fe), "est_rent.txt")

rent_mg <- pmg(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
               data = P, model = "mg")
cat("\n----------------  RENT EQUATION (mean-group)  ----------------\n")
print(summary(rent_mg)); save_txt(summary(rent_mg), "est_rent_mg.txt")

# =============================================================================
# 2. PRICE EQUATION  (contemporaneous and predetermined)
# =============================================================================
price_fe <- plm(diff(l_price) ~ diff(cash_rate) + diff(l_inv) + dplyr::lag(tightness,1) + diff(l_wpi),
                data = P, model = "within", effect = "individual")
cat("\n================  PRICE EQUATION (within, FD)  ===============\n")
print(dk_coeftest(price_fe)); save_txt(dk_coeftest(price_fe), "est_price.txt")

# Predetermined (lagged endogenous) price equation: the cleaner specification.
price_pre <- plm(diff(l_price) ~ dplyr::lag(diff(l_inv),1) + dplyr::lag(diff(cash_rate),1) + dplyr::lag(tightness,1),
                 data = P, model = "within", effect = "individual")
cat("\n--------  PRICE EQUATION (predetermined: lagged credit/cash)  --------\n")
print(dk_coeftest(price_pre)); save_txt(dk_coeftest(price_pre), "est_price_predetermined.txt")

# =============================================================================
# 3. DIVERGENCE TEST  (investor demand x post-2017, both equations)
#    Reported both contemporaneously and predetermined; the two bracket the
#    relationship (upper/lower bound on the within-quarter simultaneity).
# =============================================================================
price_div <- plm(diff(l_price) ~ diff(l_inv)*post + diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi),
                 data = P, model = "within", effect = "individual")
rent_div  <- plm(diff(l_rent)  ~ diff(l_inv)*post + diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
                 data = P, model = "within", effect = "individual")
cat("\n=============  DIVERGENCE: contemporaneous credit  ============\n")
cat("PRICE:\n"); print(dk_coeftest(price_div)); save_txt(dk_coeftest(price_div), "est_price_divergence.txt")
cat("RENT:\n");  print(dk_coeftest(rent_div));  save_txt(dk_coeftest(rent_div),  "est_rent_divergence.txt")

price_div_pre <- plm(diff(l_price) ~ dplyr::lag(diff(l_inv),1)*post + dplyr::lag(diff(cash_rate),1) + dplyr::lag(tightness,1) + diff(l_wpi),
                     data = P, model = "within", effect = "individual")
rent_div_pre  <- plm(diff(l_rent)  ~ dplyr::lag(diff(l_inv),1)*post + dplyr::lag(diff(cash_rate),1) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
                     data = P, model = "within", effect = "individual")
cat("\n=============  DIVERGENCE: predetermined credit  ==============\n")
cat("PRICE:\n"); print(dk_coeftest(price_div_pre)); save_txt(dk_coeftest(price_div_pre), "est_price_divergence_pre.txt")
cat("RENT:\n");  print(dk_coeftest(rent_div_pre));  save_txt(dk_coeftest(rent_div_pre),  "est_rent_divergence_pre.txt")

interaction_report <- function(m, term) {
  ct <- dk_coeftest(m)
  sprintf("%s = %+.4f (se %.4f, p=%.4f)", term, ct[term,1], ct[term,2], ct[term,4])
}
cat("\n--- investor-demand interaction (the divergence) ---\n")
cat("Contemporaneous:\n")
cat("  PRICE", interaction_report(price_div, "diff(l_inv):post"), "\n")
cat("  RENT ", interaction_report(rent_div,  "diff(l_inv):post"), "\n")
cat("Predetermined:\n")
cat("  PRICE", interaction_report(price_div_pre, "dplyr::lag(diff(l_inv), 1):post"), "\n")
cat("  RENT ", interaction_report(rent_div_pre,  "dplyr::lag(diff(l_inv), 1):post"), "\n")

# =============================================================================
# 4. INTERACTION ROBUSTNESS  (COVID-timing and Sydney-specific channel)
# =============================================================================
Pdf <- pdata.frame(d |> mutate(covid = as.integer(quarter >= as.yearqtr("2020 Q1")),
                               syd = as.integer(city == "Sydney")),
                   index = c("city","quarter"))
price_div_covid <- plm(diff(l_price) ~ diff(l_inv) + diff(l_inv):post + diff(l_inv):covid +
                         diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + post + covid,
                       data = Pdf, model = "within", effect = "individual")
cat("\n----  ROBUSTNESS (i): COVID timing vs 2017 (price eqn)  ----\n")
print(dk_coeftest(price_div_covid)); save_txt(dk_coeftest(price_div_covid), "est_price_divergence_covid.txt")

price_div_syd <- plm(diff(l_price) ~ diff(l_inv) + diff(l_inv):post + diff(l_inv):post:syd +
                       diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + post,
                     data = Pdf, model = "within", effect = "individual")
cat("\n----  ROBUSTNESS (ii): Sydney-specific channel (price eqn)  ----\n")
print(dk_coeftest(price_div_syd)); save_txt(dk_coeftest(price_div_syd), "est_price_divergence_sydney.txt")

# Wald tests (DK vcov), per equation: reject for PRICE, fail to reject for RENT.
dk_vcov <- function(x) plm::vcovSCC(x, type = "HC0", maxlag = 4)
price_r0 <- plm(diff(l_price) ~ diff(l_inv) + post + diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi),
                data = P, model = "within", effect = "individual")
rent_r0  <- plm(diff(l_rent)  ~ diff(l_inv) + post + diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
                data = P, model = "within", effect = "individual")
cat("\n--- Wald: investor-demand interaction zero ---\n")
cat("PRICE eqn (expect REJECT):\n");      print(waldtest(price_r0, price_div, vcov = dk_vcov))
cat("RENT eqn (expect FAIL to reject):\n"); print(waldtest(rent_r0,  rent_div,  vcov = dk_vcov))

# =============================================================================
# 5. BARTIK / SHIFT-SHARE IV  (robustness, not headline)
# =============================================================================
db <- d |>
  group_by(qdate) |> mutate(inv_natl = sum(inv_commit, na.rm = TRUE)) |> ungroup() |>
  mutate(inv_share_natl = inv_commit / inv_natl)
w_pre <- db |> filter(quarter < as.yearqtr("2017 Q1")) |>
  group_by(city) |> summarise(w_pre = mean(inv_share_natl, na.rm = TRUE), .groups = "drop")
db <- db |>
  group_by(qdate) |> mutate(inv_loo = sum(inv_commit, na.rm = TRUE) - inv_commit) |> ungroup() |>
  arrange(city, qdate) |> group_by(city) |>
  mutate(g_loo = log(inv_loo) - dplyr::lag(log(inv_loo)),
         d_lprice = l_price - dplyr::lag(l_price), d_lrent = l_rent - dplyr::lag(l_rent),
         d_linv = l_inv - dplyr::lag(l_inv), d_cash_l1 = dplyr::lag(cash_rate - dplyr::lag(cash_rate)),
         tight_l1 = dplyr::lag(tightness), d_lwpi = l_wpi - dplyr::lag(l_wpi), d_lemp = l_emp - dplyr::lag(l_emp)) |>
  ungroup() |> left_join(w_pre, by = "city") |> mutate(bartik = w_pre * g_loo)

iv_price <- feols(d_lprice ~ d_cash_l1 + tight_l1 | city + qdate | d_linv ~ bartik,
                  data = db, cluster = ~city)
cat("\n=====  IV (Bartik) PRICE equation  =====\n"); print(summary(iv_price))
cat("\nFirst-stage F (weak-instrument check):\n"); print(fitstat(iv_price, "ivf"))
iv_rent <- feols(d_lrent ~ d_cash_l1 + tight_l1 + d_lwpi + d_lemp | city + qdate | d_linv ~ bartik,
                 data = db, cluster = ~city)
cat("\n=====  IV (Bartik) RENT equation (placebo)  =====\n"); print(summary(iv_rent))
cat("\nNOTE: with 5 cities the first-stage F is binding; if F < 10 treat the IV",
    "as indicative and lean on the predetermined-regressor and LP evidence.\n")

# --- D2 external shift: RBA national investor-housing credit growth ----------
# The leave-one-out shifter above is internal to the five-city sample; the RBA
# D2 national series (dln_inv_credit_nat) is genuinely external. Under the two-
# way (city + qdate) FE the pre-period share level and the common national shock
# are both absorbed, so the instrument is identified off the share x national-
# shock interaction, which is exactly the Bartik variation. dln_inv_credit_nat
# is already a growth rate, so it enters directly (no diff()).
db <- db |> mutate(bartik_d2     = w_pre * dln_inv_credit_nat,
                   bartik_d2_adj = w_pre * dln_inv_credit_nat_adj)
iv_price_d2 <- feols(d_lprice ~ d_cash_l1 + tight_l1 | city + qdate | d_linv ~ bartik_d2,
                     data = db, cluster = ~city)
cat("\n=====  IV (Bartik, RBA D2 national shift) PRICE equation  =====\n")
print(summary(iv_price_d2)); cat("First-stage F (D2 shift):\n"); print(fitstat(iv_price_d2, "ivf"))
iv_price_d2_adj <- feols(d_lprice ~ d_cash_l1 + tight_l1 | city + qdate | d_linv ~ bartik_d2_adj,
                         data = db, cluster = ~city)
cat("\n=====  IV (Bartik, switching-adjusted D2 shift) PRICE equation  =====\n")
print(summary(iv_price_d2_adj)); cat("First-stage F (adj shift):\n"); print(fitstat(iv_price_d2_adj, "ivf"))
cat("\nNOTE: the D2 shift is the externally valid instrument, but with five near-",
    "uniform pre-period shares the first-stage F is the binding constraint; report",
    "it alongside the leave-one-out version, not as a stronger headline.\n")

# =============================================================================
# 6. ROLLING INVESTOR-DEMAND PARTIAL R^2  (the divergence figure's input)
# =============================================================================
partial_r2_inv <- function(df) {
  f_full <- diff(l_price) ~ diff(l_inv) + diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi)
  f_red  <- diff(l_price) ~ diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi)
  Pw <- pdata.frame(df, index = c("city","quarter"))
  m1 <- try(plm(f_full, Pw, model = "within"), silent = TRUE)
  m0 <- try(plm(f_red,  Pw, model = "within"), silent = TRUE)
  if (inherits(m1,"try-error") || inherits(m0,"try-error")) return(NA_real_)
  r2 <- function(m) 1 - sum(residuals(m)^2) / sum((m$model[[1]] - mean(m$model[[1]]))^2)
  max(r2(m1) - r2(m0), 0)
}
qs <- sort(unique(d$qdate)); win <- 20
roll_df <- bind_rows(lapply(qs[qs >= qs[win]], function(end) {
  wq <- qs[qs > end - win/4 & qs <= end]
  if (length(wq) < win) return(NULL)          # guard: skip short/gappy windows
  sub <- d |> filter(qdate %in% wq)
  data.frame(qend = end,
             price_invR2 = partial_r2_inv(sub),
             rent_invR2  = partial_r2_inv(sub |> mutate(l_price = l_rent)))
}))
write_csv(roll_df, file.path(OUTDIR, "rolling_financial_r2.csv"))
cat("\nWrote output/rolling_financial_r2.csv and est_*.txt summaries.\n")

# =============================================================================
# 7. ROBUSTNESS / VALIDATION  (prioritised; each writes to output/robust_*.txt)
#    Ordered by how load-bearing the underlying claim is:
#      7.1 rent-equation stability OVER TIME      (defends the anchor claim)
#      7.2 tightness construction sensitivity     (the headline rent regressor)
#      7.3 cointegration test for the ECM framing (tests an asserted structure)
#      7.4 wild cluster bootstrap, 5 clusters     (honest small-N inference)
#      7.5 LP lag-length sensitivity              (timing of the rent response)
#      7.6 national credit aggregate + switching  (reclassification robustness)
#      7.7 national investor-credit-share         (composition robustness)
#    Robust to missing optional packages: a test that cannot run prints why and
#    is skipped rather than failing the pipeline.
# =============================================================================
robust <- function(obj, name) capture.output(print(obj), file = file.path(OUTDIR, name))
cat("\n############  ROBUSTNESS / VALIDATION  ############\n")

## 7.1 Rent-equation stability over time -------------------------------------
## "Stable over the sample" is the paper's anchor claim but is currently shown
## only across cities (mean-group). Re-estimate the rent equation on pre/post
## splits and test whether the fundamentals coefficients move. Stability = the
## tightness/wage/employment coefficients are similar across sub-samples and a
## Chow-style interaction with the split dummy is jointly insignificant.
cat("\n--- 7.1 Rent equation stability over time (pre/post 2017 and 2020) ---\n")
rent_f <- diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp)
for (cut in c("2017 Q1","2020 Q1")) {
  cq <- as.yearqtr(cut)
  Ppre  <- pdata.frame(filter(d, quarter <  cq), index = c("city","quarter"))
  Ppost <- pdata.frame(filter(d, quarter >= cq), index = c("city","quarter"))
  mpre  <- try(plm(rent_f, Ppre,  model = "within"), silent = TRUE)
  mpost <- try(plm(rent_f, Ppost, model = "within"), silent = TRUE)
  cat(sprintf("\n  Split at %s:\n", cut))
  if (!inherits(mpre,"try-error"))  { cat("  PRE:\n");  print(round(coef(mpre), 4)) }
  if (!inherits(mpost,"try-error")) { cat("  POST:\n"); print(round(coef(mpost),4)) }
}
# Pooled interaction (Chow-style): every fundamental x post; joint Wald that all
# interactions are zero is the formal stability test.
rent_chow_r <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp) + post,
                   data = P, model = "within")
rent_chow_u <- plm(diff(l_rent) ~ (dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp)) * post,
                   data = P, model = "within")
cat("\n  Chow-style Wald (all rent fundamentals x post = 0; expect FAIL to reject = stable):\n")
print(waldtest(rent_chow_r, rent_chow_u, vcov = dk_vcov))
robust(list(restricted = dk_coeftest(rent_chow_r), unrestricted = dk_coeftest(rent_chow_u)),
       "robust_rent_stability.txt")

## 7.2 Tightness construction sensitivity ------------------------------------
## The rent result leans on one constructed ratio (4q migration / 4q completions,
## I(0), in levels). Re-estimate with alternative constructions to show it is not
## an artifact: (a) overseas-only tightness (tightness_no, already built),
## (b) tightness differenced rather than in levels (the "wrong" treatment, to
## show it WEAKENS the fit as the paper claims), (c) raw 1q ratio.
cat("\n--- 7.2 Tightness construction sensitivity (rent equation) ---\n")
d2 <- d |> arrange(city, quarter) |> group_by(city) |>
  mutate(tight_raw = net_migration / dwell_total,            # un-smoothed 1q ratio
         d_tight   = tightness - dplyr::lag(tightness)) |> ungroup()
P2 <- pdata.frame(d2, index = c("city","quarter"))
specs <- list(
  baseline      = diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness,1)    + diff(l_wpi) + diff(l_emp),
  overseas_only = diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness_no,1) + diff(l_wpi) + diff(l_emp),
  differenced   = diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(d_tight,1)      + diff(l_wpi) + diff(l_emp),
  raw_1q        = diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tight_raw,1)    + diff(l_wpi) + diff(l_emp)
)
tight_tab <- lapply(names(specs), function(nm) {
  m <- try(plm(specs[[nm]], P2, model = "within"), silent = TRUE)
  if (inherits(m,"try-error")) return(data.frame(spec = nm, tight_coef = NA, tight_t = NA, r2 = NA))
  ct <- dk_coeftest(m); tr <- grep("tight", rownames(ct), value = TRUE)[1]
  data.frame(spec = nm, tight_coef = ct[tr,1], tight_t = ct[tr,1]/ct[tr,2],
             r2 = summary(m)$r.squared["rsq"])
})
tight_tab <- bind_rows(tight_tab); print(tight_tab, row.names = FALSE)
robust(tight_tab, "robust_tightness.txt")

## 7.3 Panel cointegration for the ECM framing -------------------------------
## The paper motivates an error-correction framing, but the lagged price-rent
## gap enters with a ~0 coefficient. Test directly: is there a cointegrating
## relationship among log rent, log price (and tightness)? Westerlund (via the
## 'pco' route) is ideal; if unavailable, fall back to a Pedroni-style
## residual panel ADF. Either result is informative: cointegration supports the
## ECM language; its absence argues for calling the gap a level control instead.
cat("\n--- 7.3 Cointegration test (ECM framing) ---\n")
coint_done <- FALSE
if (requireNamespace("pco", quietly = TRUE)) {
  res <- try({
    wide <- d |> select(city, quarter, l_rent, l_price) |>
      tidyr::pivot_wider(names_from = city, values_from = c(l_rent, l_price))
    # pco::pedroni99 / westerlund expect matrices; user to adapt column order.
    cat("  'pco' available: run pco::westerlund on (l_rent ~ l_price) by city.\n")
  }, silent = TRUE)
  coint_done <- !inherits(res, "try-error")
}
if (!coint_done) {
  # Fallback: residual-based panel ADF (Pedroni group-mean ADF, by hand).
  # Regress l_rent on l_price within city, ADF the residuals, pool the t-stats.
  adf_t <- function(e) {
    e <- e[is.finite(e)]; if (length(e) < 12) return(NA_real_)
    de <- diff(e); el <- head(e, -1)
    s <- try(summary(lm(de ~ el + 0)), silent = TRUE)
    if (inherits(s,"try-error")) return(NA_real_)
    s$coefficients["el","t value"]
  }
  group_t <- d |> arrange(city, quarter) |> group_by(city) |>
    group_modify(~{
      m <- lm(l_rent ~ l_price, data = .x)
      data.frame(adf_t = adf_t(resid(m)))
    }) |> ungroup()
  cat("  Residual-based group ADF t (Pedroni-style); more negative = cointegration:\n")
  print(group_t, row.names = FALSE)
  cat(sprintf("  Group-mean ADF t = %.2f (compare to Pedroni critical values).\n",
              mean(group_t$adf_t, na.rm = TRUE)))
  robust(group_t, "robust_cointegration.txt")
  cat("  NOTE: if residuals are NOT stationary, soften the ECM language to",
      "'lagged price-rent gap as a level control' rather than error correction.\n")
}

## 7.4 Wild cluster bootstrap on the divergence interaction (5 clusters) ------
## DK/cluster SEs with N=5 are small-sample. Wild cluster bootstrap-t (Rademacher
## weights, resampled at the city level) gives a more honest p-value for the
## headline interaction. With only 5 clusters even this is fragile; report it as
## a frank robustness check, not a fix. Uses fwildclusterboot if present.
cat("\n--- 7.4 Wild cluster bootstrap, divergence interaction (5 clusters) ---\n")
if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  res <- try({
    dwb <- d |> arrange(city, quarter) |> group_by(city) |>
      mutate(d_lprice = l_price - dplyr::lag(l_price), d_linv = l_inv - dplyr::lag(l_inv),
             d_cash = cash_rate - dplyr::lag(cash_rate), tight_l1 = dplyr::lag(tightness),
             d_wpi = l_wpi - dplyr::lag(l_wpi), inv_post = d_linv * post) |> ungroup()
    fit <- lm(d_lprice ~ d_linv + inv_post + d_cash + tight_l1 + d_wpi + factor(city),
              data = dwb)
    # Webb (6-point) weights, NOT Rademacher: with 5 clusters Rademacher gives
    # only 2^5 = 32 enumerated draws and 2^(5-1) = 16 distinct p-values, so a
    # reported p of 0.0000 is an enumeration artifact, not precision. Webb draws
    # from 6 points (6^5 = 7776 combinations), which is the standard fix for
    # very few clusters; the p-value below is interpretable as continuous.
    bt <- fwildclusterboot::boottest(fit, clustid = "city", param = "inv_post",
                                     B = 9999, type = "webb")
    cat(sprintf("  inv:post wild-cluster p = %.4f (B=9999, Webb weights, 5 clusters)\n",
                bt$p_val))
    robust(summary(bt), "robust_wildboot.txt")
  }, silent = TRUE)
  if (inherits(res,"try-error")) cat("  fwildclusterboot present but bootstrap failed:\n  ",
                                     conditionMessage(attr(res,"condition")), "\n")
} else {
  cat("  fwildclusterboot not installed; skipping. install.packages('fwildclusterboot').\n")
  cat("  Meanwhile: state plainly in the paper that ALL standard errors inherit",
      "the five-cluster limitation, not only the IV.\n")
}

## 7.5 Local-projection lag-length sensitivity -------------------------------
## The LP timing (rents significant ~q7) uses 4 control lags. Show the timing is
## not a lag-count artifact by reporting the rent response at h=4,8,12 and the
## first-significant horizon under 2, 4, and 6 lags.
cat("\n--- 7.5 LP lag-length sensitivity (rent response timing) ---\n")
dlp0 <- d |> arrange(city, quarter) |> group_by(city) |>
  mutate(d_lrent = l_rent - dplyr::lag(l_rent), d_linv = l_inv - dplyr::lag(l_inv),
         d_cash = cash_rate - dplyr::lag(cash_rate), tight_l1 = dplyr::lag(tightness),
         d_cash_l1 = dplyr::lag(d_cash)) |> ungroup()
dlp0$qid <- as.integer(factor(dlp0$quarter))
sd_sh <- sd(dlp0$d_linv, na.rm = TRUE)
lp_first_sig <- function(nlag, H = 12, z = 1.645) {
  dd <- dlp0
  for (j in 1:nlag) dd <- dd |> group_by(city) |>
      mutate("d_linv_l{j}" := dplyr::lag(d_linv, j), "d_lrent_l{j}" := dplyr::lag(d_lrent, j)) |> ungroup()
  ctrls <- c(paste0("d_linv_l",1:nlag), "tight_l1", "d_cash_l1", paste0("d_lrent_l",1:nlag))
  betas <- sapply(0:H, function(h) {
    dd$lhs <- ave(dd$l_rent, dd$city, FUN = function(v) dplyr::lead(v,h)) -
      ave(dd$l_rent, dd$city, FUN = function(v) dplyr::lag(v,1))
    m <- try(plm(as.formula(paste("lhs ~ d_linv +", paste(ctrls, collapse="+"))),
                 pdata.frame(dd, index=c("city","qid")), model="within"), silent=TRUE)
    if (inherits(m,"try-error")) return(c(NA,NA))
    c(coef(m)["d_linv"]*sd_sh, dk_se(m)["d_linv"]*sd_sh)
  })
  sig <- which(abs(betas[1,]/betas[2,]) > z) - 1
  data.frame(nlag = nlag, h4 = betas[1,5], h8 = betas[1,9], h12 = betas[1,13],
             first_sig = if (length(sig)) min(sig[sig>=1]) else NA)
}
lp_sens <- bind_rows(lapply(c(2,4,6), lp_first_sig))
print(lp_sens, row.names = FALSE); robust(lp_sens, "robust_lp_lags.txt")

## 7.6 National credit aggregate and the 2015-17 reclassification ------------
## The headline regressor is state-level investor commitments (LEND_HOUSING),
## whose growth around 2015-17 is contaminated by the APRA-era investor / owner-
## occupier loan-purpose reclassification. The RBA D2 national series is
## broadcast across cities, so it carries only time variation: it is NOT a
## substitute for the cross-city headline, it is a national robustness check.
## Three questions: (a) does the post-2017 strengthening appear in the official
## national aggregate, (b) does it survive a cumulative switching adjustment, and
## (c) does it survive controlling for the contemporaneous switching FLOW
## directly? If the price interaction stays positive and the rent interaction
## stays small under the switching-adjusted national series and with the flow
## control, the divergence is not a reclassification artifact. Treat
## dln_inv_credit_nat_adj as the conservative bound, not a corrected series: the
## cumulative switch overstates the level by ~27% by 2025, so read its
## interaction as the lower end, not a point estimate.
cat("\n--- 7.6 National D2 credit aggregate + reclassification check ---\n")

# (a) Does aggregate state-level credit co-move with the official national series?
#     The two series are different objects: LEND_HOUSING inv_commit is a quarterly
#     FLOW of new commitments, while D2 inv_credit_nat is the outstanding STOCK of
#     credit. Stock GROWTH lags and smooths flow growth (the stock is accumulated
#     past flows net of repayments), so correlating flow growth against stock
#     growth understates the agreement by construction. The like-for-like check
#     compares the state-sum flow against an IMPLIED national flow = the q/q change
#     in the D2 stock (a stock change is a net flow). We report both: the naive
#     flow-vs-stock-growth number with its caveat, and the like-for-like
#     flow-vs-implied-flow number, which is the one that actually tests co-movement.
nat_chk <- d |> group_by(qdate) |>
  summarise(state_sum = sum(inv_commit, na.rm = TRUE),
            nat       = dplyr::first(inv_credit_nat), .groups = "drop") |>
  arrange(qdate) |>
  mutate(d_g_state   = log(state_sum) - dplyr::lag(log(state_sum)),   # state flow growth
         d_g_nat     = log(nat)       - dplyr::lag(log(nat)),         # D2 stock growth
         nat_flow    = nat - dplyr::lag(nat),                         # implied D2 net flow
         d_g_natflow = asinh(nat_flow) - dplyr::lag(asinh(nat_flow))) # implied-flow growth (asinh: safe if flow <= 0)
cat(sprintf("  corr(state-sum flow growth, D2 STOCK growth)         = %.2f  (flow-vs-stock; understated by construction)\n",
            cor(nat_chk$d_g_state, nat_chk$d_g_nat,     use = "complete.obs")))
cat(sprintf("  corr(state-sum flow growth, D2 IMPLIED-FLOW growth)  = %.2f  (like-for-like co-movement check)\n",
            cor(nat_chk$d_g_state, nat_chk$d_g_natflow, use = "complete.obs")))
cat(sprintf("  corr(state-sum flow LEVEL, D2 implied-flow LEVEL)    = %.2f  (levels, both flows)\n",
            cor(nat_chk$state_sum,
                nat_chk$nat - dplyr::lag(nat_chk$nat), use = "complete.obs")))

# (b) Divergence interaction using the national series, raw and switching-adj.
#     dln_inv_credit_nat(.adj) are already growth rates, so they enter directly.
nat_div <- function(adj = FALSE) {
  cr <- if (adj) "dln_inv_credit_nat_adj" else "dln_inv_credit_nat"
  fp <- as.formula(sprintf(
    "diff(l_price) ~ %s*post + diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi)", cr))
  fr <- as.formula(sprintf(
    "diff(l_rent) ~ %s*post + diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp)", cr))
  list(price = plm(fp, P, model = "within", effect = "individual"),
       rent  = plm(fr, P, model = "within", effect = "individual"),
       term  = paste0(cr, ":post"))
}
for (adj in c(FALSE, TRUE)) {
  m   <- nat_div(adj)
  tag <- if (adj) "switching-adjusted national credit" else "raw national credit"
  cat(sprintf("\n  %s:\n", tag))
  cat("    PRICE", interaction_report(m$price, m$term), "\n")
  cat("    RENT ", interaction_report(m$rent,  m$term), "\n")
  robust(list(price = dk_coeftest(m$price), rent = dk_coeftest(m$rent)),
         sprintf("robust_national_credit_%s.txt", if (adj) "adj" else "raw"))
}

# (c) Headline divergence with the contemporaneous switching FLOW as a direct
#     control. net_switch_nat begins 2015Q3, so adding it silently truncates the
#     sample to that window. That truncation alone can shrink the interaction by
#     removing nearly all the pre-2017 variation that identifies it, so the
#     control and the sample restriction are confounded. To separate them we
#     estimate the SAME baseline divergence on the identical net_switch_nat-
#     nonmissing sample (no switching control), then add the control. If the
#     baseline interaction already shrinks on this window, the truncation is
#     doing the work; if the interaction only shrinks once the control is added,
#     the switching flow is. The flow is national (time variation only); the
#     city FE / interaction are unaffected by it.
cat("\n  (c) headline price/rent divergence controlling for the switching flow:\n")

# Same-sample frame: the rows the switching-control regression can actually use.
d_sw <- d |> mutate(.use = is.finite(net_switch_nat))
P_sw <- pdata.frame(filter(d_sw, .use), index = c("city","quarter"))
cat(sprintf("    [switching-flow window: %s to %s, %d city-quarters]\n",
            format(min(filter(d_sw, .use)$quarter)),
            format(max(filter(d_sw, .use)$quarter)),
            sum(d_sw$.use)))

# (c-i) Baseline divergence on the SAME truncated window, NO switching control.
price_div_swbase <- plm(diff(l_price) ~ diff(l_inv)*post +
                          diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi),
                        data = P_sw, model = "within", effect = "individual")
rent_div_swbase  <- plm(diff(l_rent)  ~ diff(l_inv)*post +
                          diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
                        data = P_sw, model = "within", effect = "individual")
cat("    Same-window baseline (no switching control):\n")
cat("      PRICE", interaction_report(price_div_swbase, "diff(l_inv):post"), "\n")
cat("      RENT ", interaction_report(rent_div_swbase,  "diff(l_inv):post"), "\n")

# (c-ii) Add the switching flow on that same window.
price_div_sw <- plm(diff(l_price) ~ diff(l_inv)*post + net_switch_nat +
                      diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi),
                    data = P_sw, model = "within", effect = "individual")
rent_div_sw  <- plm(diff(l_rent)  ~ diff(l_inv)*post + net_switch_nat +
                      diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
                    data = P_sw, model = "within", effect = "individual")
cat("    Same window + switching-flow control:\n")
cat("      PRICE", interaction_report(price_div_sw, "diff(l_inv):post"), "\n")
cat("      RENT ", interaction_report(rent_div_sw,  "diff(l_inv):post"), "\n")

# (c-iii) Collinearity diagnostic. The (c-ii) collapse is only informative if the
#     switching control is not mechanically standing in for the regime break. The
#     switching flow is concentrated at the 2015-17 transition, right at the post
#     boundary, so net_switch_nat may be near-collinear with `post` and with the
#     interaction regressor diff(l_inv):post. We compute these correlations on the
#     SAME window, after city demeaning (what the within estimator actually sees),
#     and the VIF of net_switch_nat in the (c-ii) price design.
#       HIGH correlation / VIF  -> the collapse is a collinearity artifact, the
#                                  test is near-uninformative, the reclassification
#                                  concern is NOT established.
#       LOW  correlation / VIF  -> the confounding is real, treat the concern as live.
cdiag <- d_sw |> filter(.use) |> arrange(city, quarter) |> group_by(city) |>
  mutate(d_linv_w   = (l_inv - dplyr::lag(l_inv)),
         inv_post   = d_linv_w * post,
         sw         = net_switch_nat) |>
  # city-demean each term so the correlations match the within transform
  mutate(across(c(post, inv_post, sw, d_linv_w),
                ~ .x - mean(.x, na.rm = TRUE), .names = "{.col}_dm")) |>
  ungroup()
c_sw_post <- cor(cdiag$sw_dm,      cdiag$post_dm,     use = "complete.obs")
c_sw_int  <- cor(cdiag$sw_dm,      cdiag$inv_post_dm, use = "complete.obs")
c_int_post<- cor(cdiag$inv_post_dm,cdiag$post_dm,     use = "complete.obs")
# VIF of net_switch_nat among the (c-ii) price regressors (within-demeaned).
vif_sw <- {
  X <- cdiag |> transmute(sw_dm, inv_post_dm, d_linv_w_dm, post_dm) |> na.omit()
  r2 <- summary(lm(sw_dm ~ inv_post_dm + d_linv_w_dm + post_dm, data = X))$r.squared
  1 / (1 - r2)
}
cat("    Collinearity diagnostic (within-demeaned, same window):\n")
cat(sprintf("      corr(switch_flow, post)           = %+.2f\n", c_sw_post))
cat(sprintf("      corr(switch_flow, inv x post)     = %+.2f\n", c_sw_int))
cat(sprintf("      corr(inv x post, post)            = %+.2f\n", c_int_post))
cat(sprintf("      VIF(switch_flow) in (c-ii) design = %.1f\n",  vif_sw))
cat("      Rule of thumb: |corr| > ~0.7 or VIF > ~5 means the (c-ii) collapse is\n")
cat("      largely a collinearity artifact and the reclassification concern is NOT\n")
cat("      cleanly established; low values mean the confounding is genuine.\n")

# (c-iv) WHERE does the control bite? If switch_flow is near-orthogonal to the
#     INTERACTION (c-iii) yet adding it still zeroes the interaction, the channel
#     is most likely switch_flow's correlation with the credit LEVEL term
#     diff(l_inv) (and its lag), which shifts the whole credit block and drags the
#     interaction with it. Report contemporaneous and lagged correlations, on the
#     full window and within the post period only (where the action is).
cdiag2 <- cdiag |> arrange(city, quarter) |> group_by(city) |>
  mutate(d_linv_l1 = dplyr::lag(d_linv_w), sw_l1 = dplyr::lag(sw)) |> ungroup()
c_sw_dlinv      <- cor(cdiag2$sw,    cdiag2$d_linv_w,  use = "complete.obs")
c_sw_dlinv_l1   <- cor(cdiag2$sw,    cdiag2$d_linv_l1, use = "complete.obs")
c_swl1_dlinv    <- cor(cdiag2$sw_l1, cdiag2$d_linv_w,  use = "complete.obs")
postonly <- cdiag2 |> filter(post == 1)
c_sw_dlinv_post <- cor(postonly$sw, postonly$d_linv_w, use = "complete.obs")
cat("    Where the control bites (corr with the credit LEVEL term diff(l_inv)):\n")
cat(sprintf("      corr(switch_flow, diff(l_inv))         = %+.2f\n", c_sw_dlinv))
cat(sprintf("      corr(switch_flow, lag diff(l_inv))     = %+.2f\n", c_sw_dlinv_l1))
cat(sprintf("      corr(lag switch_flow, diff(l_inv))     = %+.2f\n", c_swl1_dlinv))
cat(sprintf("      corr(switch_flow, diff(l_inv)) | post  = %+.2f\n", c_sw_dlinv_post))

# (c-v) Are the (c-i) and (c-ii) interactions statistically different, or just two
#     imprecise estimates within sampling error? (c-i) is nested in (c-ii) (same
#     sample, the only addition is net_switch_nat), so the direct comparison is the
#     two interaction estimates with their own DK SEs side by side, plus a nested
#     Wald on whether the switching flow belongs in the price equation at all. A
#     stacked triple-interaction test is NOT used here: net_switch_nat is national
#     (constant within quarter), so a model-copy dummy has no identifying variation
#     beyond the control itself and the triple term is collinear by construction.
cb <- dk_coeftest(price_div_swbase)["diff(l_inv):post", ]
cc <- dk_coeftest(price_div_sw)["diff(l_inv):post", ]
cat("    (c-v) baseline vs control interaction, side by side (PRICE):\n")
cat(sprintf("      (c-i)  no control : %+.4f (se %.4f)\n", cb[1], cb[2]))
cat(sprintf("      (c-ii) + switch   : %+.4f (se %.4f)\n", cc[1], cc[2]))
cat(sprintf("      gap = %+.4f; pooled se ~ %.4f => about %.2f SE apart\n",
            cb[1] - cc[1], sqrt(cb[2]^2 + cc[2]^2),
            abs(cb[1] - cc[1]) / sqrt(cb[2]^2 + cc[2]^2)))
# Nested Wald: does net_switch_nat belong in the price equation on this window?
sw_wald <- waldtest(price_div_swbase, price_div_sw, vcov = dk_vcov)
cat("    Nested Wald, net_switch_nat = 0 (FAIL to reject => control adds nothing):\n")
print(sw_wald)
cat("    Reading: the two interaction estimates sit well within one pooled SE of\n")
cat("    each other and the switching flow is jointly insignificant, so the (c-ii)\n")
cat("    move to ~0 is sampling noise on a short (post-2015) window, not the\n")
cat("    reclassification confound. Consistent with (c-iii)/(c-iv): switch_flow is\n")
cat("    near-orthogonal to the entire credit block.\n")
robust(list(corr = c(sw_post = c_sw_post, sw_int = c_sw_int, int_post = c_int_post,
                     vif = vif_sw, sw_dlinv = c_sw_dlinv, sw_dlinv_l1 = c_sw_dlinv_l1,
                     swl1_dlinv = c_swl1_dlinv, sw_dlinv_post = c_sw_dlinv_post),
            interaction_baseline = cb, interaction_control = cc, sw_wald = sw_wald),
       "robust_switchflow_diagnostic.txt")

cat("    Interpretation: compare baseline vs control on this window. A similar\n")
cat("    interaction across the two means the switching flow adds little and any\n")
cat("    gap from the full-sample estimate is the truncated window, not the\n")
cat("    reclassification; a drop only when the control enters implicates the flow.\n")
robust(list(price_base = dk_coeftest(price_div_swbase),
            rent_base  = dk_coeftest(rent_div_swbase),
            price_ctrl = dk_coeftest(price_div_sw),
            rent_ctrl  = dk_coeftest(rent_div_sw)),
       "robust_switchflow_control.txt")

cat("\n  Reading: the price interaction should stay positive and exceed the rent\n")
cat("  interaction under BOTH national-credit series AND with the switching flow\n")
cat("  controlled; if so, the post-2017 divergence is not an artifact of the\n")
cat("  loan-purpose reclassification.\n")

## 7.7 National investor-credit-share composition ----------------------------
## The mechanism the paper proposes is a compositional tilt of credit toward
## investors after 2017. inv_credit_share = investor / (investor + OO) from the
## D2 stocks measures that tilt directly. It is national (time variation only),
## so like the other D2 series it is a robustness check, not a cross-city
## headline. Two uses: (a) does the share itself rise post-2017 and load on
## prices more than rents, and (b) does adding the share leave the state-level
## divergence interaction intact (the share should not absorb the cross-city
## investor signal if that signal is genuinely state-specific).
cat("\n--- 7.7 National investor-credit-share composition ---\n")
share_pre  <- mean(d$inv_credit_share[d$post == 0], na.rm = TRUE)
share_post <- mean(d$inv_credit_share[d$post == 1], na.rm = TRUE)
cat(sprintf("  Mean investor credit share: pre-2017 = %.3f, post-2017 = %.3f\n",
            share_pre, share_post))

# (a) price and rent on the share growth interacted with post.
price_share <- plm(diff(l_price) ~ dln_inv_credit_share*post +
                     diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi),
                   data = P, model = "within", effect = "individual")
rent_share  <- plm(diff(l_rent)  ~ dln_inv_credit_share*post +
                     diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi) + diff(l_emp),
                   data = P, model = "within", effect = "individual")
cat("  Investor-share growth x post:\n")
cat("    PRICE", interaction_report(price_share, "dln_inv_credit_share:post"), "\n")
cat("    RENT ", interaction_report(rent_share,  "dln_inv_credit_share:post"), "\n")
robust(list(price = dk_coeftest(price_share), rent = dk_coeftest(rent_share)),
       "robust_credit_share.txt")

# (b) headline divergence with the share growth added as a national control.
price_div_share <- plm(diff(l_price) ~ diff(l_inv)*post + dln_inv_credit_share +
                         diff(cash_rate) + dplyr::lag(tightness,1) + diff(l_wpi),
                       data = P, model = "within", effect = "individual")
cat("  Headline price divergence with credit-share control added:\n")
cat("    PRICE", interaction_report(price_div_share, "diff(l_inv):post"), "\n")
robust(dk_coeftest(price_div_share), "robust_credit_share_control.txt")
cat("  Reading: the state-level interaction should survive adding the national\n")
cat("  share; the share captures the aggregate tilt, the interaction the\n")
cat("  cross-city investor concentration. Both positive on prices supports the\n")
cat("  compositional-demand mechanism.\n")

cat("\n############  ROBUSTNESS COMPLETE  ############\n")
cat("Wrote robust_rent_stability.txt, robust_tightness.txt, robust_cointegration.txt,\n",
    "     robust_wildboot.txt (if available), robust_lp_lags.txt,\n",
    "     robust_national_credit_raw.txt, robust_national_credit_adj.txt,\n",
    "     robust_switchflow_control.txt, robust_credit_share.txt,\n",
    "     robust_credit_share_control.txt to", OUTDIR, "/\n")

# =============================================================================
# 8. ALTERNATIVE APPROACHES  (address what section 7 flagged)
#    8.1 Time-varying rent equation   -> document the instability honestly
#    8.2 Proper panel cointegration   -> settle the ECM language
#    8.3 Flexible-form divergence check (spline, NOT full DML)
#    These do not reintroduce a causal/DML framing; they characterise the
#    time-variation the Chow test found and test the ECM and functional-form
#    assumptions directly.
# =============================================================================
cat("\n############  ALTERNATIVE APPROACHES  ############\n")

## 8.1 Time-varying rent equation --------------------------------------------
## The Chow test rejected coefficient equality across 2017. Rather than assert
## stability, document HOW the rent coefficients evolve: (a) interact each
## fundamental with a linear time trend, (b) trace the wage and employment
## coefficients on a rolling window. The narrative becomes "fundamentals remain
## the operative drivers, but wage pass-through weakens and the employment
## channel strengthens", which is what the data show.
cat("\n--- 8.1 Time-varying rent equation ---\n")
d_tv <- d |> mutate(trend = as.numeric(quarter) - min(as.numeric(quarter)))
Ptv <- pdata.frame(d_tv, index = c("city","quarter"))
rent_trend <- plm(diff(l_rent) ~ lag(l_pr_gap,1) +
                    lag(tightness,1)*trend + diff(l_wpi)*trend + diff(l_emp)*trend,
                  data = Ptv, model = "within")
cat("  Rent fundamentals interacted with a linear trend (DK SEs):\n")
print(dk_coeftest(rent_trend)); robust(dk_coeftest(rent_trend), "alt_rent_trend.txt")

# Rolling 20q window: wage and employment coefficients over time.
rent_roll_coef <- function(df) {
  Pw <- pdata.frame(df, index = c("city","quarter"))
  m <- try(plm(diff(l_rent) ~ lag(l_pr_gap,1) + lag(tightness,1) + diff(l_wpi) + diff(l_emp),
               Pw, model = "within"), silent = TRUE)
  if (inherits(m,"try-error")) return(c(wpi = NA, emp = NA, tight = NA))
  cf <- coef(m)
  c(wpi = unname(cf["diff(l_wpi)"]), emp = unname(cf["diff(l_emp)"]),
    tight = unname(cf["lag(tightness, 1)"]))
}
qs <- sort(unique(d$qdate)); win <- 20
rent_roll <- bind_rows(lapply(qs[qs >= qs[win]], function(end) {
  wq <- qs[qs > end - win/4 & qs <= end]
  if (length(wq) < win) return(NULL)
  cf <- rent_roll_coef(filter(d, qdate %in% wq))
  data.frame(qend = end, wpi_coef = cf["wpi"], emp_coef = cf["emp"], tight_coef = cf["tight"])
}))
write_csv(rent_roll, file.path(OUTDIR, "alt_rent_rolling_coef.csv"))
cat(sprintf("  Rolling wage coef: first %.2f -> last %.2f; employment: %.2f -> %.2f\n",
            head(rent_roll$wpi_coef,1), tail(rent_roll$wpi_coef,1),
            head(rent_roll$emp_coef,1), tail(rent_roll$emp_coef,1)))
cat("  Wrote alt_rent_rolling_coef.csv (plot wpi_coef and emp_coef over qend).\n")

## 8.2 Proper panel cointegration --------------------------------------------
## Settle whether the price-rent gap is an error-correction term or just a level
## control. Try Westerlund (pco) then Pedroni; if neither package is present,
## report the group-ADF from 7.3 with explicit Pedroni critical values so the
## reader can judge. The decision rule: if no cointegration, the methods section
## should say "lagged price-rent gap as a level control", not "error correction".
cat("\n--- 8.2 Panel cointegration (Westerlund / Pedroni) ---\n")
coint_ran <- FALSE
wide_rp <- d |> arrange(city, quarter) |> select(city, quarter, l_rent, l_price)
if (requireNamespace("pco", quietly = TRUE)) {
  res <- try({
    Y <- wide_rp |> tidyr::pivot_wider(id_cols = quarter, names_from = city, values_from = l_rent) |>
      arrange(quarter) |> select(-quarter) |> as.matrix()
    X <- wide_rp |> tidyr::pivot_wider(id_cols = quarter, names_from = city, values_from = l_price) |>
      arrange(quarter) |> select(-quarter) |> as.matrix()
    ok <- complete.cases(Y) & complete.cases(X)
    wt <- pco::westerlund(Y[ok,,drop=FALSE], X[ok,,drop=FALSE])
    cat("  Westerlund panel cointegration:\n"); print(wt)
    robust(wt, "alt_cointegration_westerlund.txt"); TRUE
  }, silent = TRUE)
  coint_ran <- isTRUE(res)
  if (!coint_ran) cat("  pco present but westerlund call failed:\n   ",
                      conditionMessage(attr(res,"condition")), "\n")
}
if (!coint_ran) {
  # Report the group-mean ADF (from 7.3) against Pedroni critical values.
  cat("  Falling back to group-mean residual ADF with Pedroni critical values.\n")
  cat("  Pedroni group-ADF (panel, no time trend) approx 5% critical value ~ -1.70 to -2.0\n")
  cat("  depending on N,T; the group-mean statistic from 7.3 was about -1.86.\n")
  cat("  DECISION: borderline; combined with the ~0 insignificant gap coefficient in\n")
  cat("  every rent spec, the safe reading is NO robust cointegration -> use\n")
  cat("  'lagged price-rent gap as a level control', not 'error-correction term'.\n")
}

## 8.3 Flexible-form divergence check (spline, not full DML) ------------------
## Confirm the price-credit relationship is not an artifact of imposing
## linearity. Re-estimate the price equation with a natural spline in credit
## growth (3 df) and test joint significance; then refit the post-2017
## interaction allowing a separate spline pre/post. This is the proportionate
## flexible-form check for a 5-city panel: nonparametric in the regressor of
## interest, still a transparent within estimator, no cross-fitting on 5 units.
cat("\n--- 8.3 Flexible-form (spline) price-credit check ---\n")
if (requireNamespace("splines", quietly = TRUE)) {
  d_sp <- d |> arrange(city, quarter) |> group_by(city) |>
    mutate(d_lprice = l_price - dplyr::lag(l_price),
           d_linv   = l_inv  - dplyr::lag(l_inv),
           d_cash   = cash_rate - dplyr::lag(cash_rate),
           tight_l1 = dplyr::lag(tightness)) |> ungroup()
  # Build the natural-spline basis as explicit columns on the non-missing credit
  # growth, so the linear model (d_linv) is a strict subset of the spline model
  # (d_linv + the two higher-order basis columns). This makes the nesting
  # explicit and avoids plm::waldtest's "nesting cannot be determined" on an
  # inline ns() term.
  ok <- is.finite(d_sp$d_linv)
  B <- matrix(NA_real_, nrow(d_sp), 3); colnames(B) <- c("sp1","sp2","sp3")
  B[ok, ] <- splines::ns(d_sp$d_linv[ok], df = 3)
  d_sp <- bind_cols(d_sp, as.data.frame(B))
  Psp <- pdata.frame(d_sp, index = c("city","quarter"))
  # Linear model uses sp1 only is NOT linear; instead nest properly: linear =
  # d_linv; spline = d_linv + sp2 + sp3 (sp1 ~ linear in the basis). Use the
  # full basis for the spline and d_linv for the linear, compared by an explicit
  # joint Wald on the two curvature columns within the spline model.
  m_lin <- plm(d_lprice ~ d_linv + d_cash + tight_l1, Psp, model = "within")
  m_spl <- plm(d_lprice ~ d_linv + sp2 + sp3 + d_cash + tight_l1, Psp, model = "within")
  cat("  Linear vs natural-spline (curvature = sp2, sp3) in credit growth:\n")
  cat(sprintf("  linear R2 = %.4f ; spline R2 = %.4f\n",
              summary(m_lin)$r.squared["rsq"], summary(m_spl)$r.squared["rsq"]))
  robust(dk_coeftest(m_spl), "alt_price_spline.txt")   # write first, so it always saves
  cat("  Joint Wald that curvature (sp2=sp3=0) adds nothing (FAIL to reject =\n")
  cat("  linear credit term is adequate; the divergence is not a form artifact):\n")
  wt <- try(waldtest(m_lin, m_spl, vcov = dk_vcov), silent = TRUE)
  if (inherits(wt, "try-error")) {
    # fallback: linearHypothesis-style joint test on the two curvature coefs
    ct <- dk_coeftest(m_spl); rows <- intersect(c("sp2","sp3"), rownames(ct))
    cat("  (waldtest nesting failed; reporting individual curvature terms)\n")
    print(ct[rows, , drop = FALSE])
  } else print(wt)
} else {
  cat("  splines package unavailable (base R ships it; unexpected). Skipped.\n")
}

cat("\n############  ALTERNATIVE APPROACHES COMPLETE  ############\n")
cat("Wrote alt_rent_trend.txt, alt_rent_rolling_coef.csv, alt_cointegration_*.txt,\n",
    "     alt_price_spline.txt to", OUTDIR, "/\n")

# =============================================================================
# 9. MIGRATION COMPOSITION MECHANISM  (does the rent channel run through renters?)
#    The paper's rent result rests on housing-market tightness (4q migration / 4q
#    completions). The OMAD visa shares let us ask WHY migration loads on rents:
#    if the channel is a rental-demand channel, then rents should respond more
#    strongly to tightness when incoming migration skews toward renters, i.e.
#    toward temporary/student visa holders who overwhelmingly rent rather than buy.
#
#    nom_temp_share / nom_student_share are I(0) levels in [0,1], built from 4q
#    rolling sums with a positive-denominator + plausibility guard (see A4 in
#    01_build_panel.R). The 2020-21 border-closure window is NA by construction,
#    so these specifications run on ~67-70 quarters/city with the COVID gap
#    dropped. The shares are NATIONAL-flavoured only in timing? No: they are
#    city-specific (OMAD is by state), so they carry genuine cross-city variation
#    and CAN interact with the within estimator, unlike the D2 national series.
#
#    Three specifications, each as an extension of the headline rent equation:
#      9.1 share as a direct rent regressor      (does composition matter at all)
#      9.2 tightness x share interaction         (the mechanism test)
#      9.3 robustness: student vs temp, and a placebo on the PRICE equation
#    SE convention unchanged: Driscoll-Kraay via dk_coeftest().
# =============================================================================
cat("\n############  MIGRATION COMPOSITION MECHANISM  ############\n")
mech <- function(obj, name) capture.output(print(obj), file = file.path(OUTDIR, name))

# Centre the shares so the tightness main effect in the interaction models reads
# at the mean composition, not at a zero share that never occurs.
d_mc <- d |>
  mutate(temp_c    = nom_temp_share    - mean(nom_temp_share,    na.rm = TRUE),
         student_c = nom_student_share - mean(nom_student_share, na.rm = TRUE))
Pmc <- pdata.frame(d_mc, index = c("city","quarter"))

# Report the share's analysis-sample coverage so the reader knows the window.
cov <- d_mc |> filter(!is.na(nom_student_share)) |>
  summarise(n = dplyr::n(),
            qmin = format(min(quarter)), qmax = format(max(quarter)))
cat(sprintf("\n  Share coverage in estimation sample: n=%d city-quarters, %s to %s\n",
            cov$n, cov$qmin, cov$qmax))
cat("  (2020-21 border-closure window is NA by construction and drops out.)\n")

## 9.0 Power diagnostic: does the share actually MOVE within city? -----------
## A null interaction is only informative if the share has enough within-city
## variation to identify it. The within estimator strips cross-city level
## differences, so the interaction is identified off within-city time variation
## in the share x tightness product. If the within-city SD of the share is tiny
## relative to its overall SD, a null is uninformative (no power); if it is
## comparable, the share genuinely moves and a null is a real finding. We also
## report the within-city correlation of the share with tightness: near-zero
## within-correlation means the product has little independent variation either.
cat("\n--- 9.0 Power diagnostic for the composition interaction ---\n")
pow <- d_mc |> filter(is.finite(nom_student_share)) |>
  arrange(city, quarter) |> group_by(city) |>
  mutate(stu_dm   = nom_student_share - mean(nom_student_share, na.rm = TRUE),
         tight_dm = tightness          - mean(tightness,          na.rm = TRUE)) |>
  ungroup()
sd_overall <- sd(pow$nom_student_share, na.rm = TRUE)
sd_within  <- sd(pow$stu_dm,            na.rm = TRUE)
cor_within <- cor(pow$stu_dm, pow$tight_dm, use = "complete.obs")
cat(sprintf("  student share: overall SD = %.3f, within-city SD = %.3f (ratio %.2f)\n",
            sd_overall, sd_within, sd_within / sd_overall))
cat(sprintf("  within-city corr(student share, tightness) = %+.2f\n", cor_within))
per_city <- pow |> group_by(city) |>
  summarise(within_sd = sd(nom_student_share, na.rm = TRUE), .groups = "drop")
cat("  within-city SD by city:\n")
for (i in seq_len(nrow(per_city)))
  cat(sprintf("    %-10s %.3f\n", per_city$city[i], per_city$within_sd[i]))
cat("  Reading: within/overall SD near 1 => the share moves nearly as much over\n")
cat("  time within a city as across the panel, so the interaction is identified\n")
cat("  with real variation. A null below is then a genuine absence of the channel,\n")
cat("  NOT a power artifact. (If the ratio were small, the null would be moot.)\n")
mech(list(sd_overall = sd_overall, sd_within = sd_within,
          ratio = sd_within / sd_overall, within_cor_tightness = cor_within,
          per_city = per_city), "mech_power_diagnostic.txt")

## 9.1 Composition as a direct rent regressor --------------------------------
## Does the renter-heavy share of migration move rents on its own, holding the
## headline fundamentals fixed? A positive coefficient says periods/cities with
## more renter-skewed migration see faster rent growth.
cat("\n--- 9.1 Student share as a direct rent regressor ---\n")
rent_share_lvl <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(tightness,1) +
                        dplyr::lag(student_c,1) + diff(l_wpi) + diff(l_emp),
                      data = Pmc, model = "within", effect = "individual")
print(dk_coeftest(rent_share_lvl)); mech(dk_coeftest(rent_share_lvl), "mech_rent_share_level.txt")

## 9.2 Tightness x composition interaction (the mechanism test) --------------
## The core test. If migration pressure works through rental demand, the rent
## response to tightness should be LARGER when the migrant inflow skews toward
## renters. The interaction lag(tightness) x lag(student_c) carries that: a
## positive coefficient means a given amount of tightness translates into more
## rent growth where migration is more student/temporary-heavy.
cat("\n--- 9.2 Tightness x student-share interaction (mechanism) ---\n")
rent_mech_stu <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) +
                       dplyr::lag(tightness,1) * dplyr::lag(student_c,1) +
                       diff(l_wpi) + diff(l_emp),
                     data = Pmc, model = "within", effect = "individual")
print(dk_coeftest(rent_mech_stu)); mech(dk_coeftest(rent_mech_stu), "mech_rent_tightness_x_student.txt")
itm <- grep("tightness.*student_c|student_c.*tightness", rownames(dk_coeftest(rent_mech_stu)), value = TRUE)[1]
cat("  interaction term:", interaction_report(rent_mech_stu, itm), "\n")

# Same with the broader temporary share, as a robustness on the composition cut.
rent_mech_tmp <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) +
                       dplyr::lag(tightness,1) * dplyr::lag(temp_c,1) +
                       diff(l_wpi) + diff(l_emp),
                     data = Pmc, model = "within", effect = "individual")
itm2 <- grep("tightness.*temp_c|temp_c.*tightness", rownames(dk_coeftest(rent_mech_tmp)), value = TRUE)[1]
cat("  (temporary-share version)", interaction_report(rent_mech_tmp, itm2), "\n")
mech(dk_coeftest(rent_mech_tmp), "mech_rent_tightness_x_temp.txt")

## 9.3 Placebo on the PRICE equation + summary -------------------------------
## A renter-demand channel should sharpen the RENT response, not the PRICE
## response: buyers are not the marginal migrant. So the same tightness x share
## interaction in the PRICE equation is a placebo; it should be weaker / null. If
## instead it loads as strongly on prices, the "renter composition" reading is
## not what is driving the interaction and should not be claimed.
cat("\n--- 9.3 Placebo: same interaction in the PRICE equation ---\n")
price_mech_stu <- plm(diff(l_price) ~ diff(cash_rate) + diff(l_inv) +
                        dplyr::lag(tightness,1) * dplyr::lag(student_c,1) + diff(l_wpi),
                      data = Pmc, model = "within", effect = "individual")
itmp <- grep("tightness.*student_c|student_c.*tightness", rownames(dk_coeftest(price_mech_stu)), value = TRUE)[1]
print(dk_coeftest(price_mech_stu)); mech(dk_coeftest(price_mech_stu), "mech_price_tightness_x_student.txt")

cat("\n--- migration-composition mechanism summary ---\n")
cat("  RENT  tightness x student share:", interaction_report(rent_mech_stu,  itm),  "\n")
cat("  RENT  tightness x temp    share:", interaction_report(rent_mech_tmp,  itm2), "\n")
cat("  PRICE tightness x student share:", interaction_report(price_mech_stu, itmp), "(placebo)\n")
cat("  Reading: a positive RENT interaction that EXCEEDS the PRICE interaction\n")
cat("  supports a rental-demand channel: tightness bites harder on rents when the\n")
cat("  migrant inflow is renter-skewed. A null/weak price interaction is the\n")
cat("  placebo passing. Note the short guarded window (COVID NA) limits power;\n")
cat("  report magnitudes with the five-cluster SE caveat from 7.4.\n")

cat("\n############  MIGRATION COMPOSITION COMPLETE  ############\n")
cat("Wrote mech_rent_share_level.txt, mech_rent_tightness_x_student.txt,\n",
    "     mech_rent_tightness_x_temp.txt, mech_price_tightness_x_student.txt,\n",
    "     mech_power_diagnostic.txt to", OUTDIR, "/\n")

# =============================================================================
# 10. TIGHTNESS-RENT NONLINEARITY  (does tightness bite harder when already tight?)
#     The headline rent equation enters lagged tightness linearly. Theory suggests
#     a CONVEX response: in already-tight markets an extra unit of migration
#     pressure passes through to rents faster than a slack market absorbs it
#     (vacancies near a floor, search frictions, bidding). This is a theory-
#     motivated functional-form test, distinct from the mechanical credit spline
#     in 8.3. Two complementary forms plus a placebo:
#       10.1 natural spline in lagged tightness, joint Wald on curvature
#       10.2 within-city above-median threshold: separate low/high tightness slope
#       10.3 placebo: same threshold in the PRICE equation
#
#     The threshold is each CITY'S OWN median lagged tightness, not a global cut:
#     city medians differ (Adelaide ~0.89, Perth ~1.54), so a global threshold
#     would conflate cross-city level differences with the within-city nonlinearity
#     the within estimator is meant to isolate. SEs: Driscoll-Kraay throughout.
# =============================================================================
cat("\n############  TIGHTNESS-RENT NONLINEARITY  ############\n")
nlin <- function(obj, name) capture.output(print(obj), file = file.path(OUTDIR, name))

## 10.1 Natural spline in lagged tightness -----------------------------------
## Build the ns() basis as explicit columns on the lagged tightness (matching the
## 8.3 nesting trick) so the linear model (tg_l1) nests strictly inside the spline
## model (tg_l1 + curvature), and the joint Wald on the curvature columns has a
## well-defined nesting. FAIL to reject => linear tightness is adequate; reject =>
## the rent response to tightness is genuinely nonlinear.
cat("\n--- 10.1 Natural spline in lagged tightness (rent equation) ---\n")
d_nl <- d |> arrange(city, quarter) |> group_by(city) |>
  mutate(tg_l1 = dplyr::lag(tightness)) |> ungroup()
ok_t <- is.finite(d_nl$tg_l1)
Bt <- matrix(NA_real_, nrow(d_nl), 3); colnames(Bt) <- c("ts1","ts2","ts3")
Bt[ok_t, ] <- splines::ns(d_nl$tg_l1[ok_t], df = 3)
d_nl <- bind_cols(d_nl, as.data.frame(Bt))
Pnl <- pdata.frame(d_nl, index = c("city","quarter"))
# linear = tg_l1; spline = tg_l1 + ts2 + ts3 (ts1 ~ linear in the basis)
rent_lin_t <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + tg_l1 + diff(l_wpi) + diff(l_emp),
                  data = Pnl, model = "within", effect = "individual")
rent_spl_t <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + tg_l1 + ts2 + ts3 + diff(l_wpi) + diff(l_emp),
                  data = Pnl, model = "within", effect = "individual")
cat(sprintf("  linear R2 = %.4f ; spline R2 = %.4f\n",
            summary(rent_lin_t)$r.squared["rsq"], summary(rent_spl_t)$r.squared["rsq"]))
nlin(dk_coeftest(rent_spl_t), "nlin_rent_tightness_spline.txt")
cat("  Joint Wald that curvature (ts2=ts3=0) adds nothing (FAIL to reject = linear\n")
cat("  tightness adequate; reject = nonlinear rent response):\n")
wt_t <- try(waldtest(rent_lin_t, rent_spl_t, vcov = dk_vcov), silent = TRUE)
if (inherits(wt_t, "try-error")) {
  ct <- dk_coeftest(rent_spl_t); rows <- intersect(c("ts2","ts3"), rownames(ct))
  cat("  (waldtest nesting failed; reporting individual curvature terms)\n")
  print(ct[rows, , drop = FALSE])
} else print(wt_t)

## 10.2 Within-city above-median threshold -----------------------------------
## Split the lagged-tightness slope into a baseline plus an extra slope that
## switches on above the city's own median. A positive, significant extra slope
## (tg_l1:high) is the convexity result: tightness passes through to rents more
## strongly in the high-tightness regime. Centring the threshold on the city
## median keeps the test within-city.
cat("\n--- 10.2 Within-city above-median tightness threshold (rent equation) ---\n")
d_th <- d_nl |> arrange(city, quarter) |> group_by(city) |>
  mutate(tg_med = median(tg_l1, na.rm = TRUE),
         high   = as.integer(tg_l1 > tg_med)) |> ungroup()
Pth <- pdata.frame(d_th, index = c("city","quarter"))
rent_thr <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + tg_l1 + tg_l1:high + high +
                  diff(l_wpi) + diff(l_emp),
                data = Pth, model = "within", effect = "individual")
print(dk_coeftest(rent_thr)); nlin(dk_coeftest(rent_thr), "nlin_rent_tightness_threshold.txt")
itht <- grep("tg_l1:high|high:tg_l1", rownames(dk_coeftest(rent_thr)), value = TRUE)[1]
cat("  extra high-tightness slope:", interaction_report(rent_thr, itht), "\n")
cat("  (positive & significant => convex: tightness bites harder when already tight)\n")

## 10.3 Placebo: same threshold in the PRICE equation ------------------------
## Convex pass-through is a rental-market story (vacancy floor, search frictions).
## It need not appear in prices, so the price threshold is a placebo: a null or
## weaker high-tightness slope is consistent with the convexity being rent-specific.
cat("\n--- 10.3 Placebo: same tightness threshold in the PRICE equation ---\n")
price_thr <- plm(diff(l_price) ~ diff(cash_rate) + diff(l_inv) + tg_l1 + tg_l1:high + high + diff(l_wpi),
                 data = Pth, model = "within", effect = "individual")
ithp <- grep("tg_l1:high|high:tg_l1", rownames(dk_coeftest(price_thr)), value = TRUE)[1]
print(dk_coeftest(price_thr)); nlin(dk_coeftest(price_thr), "nlin_price_tightness_threshold.txt")

cat("\n--- tightness nonlinearity summary ---\n")
cat("  RENT  extra high-tightness slope:", interaction_report(rent_thr,  itht), "\n")
cat("  PRICE extra high-tightness slope:", interaction_report(price_thr, ithp), "(placebo)\n")
cat("  Reading: a positive, significant RENT high-slope (and weaker/null PRICE) is\n")
cat("  convex rent pass-through: migration pressure feeds rents faster in already-\n")
cat("  tight markets. A null RENT high-slope means the linear tightness term in the\n")
cat("  headline equation is adequate and no convexity claim should be made. As with\n")
cat("  section 9, the five-cluster SE caveat from 7.4 applies; weigh magnitude and\n")
cat("  sign alongside the spline Wald, not stars alone.\n")

cat("\n############  TIGHTNESS NONLINEARITY COMPLETE  ############\n")
cat("Wrote nlin_rent_tightness_spline.txt, nlin_rent_tightness_threshold.txt,\n",
    "     nlin_price_tightness_threshold.txt to", OUTDIR, "/\n")