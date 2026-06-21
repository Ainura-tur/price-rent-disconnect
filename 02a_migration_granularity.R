# =============================================================================
# 03_migration_granularity.R
#
# Does more granular migration shed further light on the rent or price equation?
#
# Section 9 of 02_estimate.R already asks one question: does the renter-skew of
# migration (student/temporary SHARE) modulate the tightness->rent channel? It
# reports a well-powered null. This script asks the DIFFERENT, unexplored
# questions that the single omnibus tightness ratio (net_migration_4q /
# dwell_total_4q) forces shut:
#
#   1. LEVELS, not shares. Does a tightness ratio built from a renter-relevant
#      SUBSET of migration (overseas-only; overseas net of students; a renter-
#      weighted flow) fit the rent equation better than the omnibus ratio that
#      weights every migrant equally? (Horse-race on fit + coefficient.)
#
#   2. UNBUNDLING the numerator. If overseas and interstate net flows are entered
#      with SEPARATE slopes instead of summed into one tightness ratio, do they
#      differ? Interstate movers skew owner-occupier / established-renter; overseas
#      arrivals skew new-rental-demand. A Wald test of slope equality says whether
#      the omnibus ratio is masking a real wedge.
#
#   3. PRICE-SIDE PLACEBO (levels version). Migration is a rent-side fundamental.
#      No granular migration flow should carry independent power in the PRICE
#      equation. If one does, the rent/price separation the paper draws is leakier
#      than claimed. (Distinct from section 9.3, which placebos the SHARE
#      interaction; here we placebo the FLOW levels.)
#
# Nothing here changes the headline specs. It is a self-contained diagnostic that
# reports whether granularity earns its keep. Reads the SAME panel and the SAME
# DK inference machinery as 02_estimate.R.
#
# Reads:  clean/panel_combined.rds
# Writes: output/granular_*.txt
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

source("00_setup.R")
library(plm); library(lmtest); library(sandwich)

# Driscoll-Kraay vcov, identical to 02_estimate.R line 136. dk_coeftest() comes
# from 00_setup.R, but dk_vcov is defined in 02_estimate.R (not setup), so this
# standalone script redefines it here with the SAME formula to stay consistent.
dk_vcov <- function(x) plm::vcovSCC(x, type = "HC0", maxlag = 4)

gout <- function(obj, name) capture.output(print(obj), file = file.path(OUTDIR, name))

panel <- readRDS(file.path(CLEAN, "panel_combined.rds")) |>
  arrange(city, quarter) |> mutate(qdate = as.numeric(quarter))

# -----------------------------------------------------------------------------
# Build the alternative tightness numerators ONCE, on the same 4q-rolling basis
# as the headline tightness, so the only thing that changes across specs is WHICH
# migration flow feeds the numerator. dwell_total denominator is held fixed.
#
#   tightness        : net_migration (overseas + interstate)   [headline]
#   tightness_no     : net_overseas only                       [already in panel]
#   tightness_no_stu : overseas net of the student component   [renter-relevant
#                       inflow MINUS the most rental-concentrated group; if the
#                       student piece were the whole rent signal, dropping it
#                       would gut the fit -- a direct test of "is it the students"]
#   tightness_rw     : renter-WEIGHTED overseas flow, weighting the overseas net
#                       flow by its trailing temporary share (temp migrants flow
#                       overwhelmingly into rental). This is the sharpest a priori
#                       renter-demand proxy the visa data supports.
#
# nom_*_share are trailing-4q shares already; multiply back onto the 4q overseas
# flow to recover an approximate 4q renter-relevant overseas count. Where the
# share is guarded-NA (COVID window) the derived numerators inherit NA, which is
# the honest behaviour -- those quarters drop, exactly as in section 9.
# -----------------------------------------------------------------------------
panel <- panel |>
  group_by(city) |>
  mutate(
    nm_4q   = zoo::rollsumr(net_migration, 4, fill = NA),
    no_4q   = zoo::rollsumr(net_overseas,  4, fill = NA),
    ni_4q   = zoo::rollsumr(net_interstate,4, fill = NA),
    dw_4q   = zoo::rollsumr(dwell_total,   4, fill = NA),
    # overseas net of students: (1 - student share) * overseas 4q flow
    no_nostu_4q = ifelse(is.finite(nom_student_share),
                         (1 - nom_student_share) * no_4q, NA_real_),
    # renter-weighted overseas: temp share * overseas 4q flow
    no_rw_4q    = ifelse(is.finite(nom_temp_share),
                         nom_temp_share * no_4q, NA_real_),
    tightness_chk    = nm_4q / dw_4q,          # reconstruct headline as a check
    tightness_no_chk = no_4q / dw_4q,
    tightness_no_stu = no_nostu_4q / dw_4q,
    tightness_rw     = no_rw_4q / dw_4q,
    # separate scaled flows for the unbundling test (each over the SAME denom)
    over_dw  = no_4q / dw_4q,
    inter_dw = ni_4q / dw_4q
  ) |>
  ungroup()

# Sanity: the reconstructed headline tightness should match the panel's stored one
# (up to rolling-window edge NAs). Fail loudly if the build drifted.
chk <- panel |> filter(is.finite(tightness), is.finite(tightness_chk)) |>
  summarise(max_abs_diff = max(abs(tightness - tightness_chk)))
cat(sprintf("tightness reconstruction check: max |diff| = %.2e\n", chk$max_abs_diff))
stopifnot(chk$max_abs_diff < 1e-8)

d <- panel |>
  mutate(l_rent = log(rents), l_price = log(prop_price), l_wpi = log(wpi),
         l_emp = log(employment), l_inv = log(inv_commit), l_pr_gap = log(price_rent))
P <- pdata.frame(d, index = c("city","quarter"))

# Helper: pull a single coefficient row under DK inference, formatted.
co <- function(m, term) {
  ct <- dk_coeftest(m)
  if (!term %in% rownames(ct)) return(sprintf("%s = [absent]", term))
  sprintf("%s = %+.4f (se %.4f, p=%.4f)", term, ct[term,1], ct[term,2], ct[term,4])
}
# Helper: within-R2 for fit comparison on the SAME estimation sample.
r2 <- function(m) summary(m)$r.squared["rsq"]

# =============================================================================
# 1. RENT EQUATION: horse-race tightness numerators
#    Same rent spec as the headline (FD within), swapping only the lagged
#    tightness regressor. Compared on within-R2 and on the tightness coefficient.
#    IMPORTANT: the renter-relevant variants are NA in the COVID window, so they
#    run on a smaller sample than the headline. We therefore also re-fit the
#    HEADLINE tightness on each variant's sample, so every fit comparison is
#    like-for-like (same rows) rather than confounded by sample size.
# =============================================================================
cat("\n############  1. RENT: TIGHTNESS NUMERATOR HORSE-RACE  ############\n")

rent_spec <- function(tvar, data) {
  f <- as.formula(sprintf(
    "diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(%s,1) + diff(l_wpi) + diff(l_emp)", tvar))
  plm(f, data = data, model = "within", effect = "individual")
}

variants <- c("tightness", "tightness_no_chk", "tightness_no_stu", "tightness_rw")
labels   <- c("omnibus (net migration)", "overseas only",
              "overseas ex-students", "renter-weighted overseas")

# (a) Each variant on its OWN full sample.
cat("\n--- 1a. Each numerator on its own sample ---\n")
fits_own <- list()
for (k in seq_along(variants)) {
  m <- rent_spec(variants[k], P)
  fits_own[[variants[k]]] <- m
  cat(sprintf("  %-26s  R2=%.4f  %s\n", labels[k], r2(m),
              co(m, sprintf("dplyr::lag(%s, 1)", variants[k]))))
  gout(dk_coeftest(m), sprintf("granular_rent_%s.txt", variants[k]))
}

# (b) Like-for-like: restrict to rows where ALL variants are observable, then
#     re-fit each so R2 differences reflect the numerator, not the sample.
cat("\n--- 1b. Common-sample horse-race (renter variants define the window) ---\n")
need <- c("l_rent","l_pr_gap","l_wpi","l_emp",
          "tightness","tightness_no_chk","tightness_no_stu","tightness_rw")
d_lag <- d |> arrange(city, quarter) |> group_by(city) |>
  mutate(across(all_of(c("tightness","tightness_no_chk","tightness_no_stu","tightness_rw")),
                ~dplyr::lag(.x), .names = "L_{.col}")) |> ungroup()
common <- d_lag |>
  filter(if_all(c(L_tightness, L_tightness_no_chk, L_tightness_no_stu, L_tightness_rw,
                  l_rent, l_pr_gap, l_wpi, l_emp), is.finite))
Pc <- pdata.frame(common, index = c("city","quarter"))
cat(sprintf("  common sample: %d city-quarters\n", nrow(common)))
for (k in seq_along(variants)) {
  m <- rent_spec(variants[k], Pc)
  cat(sprintf("  %-26s  R2=%.4f  %s\n", labels[k], r2(m),
              co(m, sprintf("dplyr::lag(%s, 1)", variants[k]))))
}
cat("  Reading: if a renter-relevant numerator (overseas-only / ex-students /\n")
cat("  renter-weighted) lifts within-R2 MATERIALLY over the omnibus on the SAME\n")
cat("  rows, the single net-migration ratio is throwing away rent-relevant signal\n")
cat("  by weighting every migrant equally. If R2 is flat and the coefficient is\n")
cat("  stable, the omnibus tightness is sufficient and granularity adds nothing.\n")

# =============================================================================
# 2. UNBUNDLING: overseas vs interstate net flows with SEPARATE slopes
#    Headline forces over_dw and inter_dw to share one coefficient (their sum is
#    the tightness numerator). Enter them separately; Wald-test slope equality.
#    A rejection means the omnibus ratio masks a genuine overseas/interstate wedge
#    in how migration pressure reaches rents.
# =============================================================================
cat("\n############  2. RENT: OVERSEAS vs INTERSTATE SLOPE WEDGE  ############\n")
rent_split <- plm(
  diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(over_dw,1) + dplyr::lag(inter_dw,1) +
    diff(l_wpi) + diff(l_emp),
  data = P, model = "within", effect = "individual")
print(dk_coeftest(rent_split)); gout(dk_coeftest(rent_split), "granular_rent_split_flows.txt")
cat("  overseas slope :", co(rent_split, "dplyr::lag(over_dw, 1)"), "\n")
cat("  interstate slope:", co(rent_split, "dplyr::lag(inter_dw, 1)"), "\n")

# Wald test that the two slopes are equal, under DK vcov. linearHypothesis from
# car if available; else a manual contrast.
eqtest <- try({
  car::linearHypothesis(rent_split,
                        "dplyr::lag(over_dw, 1) = dplyr::lag(inter_dw, 1)", vcov. = dk_vcov(rent_split))
}, silent = TRUE)
if (!inherits(eqtest, "try-error")) {
  print(eqtest); gout(eqtest, "granular_rent_split_waldeq.txt")
} else {
  V <- dk_vcov(rent_split); b <- coef(rent_split)
  i1 <- "dplyr::lag(over_dw, 1)"; i2 <- "dplyr::lag(inter_dw, 1)"
  L <- setNames(rep(0, length(b)), names(b)); L[i1] <- 1; L[i2] <- -1
  est <- sum(L*b); se <- sqrt(as.numeric(t(L) %*% V %*% L))
  cat(sprintf("  (manual contrast) overseas - interstate = %+.4f (se %.4f, t=%.2f)\n",
              est, se, est/se))
}
cat("  Reading: reject equality => overseas and interstate migration reach rents\n")
cat("  with different slopes, and collapsing them into one tightness ratio is a\n")
cat("  restriction the data dislike. Fail to reject => the pooled ratio is fine.\n")

## 2b. Leave-one-city-out robustness on the wedge ----------------------------
## Interstate net migration nets to ~zero across the five cities each quarter
## (one city's outflow is another's inflow), so the wedge is identified off
## within-city timing and could be carried by a single city's swing (e.g. the
## pandemic-era Brisbane inflow / Sydney-Melbourne outflow, or the Perth mining
## cycle). Re-estimate the split, dropping each city in turn, and report the
## overseas slope, the interstate slope, and the overseas-minus-interstate
## contrast with its DK SE. A wedge that survives every drop is structural; one
## that collapses when a particular city leaves is that city's artefact.
cat("\n--- 2b. Leave-one-city-out: overseas/interstate wedge stability ---\n")
contrast_dk <- function(m) {
  V <- dk_vcov(m); b <- coef(m)
  i1 <- "dplyr::lag(over_dw, 1)"; i2 <- "dplyr::lag(inter_dw, 1)"
  L <- setNames(rep(0, length(b)), names(b)); L[i1] <- 1; L[i2] <- -1
  est <- sum(L * b); se <- sqrt(as.numeric(t(L) %*% V %*% L))
  c(over = unname(b[i1]), inter = unname(b[i2]), diff = est, se = se, t = est / se)
}
loo_rows <- lapply(c("(none)", CITIES), function(drop_city) {
  dd <- if (drop_city == "(none)") d else filter(d, city != drop_city)
  Pd <- pdata.frame(dd, index = c("city","quarter"))
  m  <- plm(diff(l_rent) ~ dplyr::lag(l_pr_gap,1) + dplyr::lag(over_dw,1) +
              dplyr::lag(inter_dw,1) + diff(l_wpi) + diff(l_emp),
            data = Pd, model = "within", effect = "individual")
  cc <- contrast_dk(m)
  data.frame(dropped = drop_city, over = cc["over"], inter = cc["inter"],
             diff = cc["diff"], se = cc["se"], t = cc["t"], row.names = NULL)
})
loo <- do.call(rbind, loo_rows)
print(transform(loo,
                over  = round(over, 4), inter = round(inter, 4),
                diff  = round(diff, 4), se = round(se, 4), t = round(t, 2)))
gout(loo, "granular_rent_split_loo.txt")
cat("  Reading: scan the 'diff' (overseas - interstate) and 't' columns down the\n")
cat("  drops. If the contrast stays negative with |t| around 2 throughout, the\n")
cat("  wedge is not a single city's artefact. If dropping one city sends |t| well\n")
cat("  below 2 or flips the sign, that city carries the result and the wedge\n")
cat("  should be reported as driven by it, not as a general pattern.\n")

# =============================================================================
# 3. PRICE-SIDE PLACEBO (levels). No granular migration flow should carry
#    independent power in the price equation. Add the renter-weighted overseas
#    tightness to the headline price spec; expect a null. A non-null would mean
#    migration is leaking into prices, undercutting the rent/price separation.
# =============================================================================
cat("\n############  3. PRICE: GRANULAR-MIGRATION PLACEBO  ############\n")
price_base <- plm(
  diff(l_price) ~ diff(cash_rate) + diff(l_inv) + dplyr::lag(tightness,1) + diff(l_wpi),
  data = P, model = "within", effect = "individual")
price_plac <- plm(
  diff(l_price) ~ diff(cash_rate) + diff(l_inv) + dplyr::lag(tightness,1) +
    dplyr::lag(tightness_rw,1) + diff(l_wpi),
  data = P, model = "within", effect = "individual")
cat("  baseline tightness   :", co(price_base, "dplyr::lag(tightness, 1)"), "\n")
cat("  + renter-wtd overseas:", co(price_plac, "dplyr::lag(tightness_rw, 1)"),
    "(expect null)\n")
print(dk_coeftest(price_plac)); gout(dk_coeftest(price_plac), "granular_price_placebo.txt")
cat("  Reading: a null renter-weighted-overseas term in the PRICE equation keeps\n")
cat("  migration on the rent side, consistent with the paper's separation. A\n")
cat("  significant term would be evidence the separation is leakier than claimed.\n")

## 3b. Non-overlapping placebo (drop the collinear omnibus tightness) ---------
## The 3a placebo puts BOTH omnibus tightness and renter-weighted overseas in one
## equation. They share the overseas net-migration component, so they are
## collinear, and the marginal renter-weighted coefficient there is the part of
## the renter-weighted flow ORTHOGONAL to omnibus tightness -- a strange object
## that also threw the wpi coefficient to an implausible -1.6. The clean placebo
## enters the renter-weighted overseas tightness ALONE (no omnibus term), so the
## coefficient is the total association of renter-skewed migration with price
## growth, not a residual. If THIS is null, the marginal 3a result was a
## collinearity artefact and the separation holds; if it survives, the leak is real.
cat("\n--- 3b. Non-overlapping placebo: renter-weighted overseas alone ---\n")
price_plac2 <- plm(
  diff(l_price) ~ diff(cash_rate) + diff(l_inv) + dplyr::lag(tightness_rw,1) + diff(l_wpi),
  data = P, model = "within", effect = "individual")
cat("  renter-wtd overseas (alone):", co(price_plac2, "dplyr::lag(tightness_rw, 1)"),
    "(expect null)\n")
cat("  wpi coefficient (sanity)   :", co(price_plac2, "diff(l_wpi)"), "\n")
print(dk_coeftest(price_plac2)); gout(dk_coeftest(price_plac2), "granular_price_placebo_alone.txt")
cat("  Reading: compare with 3a. If the term goes null (and wpi returns to a\n")
cat("  sane magnitude) once the collinear omnibus tightness is removed, the\n")
cat("  marginal 3a result was an artefact of the overlap, and the price equation\n")
cat("  carries no independent granular-migration signal -- the placebo passes.\n")

cat("\n############  MIGRATION GRANULARITY DIAGNOSTIC COMPLETE  ############\n")
cat("Wrote granular_rent_*.txt, granular_rent_split_flows.txt,\n",
    "     granular_rent_split_waldeq.txt, granular_rent_split_loo.txt,\n",
    "     granular_price_placebo.txt, granular_price_placebo_alone.txt to",
    OUTDIR, "/\n")
cat("\nInterpretation guide:\n")
cat("  * If 1b shows flat R2 and 2 fails to reject and 3 is null, the SINGLE\n")
cat("    tightness ratio is doing all the work granular migration could do: report\n")
cat("    this as a robustness paragraph confirming the omnibus measure suffices.\n")
cat("  * If 1b lifts R2 for a renter-relevant numerator OR 2 rejects equality, the\n")
cat("    granularity is informative: consider reporting the better numerator as an\n")
cat("    alternative tightness, with the five-cluster SE caveat from 7.4 and the\n")
cat("    shorter COVID-guarded window noted.\n")