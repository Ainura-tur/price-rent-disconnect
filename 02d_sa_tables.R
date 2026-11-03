# =============================================================================
# 02d_sa_tables.R -- appendix tables and checks on seasonally adjusted credit
#
#   S1  Stacked test of equal price and rent slopes on credit (Table C.1 text)
#   S2  Break tests at dated breaks and a sup-Wald search (Table C.2)
#   S3  Granger tests in both directions (Table C.3)
#   S4  Seasonal adjustment: unadjusted-series comparisons (Appendix C, Section 4.1.1)
#   S5  Sydney-specific and city-by-city post-2017 changes (Appendix E)
#   S6  Cash-rate interaction and cash-rate level (Appendix E, Section 5.6.4)
#   S7  Dropping the 2015:Q3-2016:Q2 reclassification quarters (Appendix E)
#   S8  Summary statistics (Tables 1 and 2)
#
# Reads clean/panel_combined.rds (01_build_panel.R). Writes output/sa2_*.
# Standard errors: Driscoll-Kraay (HC0, maxlag 4) unless stated.
# =============================================================================

source("00_setup.R")
suppressPackageStartupMessages({
  library(plm); library(lmtest); library(sandwich); library(car)
})

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select    <- dplyr::select;    filter    <- dplyr::filter
mutate    <- dplyr::mutate;    slice     <- dplyr::slice
recode    <- dplyr::recode;    rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange   <- dplyr::arrange;   count     <- dplyr::count
lag       <- dplyr::lag

dkv <- function(m) plm::vcovSCC(m, type = "HC0", maxlag = 4)
ct  <- function(m) lmtest::coeftest(m, vcov = dkv)
L   <- function(x, k = 1) dplyr::lag(x, k)
out <- function(obj, name) capture.output(print(obj), file = file.path(OUTDIR, paste0("sa2_", name)))
pd  <- function(df, id = "city") pdata.frame(df, index = c(id, "quarter"))
fe  <- function(f, df, id = "city", effect = "individual")
  plm(f, data = pd(df, id), model = "within", effect = effect)
row <- function(m, term) {   # matches interaction terms whatever their order
  x <- ct(m); key <- function(t) paste(sort(strsplit(t, ":")[[1]]), collapse = ":")
  i <- match(key(term), sapply(rownames(x), key)); stopifnot(!is.na(i))
  c(b = x[i, 1], se = x[i, 2], p = x[i, 4])
}
lin <- function(m, terms) {   # sum of coefficients with its DK standard error and p-value
  b <- coef(m)[terms]; V <- dkv(m)[terms, terms, drop = FALSE]
  s <- sum(b); se <- sqrt(sum(V)); c(b = s, se = se, p = 2 * pnorm(-abs(s / se)))
}

# ---- Data -------------------------------------------------------------------
d <- readRDS(file.path(CLEAN, "panel_combined.rds")) |> arrange(city, quarter) |>
  group_by(city) |>
  mutate(dp = log(prop_price) - L(log(prop_price)), dr = log(rents) - L(log(rents)),
         di = log(inv_commit_sa) - L(log(inv_commit_sa)),          # seasonally adjusted
         du = log(inv_commit) - L(log(inv_commit)),                # unadjusted
         dw = log(wpi) - L(log(wpi)), dn = log(employment) - L(log(employment)),
         dc = cash_rate - L(cash_rate), dc_l1 = L(dc), di_l1 = L(di), du_l1 = L(du),
         dp_l1 = L(dp), tight_l1 = L(tightness), cash_l1 = L(cash_rate)) |>
  ungroup() |>
  mutate(post = as.integer(quarter >= as.yearqtr("2017 Q1")),
         covid = as.integer(quarter >= as.yearqtr("2020 Q1")),
         syd = as.integer(city == "Sydney"),
         qoy = factor(cycle(quarter)))
est <- d |> filter(is.finite(tight_l1), is.finite(dr), is.finite(dp), is.finite(dw), is.finite(di))
cat("Estimation sample:", nrow(est), "city-quarters\n")

# Table C.1 specifications (current and lagged credit).
ctl_p <- function(lagged) if (lagged) "dc_l1 + tight_l1 + dw" else "dc + tight_l1 + dw"
ctl_r <- function(lagged) paste(ctl_p(lagged), "+ dn")
cr    <- function(lagged, v = "di") if (lagged) paste0(v, "_l1") else v

# ---- S1. Stacked test of equal pre-2017 slopes --------------------------------
# Both equations get the same controls (cash-rate change, tightness, wages,
# employment, the post-2017 indicator and its credit interaction); each
# coefficient differs by equation; DK errors by quarter cover both equations.
stack <- function(df, rhs) {
  s <- bind_rows(df |> mutate(y = dp, eq = "price"), df |> mutate(y = dr, eq = "rent")) |>
    mutate(rent = as.integer(eq == "rent"), id = paste(city, eq))
  vars <- trimws(strsplit(rhs, "\\+")[[1]])
  for (v in vars) s[[paste0(v, "_R")]] <- s[[v]] * s$rent
  f <- as.formula(paste("y ~", paste(c(vars, paste0(vars, "_R")), collapse = " + ")))
  list(m = fe(f, s, id = "id"), vars = vars)
}
S1 <- lapply(c(cur = FALSE, lag = TRUE), function(lg) {
  x <- cr(lg); est2 <- est |> mutate(int = .data[[x]] * post, cred = .data[[x]])
  st <- stack(est2, paste("cred + int + post +", ctl_r(lg)))
  row(st$m, "cred_R")    # rent slope minus price slope, before 2017
})
cat("\n== S1 stacked: rent minus price slope before 2017 ==\n"); print(S1, digits = 3)
out(S1, "S1_stacked.txt")

# ---- S2. Break tests (Table C.2) ----------------------------------------------
# Same four controls in both equations: cash-rate change (lagged with lagged
# credit), lagged tightness, wage growth, employment growth.
ctl4 <- function(lagged) paste(if (lagged) "dc_l1" else "dc", "+ tight_l1 + dw + dn")
brk <- function(tau, lagged, df = est) {
  x <- cr(lagged)
  e <- df |> mutate(B = as.integer(quarter >= tau), cred = .data[[x]], int = cred * B)
  rhs <- paste("cred + int + B +", ctl4(lagged))
  mp <- fe(as.formula(paste("dp ~", rhs)), e); mr <- fe(as.formula(paste("dr ~", rhs)), e)
  st <- stack(e, rhs)
  rbind(price = row(mp, "int"), rent = row(mr, "int"), rent_minus_price = row(st$m, "int_R"))
}
dated <- c("2015 Q1", "2017 Q1", "2017 Q2")
S2a <- do.call(rbind, lapply(dated, function(q) do.call(rbind, lapply(c(FALSE, TRUE), function(lg) {
  r <- brk(as.yearqtr(q), lg)
  data.frame(break_q = q, credit = ifelse(lg, "lagged", "current"), eq = rownames(r), r, row.names = NULL)
}))))
cat("\n== S2 dated breaks ==\n"); print(S2a, digits = 3)
cands <- as.yearqtr(seq(as.numeric(as.yearqtr("2010 Q3")), as.numeric(as.yearqtr("2023 Q1")), 0.25))
supw <- do.call(rbind, lapply(c(FALSE, TRUE), function(lg) do.call(rbind, lapply(c("dp", "dr"), function(y) {
  w <- t(sapply(cands, function(q) {
    x <- cr(lg); e <- est |> mutate(B = as.integer(quarter >= q), cred = .data[[x]], int = cred * B)
    r <- row(fe(as.formula(paste(y, "~ cred + int + B +", ctl4(lg))), e), "int")
    c(wald = unname((r["b"] / r["se"])^2), change = unname(r["b"]))
  }))
  k <- which.max(w[, "wald"])
  data.frame(eq = ifelse(y == "dp", "price", "rent"), credit = ifelse(lg, "lagged", "current"),
             sup_wald = w[k, "wald"], date = format(cands[k]), change = w[k, "change"],
             n_candidates = length(cands))
}))))
cat("\n== S2 sup-Wald, 2010:Q3-2023:Q1 (Andrews 15% trimming, 1 restriction: 7.12 / 8.68 / 12.16) ==\n")
print(supw, digits = 3)
out(list(dated = S2a, supwald = supw), "S2_breaks.txt")

# ---- S3. Granger tests (Table C.3) --------------------------------------------
# Common sample: city-quarters that hold six lags of every variable.
gd <- d |> group_by(city) |>
  mutate(across(c(dp, dr, di), list(l1 = ~L(.x, 1), l2 = ~L(.x, 2), l3 = ~L(.x, 3),
                                    l4 = ~L(.x, 4), l5 = ~L(.x, 5), l6 = ~L(.x, 6)))) |>
  ungroup() |> filter(quarter <= TARGET_END) |>
  filter(if_all(c(dp, dr, di, matches("_l[1-6]$")), is.finite))
cat("\nGranger sample:", nrow(gd), "city-quarters,", format(min(gd$quarter)), "to", format(max(gd$quarter)), "\n")
granger <- function(y, x, k = 4, twoway = FALSE) {
  own <- paste0(y, "_l", 1:k); oth <- paste0(x, "_l", 1:k)
  f <- as.formula(paste(y, "~", paste(c(own, oth), collapse = " + "), if (!twoway) "+ qoy"))
  m <- fe(f, gd, effect = if (twoway) "twoways" else "individual")
  w <- car::linearHypothesis(m, paste(oth, "= 0"), vcov. = dkv(m), test = "Chisq")
  c(wald = w$Chisq[2], p = w$`Pr(>Chisq)`[2], lin(m, oth)[c("b", "se")])
}
pairs <- list(c("dp", "di"), c("di", "dp"), c("dr", "di"), c("di", "dr"))
S3 <- do.call(rbind, lapply(pairs, function(pr) {
  g4 <- granger(pr[1], pr[2])
  data.frame(test = paste(pr[2], "->", pr[1]), wald4 = g4["wald"], p4 = g4["p"],
             sum = g4["b"], sum_se = g4["se"], p2 = granger(pr[1], pr[2], 2)["p"],
             p6 = granger(pr[1], pr[2], 6)["p"], p4_qfe = granger(pr[1], pr[2], 4, TRUE)["p"],
             n = nrow(gd), row.names = NULL)
}))
cat("\n== S3 Granger ==\n"); print(S3, digits = 3)
out(S3, "S3_granger.txt")

# ---- S4. Seasonal adjustment: unadjusted-series comparisons --------------------
sea_r2 <- function(v, df) summary(lm(as.formula(paste(v, "~ qoy + city")), df))$r.squared
S4 <- list(
  r2_quarter_dummies = c(unadjusted = sea_r2("du", est), adjusted = sea_r2("di", est)),
  r2_unadjusted_pre_post = c(pre = sea_r2("du", filter(est, post == 0)), post = sea_r2("du", filter(est, post == 1))),
  sd_growth = c(unadjusted = sd(est$du), adjusted = sd(est$di)),   # estimation sample
  unadjusted_interaction = row(fe(as.formula(paste("dp ~ du*post +", ctl_p(FALSE))), est), "du:post"),
  slope_ratio_unadj_to_adj = local({   # from the Table C.1 interaction models
    bu <- coef(fe(as.formula(paste("dp ~ du*post +", ctl_p(FALSE))), est))
    ba <- coef(fe(as.formula(paste("dp ~ di*post +", ctl_p(FALSE))), est))
    c(pre = unname(bu["du"] / ba["di"]),
      post = unname((bu["du"] + bu["du:post"]) / (ba["di"] + ba["di:post"]))) }),
  unadjusted_lagged_with_momentum = row(fe(dp ~ dc_l1 + du_l1 + tight_l1 + dp_l1, est), "du_l1"))
cat("\n== S4 seasonal adjustment ==\n"); print(S4, digits = 3)
out(S4, "S4_seasonal.txt")

# ---- S5. Sydney and city-by-city post-2017 changes -----------------------------
base_p <- paste("di + di:post + post +", ctl_p(FALSE))
S5 <- list(
  sydney_term = ct(fe(as.formula(paste("dp ~", base_p, "+ di:post:syd")), est)),
  sydney_pre_slope = row(fe(as.formula(paste("dp ~", base_p, "+ di:syd")), est), "di:syd"),
  sydney_with_own_slope = row(fe(as.formula(paste("dp ~", base_p, "+ di:syd + di:post:syd")), est), "di:post:syd"),
  sydney_from_2020 = row(fe(as.formula(paste("dp ~", base_p, "+ di:post:syd + covid + di:covid + di:covid:syd")), est), "di:covid:syd"),
  sydney_term_unadjusted = row(fe(as.formula(paste("dp ~ du + du:post + post +", ctl_p(FALSE), "+ du:post:syd")), est), "du:post:syd"))
city_nw <- do.call(rbind, lapply(CITIES, function(c) {
  m <- lm(dp ~ di * post + dc + tight_l1 + dw, data = filter(est, city == c))
  x <- lmtest::coeftest(m, vcov = sandwich::NeweyWest(m, lag = 4, prewhite = FALSE))
  mu <- lm(dp ~ du * post + dc + tight_l1 + dw, data = filter(est, city == c))
  data.frame(city = c, change = x["di:post", 1], p = x["di:post", 4],
             change_unadjusted = unname(coef(mu)["du:post"]))
}))
S5$city_by_city <- city_nw
cat("\n== S5 Sydney and city by city ==\n"); print(S5, digits = 3)
out(S5, "S5_sydney_cities.txt")

# ---- S6. Cash-rate interaction and level ---------------------------------------
cash_mean <- mean(est$cash_l1, na.rm = TRUE)
e6 <- est |> mutate(cdev = cash_l1 - cash_mean)
S6_int <- do.call(rbind, lapply(c(FALSE, TRUE), function(lg) {
  x <- cr(lg); cc <- if (lg) "dc_l1" else "dc"
  e <- e6 |> mutate(cred = .data[[x]], cash = .data[[cc]])
  mp <- fe(as.formula(paste("dp ~ cred*post + cash:post + cash + tight_l1 + dw")), e)
  mr <- fe(as.formula(paste("dr ~ cred*post + cash:post + cash + tight_l1 + dw + dn")), e)
  data.frame(credit = ifelse(lg, "lagged", "current"),
             price_int = row(mp, "cred:post")["b"], price_p = row(mp, "cred:post")["p"],
             rent_int = row(mr, "cred:post")["b"], rent_p = row(mr, "cred:post")["p"],
             cash_pre = coef(mp)["cash"], cash_post = sum(coef(mp)[c("cash", "post:cash")]),
             cash_change_p = row(mp, "post:cash")["p"], row.names = NULL)
}))
lvl <- function(y, lg, extra = "") {
  x <- cr(lg); cc <- if (lg) "dc_l1" else "dc"
  e <- e6 |> mutate(cred = .data[[x]], cash = .data[[cc]])
  ctl <- if (y == "dp") "cash + tight_l1 + dw" else "cash + tight_l1 + dw + dn"
  m <- fe(as.formula(paste(y, "~ cred + cred:cdev + cdev +", ctl, extra)), e)
  b <- coef(m); at <- function(r) unname(b["cred"] + b["cred:cdev"] * (r - cash_mean))
  c(int = row(m, "cred:cdev"), slope_mean = row(m, "cred"), slope_at_0.1 = at(0.1), slope_at_4.35 = at(4.35),
    cdev = unname(b["cdev"]))
}
S6_lvl <- list(mean_cash = cash_mean,
  price_cur = lvl("dp", FALSE), price_lag = lvl("dp", TRUE),
  rent_cur = lvl("dr", FALSE), rent_lag = lvl("dr", TRUE),
  price_cur_with_post = lvl("dp", FALSE, "+ post + cred:post"),
  price_lag_with_post = lvl("dp", TRUE, "+ post + cred:post"),
  price_cur_with_covid = lvl("dp", FALSE, "+ covid + cred:covid"),
  price_lag_with_covid = lvl("dp", TRUE, "+ covid + cred:covid"),
  price_cur_unadjusted = { e <- e6 |> mutate(cred = du, cash = dc)
    row(fe(dp ~ cred + cred:cdev + cdev + cash + tight_l1 + dw, e), "cred:cdev") })
cat("\n== S6 cash rate ==\n"); print(S6_int, digits = 3); print(S6_lvl, digits = 3)
out(list(interaction = S6_int, level = S6_lvl), "S6_cash.txt")

# ---- S7. Dropping the reclassification quarters -------------------------------
recl <- as.yearqtr(c("2015 Q3", "2015 Q4", "2016 Q1", "2016 Q2"))
S7 <- row(fe(as.formula(paste("dp ~ di*post +", ctl_p(FALSE))), filter(est, !quarter %in% recl)), "di:post")
cat("\n== S7 without 2015:Q3-2016:Q2 ==\n"); print(S7, digits = 3)
out(S7, "S7_reclass.txt")

# ---- S8. Summary statistics (Tables 1 and 2) -----------------------------------
win <- d |> filter(quarter >= as.yearqtr("2006 Q3"), quarter <= TARGET_END)
t2 <- function(v, df = win) { x <- df[[v]][is.finite(df[[v]])]
  c(N = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x)) }
S8 <- list(
  table2 = rbind(rent = t2("dr"), price = t2("dp"), credit_sa = t2("di"), credit_unadj = t2("du"),
                 wpi = t2("dw"), employment = t2("dn"), cash = t2("dc"),
                 tightness = t2("tightness", filter(win, is.finite(tight_l1)) |> mutate(tightness = tight_l1)),
                 gap = t2("gap", win |> mutate(gap = log(price_rent)))),   # level series
  table1 = win |> arrange(city, quarter) |> group_by(city) |>
    mutate(rc = zoo::rollapplyr(cbind(dp, dr), 20, function(m) cor(m[, 1], m[, 2]),
                                by.column = FALSE, fill = NA)) |>   # 20-quarter rolling growth correlation
    summarise(rent_mean = mean(rents, na.rm = TRUE), price_mean = mean(prop_price, na.rm = TRUE),
              corr_pre = rc[quarter == as.yearqtr("2016 Q4")], corr_post = rc[quarter == TARGET_END],
              corr_change = corr_post - corr_pre,
              nom_k = mean(net_overseas, na.rm = TRUE) / 1000, nim_k = mean(net_interstate, na.rm = TRUE) / 1000,
              gfcf_pub_bn = mean(gfcf_public, na.rm = TRUE) / 1000, gfcf_priv_bn = mean(gfcf_private, na.rm = TRUE) / 1000,
              pub_priv = gfcf_pub_bn / gfcf_priv_bn, emp_m = mean(employment, na.rm = TRUE) / 1000,
              wpi_mean = mean(wpi, na.rm = TRUE), n = n(), .groups = "drop"))
cat("\n== S8 summary statistics ==\n"); print(S8, digits = 4)
out(S8, "S8_summary.txt")
cat("\nDone.\n")
