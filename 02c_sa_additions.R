# =============================================================================
# 02c_sa_additions.R  --  revision analyses on the seasonally adjusted credit
#                         measure used in the main text
#
#   A  Price equation, with price momentum in columns 2 and 4 (Table 4)
#   B  Dynamic (Calvo) rent equation: omega and long-run coefficients
#   C  Local projections with labour-market controls; post-impact projections
#   D  Tenure-channel test: lagged credit given lagged price growth
#   E  Implied omega*kappa*eta
#   F  Owner-occupier credit in the price equation
#   G  Shift-share IV: own investor share x outside-sample credit growth
#   H  Sample ending in 2021:Q4 (before the price splice)
#   I  Rent-measurement indicators (moratoria, Rent Assistance increases)
#
# The wage index, net migration and completions are dated by the quarter they
# describe in clean/panel_combined.rds (01_build_panel.R), so no re-dating is
# applied here.
#
# Reads clean/panel_combined.rds and the ABS lending file. Writes output/sa_*.
# Standard errors: Driscoll-Kraay (HC0, maxlag 4).
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


dkv <- function(m) plm::vcovSCC(m, type = "HC0", maxlag = 4)
ct  <- function(m) lmtest::coeftest(m, vcov = dkv)
L   <- function(x, k = 1) dplyr::lag(x, k)
out <- function(obj, name) capture.output(print(obj), file = file.path(OUTDIR, paste0("sa_", name)))

# ---- Lending: seasonally adjusted investor and owner-occupier commitments ----
lend <- read_csv(F_LEND, show_col_types = FALSE) |>
  filter(Measure == "Value", `Loan Purpose` == "Total dwellings excluding refinancing",
         `Housing Purpose` %in% c("Investor", "Owner occupier")) |>
  mutate(quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)), v = as.numeric(OBS_VALUE)) |>
  distinct(quarter, Region, `Housing Purpose`, `Adjustment Type`, .keep_all = TRUE) |>
  select(quarter, Region, hp = `Housing Purpose`, adj = `Adjustment Type`, v)
st <- lend |> filter(Region %in% names(STATE2CITY), adj == "Seasonally Adjusted") |>
  mutate(city = recode(Region, !!!STATE2CITY)) |>
  tidyr::pivot_wider(id_cols = c(city, quarter), names_from = hp, values_from = v) |>
  rename(inv_sa = Investor, oo_sa = `Owner occupier`)
rest <- lend |> filter(Region %in% c("Tasmania", "Northern Territory", "Australian Capital Territory"),
                       hp == "Investor", adj == "Seasonally Adjusted") |>
  group_by(quarter) |> summarise(v = sum(v), n = n(), .groups = "drop") |> filter(n == 3) |>
  arrange(quarter) |> mutate(g_rest = log(v) - L(log(v))) |> select(quarter, g_rest)

# ---- Panel ------------------------------------------------------------------
d <- readRDS(file.path(CLEAN, "panel_combined.rds")) |>
  left_join(st, by = c("city", "quarter")) |> arrange(city, quarter) |>
  group_by(city) |>
  mutate(tight = tightness,
         l_rent = log(rents), l_price = log(prop_price),
         dr = l_rent - L(l_rent), dp = l_price - L(l_price),
         dw = log(wpi) - L(log(wpi)), dn = log(employment) - L(log(employment)),
         di = log(inv_sa) - L(log(inv_sa)), doo = log(oo_sa) - L(log(oo_sa)),
         dc = cash_rate - L(cash_rate),
         tight_l1 = L(tight), gap_l1 = L(log(price_rent)),
         dr_l1 = L(dr), dp_l1 = L(dp), di_l1 = L(di), dc_l1 = L(dc), doo_l1 = L(doo)) |>
  ungroup() |>
  mutate(post = as.integer(quarter >= as.yearqtr("2017 Q1")),
         qid = as.integer(factor(quarter)),
         mora = as.integer(quarter >= as.yearqtr("2020 Q2") & quarter <= as.yearqtr("2020 Q4")),
         # Rent Assistance rose from 20 September 2023 and 20 September 2024
         cra  = as.integer(quarter %in% as.yearqtr(c("2023 Q3", "2024 Q3"))))
pd  <- function(df) pdata.frame(df, index = c("city", "quarter"))
fe  <- function(f, df) plm(f, data = pd(df), model = "within")
est <- d |> filter(is.finite(tight_l1), is.finite(dr), is.finite(dp), is.finite(dw), is.finite(di))
cat("Estimation sample:", nrow(est), "city-quarters\n")

# ---- A. Price equation with momentum -----------------------------------------
A <- list(cur   = fe(dp ~ dc + di + tight_l1, est),
          cur_m = fe(dp ~ dc + di + tight_l1 + dp_l1, est),
          lag   = fe(dp ~ dc_l1 + di_l1 + tight_l1, est),
          lag_m = fe(dp ~ dc_l1 + di_l1 + tight_l1 + dp_l1, est))
cat("\n== A price equation ==\n")
for (n in names(A)) { cat(n, " within-R2 =", round(summary(A[[n]])$r.squared["rsq"], 3), "\n"); print(ct(A[[n]])) }
out(lapply(A, ct), "A_price.txt")

# ---- B. Dynamic rent equation ------------------------------------------------
B0 <- fe(dr ~ gap_l1 + tight_l1 + dw + dn, est)
B  <- fe(dr ~ dr_l1 + gap_l1 + tight_l1 + dw + dn, est)
V <- dkv(B); b <- coef(B); om <- 1 - b["dr_l1"]
lr <- sapply(c("tight_l1", "dw", "dn", "gap_l1"), function(v) {
  J <- setNames(rep(0, length(b)), names(b)); J[v] <- 1 / om; J["dr_l1"] <- b[v] / om^2
  c(lr = unname(b[v] / om), se = sqrt(drop(t(J) %*% V %*% J)))
})
BD <- fe(dr ~ dr_l1 + gap_l1 + tight_l1 + dw + dn + mora + cra, est)
cat("\n== B dynamic rent equation ==\n"); print(ct(B0)); print(ct(B))
cat(sprintf("omega = %.3f (se %.3f); within-R2 static %.3f, dynamic %.3f\n", om, sqrt(V["dr_l1", "dr_l1"]),
            summary(B0)$r.squared["rsq"], summary(B)$r.squared["rsq"]))
print(round(lr, 4)); cat("-- with indicators --\n"); print(ct(BD))
out(list(static = ct(B0), dynamic = ct(B), omega = om, longrun = lr, indicators = ct(BD)), "B_rent.txt")
write_csv(data.frame(omega = unname(om), se = sqrt(V["dr_l1", "dr_l1"]),
                     lr_wage = lr["lr", "dw"], lr_wage_se = lr["se", "dw"]),
          file.path(OUTDIR, "sa_B_omega.csv"))   # read by 04_model_matching.R

# ---- C. Local projections ----------------------------------------------------
H <- 12; z90 <- 1.645
dl <- d
for (j in 1:4) dl <- dl |> group_by(city) |>
  mutate("di_l{j}" := L(di, j), "dp_l{j}" := L(dp, j), "dr_l{j}" := L(dr, j),
         "dw_l{j}" := L(dw, j), "dn_l{j}" := L(dn, j)) |> ungroup()
for (h in 0:H) dl <- dl |> group_by(city) |>
  mutate("yp_{h}" := dplyr::lead(l_price, h) - L(l_price), "yr_{h}" := dplyr::lead(l_rent, h) - L(l_rent),
         "yc_{h}" := dplyr::lead(log(inv_sa), h) - L(log(inv_sa)),
         "fp_{h}" := dplyr::lead(l_price, h) - l_price,    "fr_{h}" := dplyr::lead(l_rent, h) - l_rent) |>
  ungroup()
sd_sh <- sd(est$di)
lab <- c(paste0("dw_l", 1:4), paste0("dn_l", 1:4))
lp <- function(y, ctrl, hs = 0:H, df = dl, label)
  bind_rows(lapply(hs, function(h) {
    m <- plm(as.formula(paste0(y, "_", h, " ~ di + ", paste(ctrl, collapse = " + "))),
             data = pdata.frame(df, index = c("city", "qid")), model = "within")
    data.frame(spec = label, h = h, beta = coef(m)["di"] * sd_sh,
               se = sqrt(dkv(m)["di", "di"]) * sd_sh, n = nobs(m))
  }))
base <- function(own) c(paste0("di_l", 1:4), paste0(own, "_l", 1:4), "tight_l1", "dc_l1")
pre <- filter(dl, quarter <= as.yearqtr("2021 Q4"))
C <- bind_rows(
  lp("yp", base("dp"), label = "Price, baseline"),
  lp("yr", base("dr"), label = "Rent, baseline"),
  lp("yc", base("dp"), label = "Credit, baseline"),
  lp("yp", c(base("dp"), lab), label = "Price, labour controls"),
  lp("yr", c(base("dr"), lab), label = "Rent, labour controls"),
  lp("yr", c(base("dr"), lab, "dw", "dn"), label = "Rent, labour controls incl. current"),
  lp("fp", c(base("dp"), lab, "dp"), hs = 1:H, label = "Price, post-impact"),
  lp("fr", c(base("dr"), lab, "dp", "dr", paste0("dp_l", 1:4)), hs = 1:H, label = "Rent, post-impact"),
  lp("yp", base("dp"), df = pre, label = "Price, baseline, to 2021Q4"),
  lp("yr", base("dr"), df = pre, label = "Rent, baseline, to 2021Q4"))
C$lo <- C$beta - z90 * C$se; C$hi <- C$beta + z90 * C$se
cat("\n== C local projections (sd shock =", round(sd_sh, 4), ") ==\n")
print(C |> filter(h %in% c(0, 1, 2, 4, 8, 12)), digits = 3, row.names = FALSE)
write_csv(C, file.path(OUTDIR, "sa_C_lp.csv"))

pal <- c(Price = "#c0392b", Rent = "#1b6f7a")
figdat <- C |> filter(spec %in% c("Price, labour controls", "Rent, labour controls",
                                  "Price, post-impact", "Rent, post-impact")) |>
  mutate(outcome = factor(ifelse(grepl("^Price", spec), "Price", "Rent"), levels = c("Price", "Rent")),
         panel = ifelse(grepl("post-impact", spec),
                        "(b) Post-impact response, y[t+h] - y[t], current price change held fixed",
                        "(a) Cumulative response with labour-market controls, y[t+h] - y[t-1]"))
ggsave(file.path(OUTDIR, "fig_lp_postimpact.png"),
       ggplot(figdat, aes(h, beta, colour = outcome, fill = outcome)) +
         geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.22, colour = NA) +
         geom_line(linewidth = 0.9) + facet_wrap(~panel, ncol = 1) +
         scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
         scale_x_continuous(breaks = 0:H) +
         labs(x = "Quarters after the innovation", y = "Log points", colour = NULL, fill = NULL) +
         theme_minimal(base_size = 12) +
         theme(legend.position = "bottom", strip.text = element_text(hjust = 0, face = "bold")),
       width = 8, height = 7.5, dpi = 300)

# Figure 5: baseline cumulative responses of the price and rent levels.
base <- C |> filter(spec %in% c("Price, baseline", "Rent, baseline")) |>
  mutate(outcome = factor(sub(",.*", "", spec), levels = c("Price", "Rent")))
ggsave(file.path(OUTDIR, "fig_lp_irf_adjusted.png"),
       ggplot(base, aes(h, beta, colour = outcome, fill = outcome)) +
         geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
         geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.22, colour = NA) +
         geom_line(linewidth = 1.0) +
         scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
         scale_x_continuous(breaks = 0:H) +
         labs(x = "Quarters after the innovation", y = "Log points", colour = NULL, fill = NULL) +
         theme_minimal(base_size = 12) + theme(legend.position = "bottom"),
       width = 8, height = 4.6, dpi = 300)

# ---- D. Tenure-channel test --------------------------------------------------
ch <- dl |> filter(quarter %in% est$quarter) |>
  semi_join(est, by = c("city", "quarter")) |>
  filter(if_all(c(paste0("di_l", 1:4), paste0("dp_l", 1:4)), is.finite))
pl <- paste0("dp_l", 1:4, collapse = " + "); cl <- paste0("di_l", 1:4, collapse = " + ")
D_full <- fe(as.formula(paste("dr ~ dr_l1 + tight_l1 + dw + dn +", pl, "+", cl)), ch)
D_np   <- fe(as.formula(paste("dr ~ dr_l1 + tight_l1 + dw + dn +", cl)), ch)
D_nc   <- fe(as.formula(paste("dr ~ dr_l1 + tight_l1 + dw + dn +", pl)), ch)
w_c <- waldtest(D_nc, D_full, vcov = dkv); w_p <- waldtest(D_np, D_full, vcov = dkv)
sums <- sapply(list(price = paste0("dp_l", 1:4), credit = paste0("di_l", 1:4)), function(v) {
  bb <- coef(D_full)[v]; VV <- dkv(D_full)[v, v]; c(sum = sum(bb), se = sqrt(sum(VV))) })
cat("\n== D channel test ==\n"); print(w_c); print(w_p); print(round(sums, 4))
out(list(full = ct(D_full), credit_given_price = w_c, price_given_credit = w_p, sums = sums), "D_channel.txt")

# ---- E. Implied omega*kappa*eta ----------------------------------------------
g <- function(s, h) C$beta[C$spec == s & C$h == h]
okh <- (g("Rent, baseline", 1) - g("Rent, baseline", 0)) / g("Price, baseline", 0)
cat(sprintf("\n== E omega*kappa*eta = %.3f; kappa*eta = %.3f ==\n", okh, okh / om))

# ---- F. Owner-occupier credit ------------------------------------------------
F1 <- fe(dp ~ dc + di + doo + tight_l1, est)
F2 <- fe(dp ~ dc_l1 + di_l1 + doo_l1 + tight_l1 + dp_l1, est)
cat("\n== F owner-occupier ==\n"); print(ct(F1)); print(ct(F2))
out(list(cur = ct(F1), lag_m = ct(F2)), "F_oo.txt")

# ---- G. Shift-share IV ------------------------------------------------------
ex <- d |> filter(quarter < as.yearqtr("2017 Q1")) |> group_by(city) |>
  summarise(theta = mean(inv_sa / (inv_sa + oo_sa), na.rm = TRUE), .groups = "drop")
iv <- est |> left_join(rest, by = "quarter") |> left_join(ex, by = "city") |>
  mutate(z = theta * g_rest) |> filter(is.finite(z))
tw <- function(f, df) plm(f, data = pd(df), model = "within", effect = "twoways")
iv_run <- function(y, ctrl, df = iv, grid = seq(-1, 2, by = 0.0025), label) {
  cs <- paste(ctrl, collapse = " + ")
  fs <- tw(as.formula(paste("di ~ z +", cs)), df)
  m2 <- plm(as.formula(paste(y, "~ di +", cs, "| z +", cs)), data = pd(df), model = "within", effect = "twoways")
  ar <- sapply(grid, function(b0) { df$yt <- df[[y]] - b0 * df$di
    m <- tw(as.formula(paste("yt ~ z +", cs)), df); coef(m)["z"]^2 / dkv(m)["z", "z"] })
  acc <- grid[ar < qchisq(0.90, 1)]
  data.frame(label = label, fs = coef(fs)["z"], F_eff = coef(fs)["z"]^2 / dkv(fs)["z", "z"],
             b = coef(m2)["di"], se = sqrt(dkv(m2)["di", "di"]),
             AR_lo = if (length(acc)) min(acc) else NA, AR_hi = if (length(acc)) max(acc) else NA,
             bounded = length(acc) > 0 && min(acc) > min(grid) && max(acc) < max(grid), n = nobs(m2))
}
G <- bind_rows(iv_run("dp", "tight_l1", label = "Price"),
               iv_run("dp", c("tight_l1", "dp_l1"), label = "Price, momentum"),
               iv_run("dr", c("gap_l1", "tight_l1", "dw", "dn"), label = "Rent"))
plac <- tw(dp_l1 ~ z + tight_l1, iv)
cat("\n== G shift-share IV ==\n"); print(ex); print(G, digits = 3); print(ct(plac)["z", ])
write_csv(G, file.path(OUTDIR, "sa_G_iv.csv"))

# ---- H. Pre-splice sample -----------------------------------------------------
pre_e <- est |> filter(quarter <= as.yearqtr("2021 Q4"))
H1 <- rbind(price_cur  = ct(fe(dp ~ dc + di + tight_l1, pre_e))["di", ],
            price_lagm = ct(fe(dp ~ dc_l1 + di_l1 + tight_l1 + dp_l1, pre_e))["di_l1", ],
            rent_tight = ct(fe(dr ~ gap_l1 + tight_l1 + dw + dn, pre_e))["tight_l1", ],
            rent_wage  = ct(fe(dr ~ gap_l1 + tight_l1 + dw + dn, pre_e))["dw", ],
            div_price  = ct(fe(dp ~ di * post + dc + tight_l1, pre_e))["di:post", ])
cat("\n== H pre-splice (N =", nrow(pre_e), ") ==\n"); print(round(H1, 4))
out(H1, "H_presplice.txt")
cat("\nDone.\n")
