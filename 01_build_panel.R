# =============================================================================
# 01_build_panel.R   (merges old 01_build_price_panel + 03_build_combined_panel)
#
# Builds the analysis panel in three stages:
#   A. Reconstruct a continuous five-city property price index (BIS for Sydney,
#      6416.0 + ABS-mean-price splice for the others), rebased to 2011-12 = 100.
#   B. Take the existing paper panel, swap in the reconstructed price, replace
#      the mis-dated migration and completions series, attach investor lending.
#   C. Extend every series to TARGET_END by growth-chaining recent ABS quarters
#      onto the 2024Q3 level, build derived variables, write clean/panel_combined.
#
# The price reconstruction (A) is computed ONCE here and reused, rather than
# being duplicated as it was across the old 01 and 03. The eight near-identical
# ABS read-and-chain blocks collapse onto build_xlsx_tail()/chain_tail() from
# 00_setup.R.
#
# Output: clean/panel_combined.{rds,csv}, output/panel_combined.csv,
#         clean/price_panel.{rds,csv}, and (optional) diagnostic plots.
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

source("00_setup.R")

ANCHOR <- as.yearqtr("2024 Q3")   # last_hist: existing panel ends here

# =============================================================================
# A. PROPERTY PRICE RECONSTRUCTION
# =============================================================================
f_6416 <- file.path(RAW, "641601.xlsx")
f_bis  <- file.path(RAW, "bis_dp_search_export_20260615-045639.csv")
f_abs  <- file.path(RAW, "ABS_RES_DWELL_ST_1_0_0__1_2_3_4_5_Q.csv")

# 6416.0 per-city index (historical leg), located by position.
r6416   <- read_excel(f_6416, sheet = "Data1", col_names = FALSE)
ser_ids <- as.character(unlist(r6416[10, 2:6]))
hist_wide <- r6416[11:nrow(r6416), 1:6]; names(hist_wide) <- c("date", unname(IDMAP_6416[ser_ids]))
rppi_hist <- hist_wide |>
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         quarter = as.yearqtr(date)) |>
  pivot_longer(all_of(CITIES), names_to = "city", values_to = "prop_hist") |>
  mutate(prop_hist = as.numeric(prop_hist)) |>
  filter(!is.na(prop_hist), quarter <= BASE_QTR) |>
  select(city, quarter, prop_hist)

# BIS Sydney index (full sample).
bis_syd <- read_csv(f_bis, skip = 2, show_col_types = FALSE)
vcol <- grep("^OBS_VALUE",   names(bis_syd), value = TRUE)[1]
pcol <- grep("^TIME_PERIOD", names(bis_syd), value = TRUE)[1]
bis_syd <- bis_syd |>
  transmute(quarter = as.yearqtr(as.Date(.data[[pcol]])),
            prop_bis = as.numeric(.data[[vcol]])) |>
  filter(!is.na(prop_bis)) |> arrange(quarter)

# ABS mean price by state (continuation leg), unsmoothed q/q growth.
mean_price <- read_csv(f_abs, show_col_types = FALSE) |>
  filter(Measure == "Mean price of residential dwellings") |>
  mutate(city = recode(Region, !!!STATE2CITY),
         quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD))) |>
  filter(city %in% CITIES) |>
  transmute(city, quarter, mean_price = as.numeric(OBS_VALUE)) |>
  arrange(city, quarter) |> group_by(city) |>
  mutate(g_raw = mean_price / lag(mean_price) - 1) |> ungroup()

# Splice the four non-Sydney cities: 6416.0 to BASE_QTR, then chain mean-price growth.
splice_one <- function(c) {
  h <- rppi_hist |> filter(city == c) |> arrange(quarter)
  lvl <- h$prop_hist[h$quarter == BASE_QTR]
  cont <- mean_price |> filter(city == c, quarter > BASE_QTR) |> arrange(quarter) |>
    mutate(prop_price = lvl * cumprod(1 + g_raw)) |> select(city, quarter, prop_price)
  bind_rows(h |> transmute(city, quarter, prop_price = prop_hist), cont)
}
spliced <- bind_rows(lapply(CITIES, splice_one))
price <- bind_rows(filter(spliced, city != "Sydney"),
                   bis_syd |> transmute(city = "Sydney", quarter, prop_price = prop_bis)) |>
  arrange(city, quarter) |>
  group_by(city) |>
  mutate(prop_price = 100 * prop_price / mean(prop_price[quarter %in% REBASE_WIN], na.rm = TRUE)) |>
  ungroup()

# Sydney splice-vs-BIS validation (base-neutral), reported in the appendix.
syd_spl <- splice_one("Sydney")
b_spl <- syd_spl$prop_price[syd_spl$quarter == BASE_QTR]
b_bis <- bis_syd$prop_bis[bis_syd$quarter == BASE_QTR]
syd_check <- syd_spl |> inner_join(bis_syd, by = "quarter") |>
  mutate(spl_idx = 100 * prop_price / b_spl, bis_idx = 100 * prop_bis / b_bis) |>
  filter(quarter > BASE_QTR) |> mutate(pct_diff = 100 * (spl_idx / bis_idx - 1))
message(sprintf("Sydney splice vs BIS (=100 at %s): mean abs %% diff = %.2f%%, max = %.2f%%",
                format(BASE_QTR), mean(abs(syd_check$pct_diff), na.rm = TRUE),
                max(abs(syd_check$pct_diff), na.rm = TRUE)))

saveRDS(price, file.path(CLEAN, "price_panel.rds"))
write_csv(price |> mutate(quarter = format(quarter)), file.path(CLEAN, "price_panel.csv"))

# =============================================================================
# A2. NATIONAL HOUSING CREDIT (RBA D2)   built once, broadcast across cities
# =============================================================================
# DLCACIHN investor-housing credit and DLCACOHN owner-occupier credit (stocks,
# $bn, original) plus DLCANS net switching of housing loan purpose (flow, $bn).
# National only: this is the national leg of the shift-share instrument and a
# national robustness series. It does NOT replace the state-level investor
# commitments (inv_commit, LEND_HOUSING); it sits alongside as inv_credit_nat.
#
# Date handling matches the ABS reads above: with col_names = FALSE the mixed
# text/date column 1 comes back as character with date cells as Excel serials,
# so we recover dates via as.Date(as.numeric(.), origin = "1899-12-30").
f_d2 <- file.path(RAW, "d02hist.xlsx")
D2_IDS <- c(inv_credit_nat = "DLCACIHN",
            oo_credit_nat  = "DLCACOHN",
            net_switch_nat = "DLCANS")

d2_raw <- read_excel(f_d2, sheet = "Data", col_names = FALSE)
d2_ids <- as.character(unlist(d2_raw[11, ]))          # row 11 = Series ID row
d2_idx <- match(D2_IDS, d2_ids)
stopifnot(!anyNA(d2_idx))                             # fail if IDs move/disappear

d2_monthly <- tibble(
  date           = d2_raw[[1]],
  inv_credit_nat = d2_raw[[d2_idx[1]]],
  oo_credit_nat  = d2_raw[[d2_idx[2]]],
  net_switch_nat = d2_raw[[d2_idx[3]]]
) |>
  slice(12:n()) |>                                    # row 12 = first observation
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         across(c(inv_credit_nat, oo_credit_nat, net_switch_nat), as.numeric)) |>
  filter(!is.na(date)) |> arrange(date)

# Quarterly collapse: stocks take the end-of-quarter level; the switching flow
# is summed within the quarter. sum_or_na preserves NA before the switching
# series begins (2015) rather than coercing absent data to a spurious zero.
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

d2_credit <- d2_monthly |>
  mutate(quarter = as.yearqtr(date)) |>
  group_by(quarter) |>
  summarise(inv_credit_nat = dplyr::last(inv_credit_nat),
            oo_credit_nat  = dplyr::last(oo_credit_nat),
            net_switch_nat = sum_or_na(net_switch_nat),
            .groups = "drop") |>
  arrange(quarter) |>
  # Switching-adjusted investor stock for the 2015-17 reclassification check.
  # SIGN CONVENTION: this assumes a positive DLCANS is an amount switched OUT of
  # investor (consistent with the late-2015 fall in investor / rise in OO), so
  # it is added back to investor. Verify against the D2 'Notes' / 'Series breaks'
  # sheets before relying on inv_credit_nat_adj; flip the sign if reversed.
  mutate(switch_cum             = cumsum(coalesce(net_switch_nat, 0)),
         inv_credit_nat_adj     = inv_credit_nat + switch_cum,
         dln_inv_credit_nat     = c(NA, diff(log(inv_credit_nat))),
         dln_inv_credit_nat_adj = c(NA, diff(log(inv_credit_nat_adj))))

# =============================================================================
# A3. STATE INVESTOR LENDING COMMITMENTS (ABS LEND_HOUSING)  built once
# =============================================================================
# New-loan-commitment VALUES to investors for housing, by state, from the modern
# ABS Lending Indicators dataflow (LEND_HOUSING 1.1). This supersedes the legacy
# 5601.0 Table 14 (560114.xlsx) read: it covers the same concept (investor
# housing commitments, $m, Original) on the five mainland states and runs to
# 2026Q1, so it serves BOTH the historical panel join and the extension tail
# with a single internally-consistent source. The column name (inv_commit) and
# city-level semantics are unchanged, so downstream code is unaffected.
#
# Slice selected:
#   Measure            = "Value"                              (vs Number)
#   Housing Purpose    = "Investor"
#   Loan Purpose       = "Total dwellings excluding refinancing"
#   Adjustment Type    = "Original"                           (matches old read)
#   Region             = the five mainland states -> CITIES via STATE2CITY
# Units are AUD millions (UNIT_MULT = 6), consistent with the 5601.0 levels.
f_lend <- file.path(RAW, "ABS_LEND_HOUSING_1_1_______10_20_1_2_3_4_5_6_7_8_AUS_Q.csv")

inv_commit <- read_csv(f_lend, show_col_types = FALSE) |>
  filter(Measure == "Value",
         `Housing Purpose` == "Investor",
         `Loan Purpose` == "Total dwellings excluding refinancing",
         `Adjustment Type` == "Original",
         Region %in% names(STATE2CITY)) |>
  mutate(city = recode(Region, !!!STATE2CITY),
         quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)),
         inv_commit = as.numeric(OBS_VALUE)) |>
  filter(city %in% CITIES, !is.na(quarter), !is.na(inv_commit)) |>
  transmute(city, quarter, inv_commit) |>
  arrange(city, quarter)

stopifnot(setequal(unique(inv_commit$city), CITIES))   # fail if a state is missing
message("inv_commit (LEND_HOUSING): ", format(min(inv_commit$quarter)), " to ",
        format(max(inv_commit$quarter)))

# =============================================================================
# A4. MIGRATION VISA COMPOSITION (ABS OMAD by visa group)   built once
# =============================================================================
# Quarterly Net Overseas Migration by visa-and-citizenship group, by state, on
# the proper 12/16-month-rule NOM concept. The published 'Total' here reconciles
# to the unit with the panel's existing net_overseas (3101.0), so this is the
# SAME total decomposed by visa, NOT a replacement series: net_overseas is left
# untouched. What it adds is the housing-relevant COMPOSITION of migration. The
# temporary and student shares matter because temporary migrants (students,
# working-holiday) flow overwhelmingly into the rental market and concentrate in
# particular cities, which is the tightness-to-rent channel the paper models.
#
# NOM by group = migrant arrivals (M1) - migrant departures (M2). We use the
# published aggregate labels directly ('Total', 'Temporary visa - Total',
# 'Temporary visa - Student') rather than summing subgroups, to avoid double-
# counting the nested student categories.
#
# SHARE CONSTRUCTION: shares are built from FOUR-QUARTER ROLLING SUMS of the net
# flows, not from the raw quarterly ratio. The raw quarterly share is unusable
# because total net NOM passes through zero and goes negative during the 2020-21
# border closures, so a quarterly numerator/denominator ratio explodes (shares of
# +1400% / -2600% in COVID quarters). The 4q rolling sum keeps the denominator
# safely positive, removes seasonality, and matches how the tightness variable is
# already built (4q migration / 4q completions). A guard additionally sets the
# share to NA in any window where the rolling total NOM is not comfortably
# positive (<= NOM_FLOOR), so no near-zero-denominator value leaks through.
f_omad <- file.path(RAW, "ABS_OMAD_VISA___11_12_15_22_23_24_25_1009_1010_1020_1030_1040_1041_2203_2208_01_02_03_1_2_3_4_5_AUS_Q.csv")
NOM_FLOOR  <- 5000          # persons per trailing 4q; below this NOM is too small for a meaningful share
SHARE_BAND <- c(-0.05, 1.05) # plausible share range; outside this set NA (guards residual blow-ups)

omad_raw <- read_csv(f_omad, show_col_types = FALSE) |>
  filter(Region %in% names(STATE2CITY),
         `Visa and Citizenship Groups` %in%
           c("Total", "Temporary visa - Total", "Temporary visa - Student")) |>
  mutate(city  = recode(Region, !!!STATE2CITY),
         quarter = as.yearqtr(gsub("-Q", " Q", TIME_PERIOD)),
         grp = recode(`Visa and Citizenship Groups`,
                      "Total"                    = "nom_total",
                      "Temporary visa - Total"   = "nom_temp",
                      "Temporary visa - Student" = "nom_student"),
         signed = ifelse(`Migration Type` == "Migrant arrivals", 1, -1) * as.numeric(OBS_VALUE)) |>
  filter(city %in% CITIES, !is.na(quarter))

# Net NOM per city x quarter x group, then 4q rolling sums, then guarded shares.
# Guard = positive denominator (NOM_FLOOR) AND resulting share inside SHARE_BAND.
clamp_share <- function(s) ifelse(s >= SHARE_BAND[1] & s <= SHARE_BAND[2], s, NA_real_)
nom_visa <- omad_raw |>
  group_by(city, quarter, grp) |>
  summarise(nom = sum(signed, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = grp, values_from = nom) |>
  arrange(city, quarter) |>
  group_by(city) |>
  mutate(nom_total_4q   = zoo::rollsumr(nom_total,   4, fill = NA),
         nom_temp_4q    = zoo::rollsumr(nom_temp,    4, fill = NA),
         nom_student_4q = zoo::rollsumr(nom_student, 4, fill = NA)) |>
  ungroup() |>
  mutate(ok = is.finite(nom_total_4q) & nom_total_4q > NOM_FLOOR,
         nom_temp_share    = clamp_share(ifelse(ok, nom_temp_4q    / nom_total_4q, NA_real_)),
         nom_student_share = clamp_share(ifelse(ok, nom_student_4q / nom_total_4q, NA_real_))) |>
  select(city, quarter, nom_temp_share, nom_student_share) |>
  arrange(city, quarter)

n_guarded <- sum(is.na(nom_visa$nom_temp_share))
stopifnot(setequal(unique(nom_visa$city), CITIES))
message("nom_visa shares (OMAD, 4q rolling, guarded): ", format(min(nom_visa$quarter)), " to ",
        format(max(nom_visa$quarter)), "; ", n_guarded,
        " city-quarters set NA (warm-up + low-denominator + out-of-band guard)")

# =============================================================================
# B. BASE PANEL: swap in reconstructed price; rebuild migration/completions
# =============================================================================
panel0 <- read_csv(file.path(RAW, "panel_main.csv"), show_col_types = FALSE) |>
  mutate(quarter = as.yearqtr(as.Date(quarter, format = "%d/%m/%Y"))) |>
  arrange(city, quarter)
message("Existing panel: ", format(min(panel0$quarter)), " to ",
        format(max(panel0$quarter)), " (", nrow(panel0), " rows)")

# Swap reconstructed price in for the old prop_price (full-sample piece).
price_new <- price |> rename(prop_price_new = prop_price)
panel <- panel0 |> select(-prop_price) |>
  left_join(price_new, by = c("city","quarter")) |> rename(prop_price = prop_price_new)

# Generic header-parsed ABS picker (for 3101.0, 8752.0 wide workbooks).
pick_by_header <- function(raw_df, header_row, match_metric, value_origin = TRUE) {
  hdr  <- as.character(unlist(raw_df[header_row, ]))
  cols <- which(startsWith(hdr, match_metric) &
                  grepl(paste(names(STATE2CITY), collapse = "|"), hdr))
  bind_rows(lapply(cols, function(j) {
    st <- names(STATE2CITY)[sapply(names(STATE2CITY), function(s) grepl(s, hdr[j]))][1]
    tibble(date = raw_df[[1]], val = as.numeric(raw_df[[j]])) |>
      slice(11:n()) |>
      mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |>
      filter(!is.na(date), !is.na(val)) |>
      transmute(city = unname(STATE2CITY[st]), quarter = as.yearqtr(date), val)
  }))
}

# Migration (replace mis-dated columns with correctly-dated 3101.0).
pop_raw <- read_excel(file.path(RAW, "310102.xlsx"), sheet = "Data1", col_names = FALSE)
nom_new <- pick_by_header(pop_raw, 1, "Net Overseas Migration")   |> rename(net_overseas_new = val)
nis_new <- pick_by_header(pop_raw, 1, "Net Interstate Migration") |> rename(net_interstate_new = val)
mig_new <- full_join(nom_new, nis_new, by = c("city","quarter")) |>
  mutate(net_migration_new = net_overseas_new + net_interstate_new)
panel <- panel |> left_join(mig_new, by = c("city","quarter")) |>
  mutate(net_overseas = coalesce(net_overseas_new, net_overseas),
         net_interstate = coalesce(net_interstate_new, net_interstate),
         net_migration = coalesce(net_migration_new, net_overseas + net_interstate)) |>
  select(-net_overseas_new, -net_interstate_new, -net_migration_new)

# Migration visa composition (NEW columns; OMAD, built in A4). net_overseas is
# unchanged; these are the temporary/student shares of NOM that ride alongside.
panel <- panel |> left_join(nom_visa, by = c("city","quarter"))

# Dwelling completions (replace mis-dated columns with 8752.0 Table 39).
dw_raw <- read_excel(file.path(RAW, "87520039.xlsx"), sheet = "Data1", col_names = FALSE)
dw_hdr <- as.character(unlist(dw_raw[1, ]))
dw_pick <- function(sector) {
  cols <- which(grepl(sector, dw_hdr, fixed = TRUE) &
                  grepl("Total (Type of Building)", dw_hdr, fixed = TRUE) &
                  grepl("Total (Type of Work)", dw_hdr, fixed = TRUE) &
                  grepl(paste(names(STATE2CITY), collapse = "|"), dw_hdr))
  bind_rows(lapply(cols, function(j) {
    st <- names(STATE2CITY)[sapply(names(STATE2CITY), function(s) grepl(s, dw_hdr[j]))][1]
    tibble(date = dw_raw[[1]], val = as.numeric(dw_raw[[j]])) |>
      slice(11:n()) |>
      mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |>
      filter(!is.na(date), !is.na(val)) |>
      transmute(city = unname(STATE2CITY[st]), quarter = as.yearqtr(date), val)
  }))
}
dwell_new <- dw_pick("Total Sectors")  |> rename(dwell_total_new = val) |>
  left_join(dw_pick("Private Sector") |> rename(dwell_private_new = val), by = c("city","quarter")) |>
  left_join(dw_pick("Public Sector")  |> rename(dwell_public_new = val),  by = c("city","quarter"))
panel <- panel |> left_join(dwell_new, by = c("city","quarter")) |>
  mutate(dwell_total = coalesce(dwell_total_new, dwell_total),
         dwell_private = coalesce(dwell_private_new, dwell_private),
         dwell_public = coalesce(dwell_public_new, dwell_public)) |>
  select(-dwell_total_new, -dwell_private_new, -dwell_public_new)

# Investor lending (NEW column for the price equation; LEND_HOUSING, built in A3).
panel <- panel |> left_join(inv_commit, by = c("city","quarter"))

# =============================================================================
# C. EXTENSION to TARGET_END
# =============================================================================
new_q <- as.yearqtr(seq(as.numeric(max(panel$quarter)) + 0.25,
                        as.numeric(TARGET_END), by = 0.25))
n_new <- length(new_q)
scaffold <- tidyr::expand_grid(city = CITIES, quarter = new_q)

# prop_price tail comes straight from the reconstruction.
ext <- scaffold |>
  left_join(price |> filter(quarter > ANCHOR, quarter <= TARGET_END) |>
              transmute(city, quarter, prop_price = prop_price), by = c("city","quarter"))

# --- growth-chained xlsx tails (rents, WPI, employment) via the helper -------
rent_loc <- list(Sydney=list("Data1",56), Melbourne=list("Data1",188),
                 Brisbane=list("Data2",70), Adelaide=list("Data2",202), Perth=list("Data3",84))
rents_tail <- purrr::imap_dfr(rent_loc, function(loc, city)
  read_abs_xlsx_col(file.path(RAW,"6401010.xlsx"), loc[[1]], loc[[2]]) |>
    to_quarterly() |> chain_tail(panel, "rents", city, ANCHOR, TARGET_END))
assert_tail_ok(rents_tail, length(CITIES), n_new, "rents")
ext <- ext |> left_join(rents_tail, by = c("city","quarter")) |> rename(rents = value)

wpi_tail <- build_xlsx_tail(file.path(RAW,"634502b.xlsx"), "Data1",
                            c(Sydney=2,Melbourne=3,Brisbane=4,Adelaide=5,Perth=6),
                            panel, "wpi", ANCHOR, TARGET_END)
assert_tail_ok(wpi_tail, length(CITIES), n_new, "wpi")
ext <- ext |> left_join(wpi_tail, by = c("city","quarter")) |> rename(wpi = value)

emp_tail <- build_xlsx_tail(file.path(RAW,"62020010.xlsx"), "Data1",
                            c(Sydney=6,Melbourne=9,Brisbane=12,Adelaide=15,Perth=18),
                            panel, "employment", ANCHOR, TARGET_END)
assert_tail_ok(emp_tail, length(CITIES), n_new, "employment")
ext <- ext |> left_join(emp_tail, by = c("city","quarter")) |> rename(employment = value)

# --- cash rate tail: national published path (assert it reaches TARGET_END) --
cash_path <- tibble(
  quarter = as.yearqtr(c("2024 Q4","2025 Q1","2025 Q2","2025 Q3","2025 Q4")),
  cr = c(4.35, 4.10, 3.85, 3.60, 3.60))
stopifnot(max(cash_path$quarter) == TARGET_END)   # fail loudly if it goes stale
ext <- ext |>
  left_join(tidyr::expand_grid(city = CITIES, cash_path) |> transmute(city, quarter, cash_rate = cr),
            by = c("city","quarter"))

# --- migration / completions / lending tails (from the rebuilt full series) --
# inv_commit now extends natively to its own last quarter (LEND_HOUSING), so the
# tail join simply carries the observed values past ANCHOR; no chaining needed.
ext <- ext |>
  left_join(mig_new |> filter(quarter > ANCHOR, quarter <= TARGET_END) |>
              transmute(city, quarter, net_overseas = net_overseas_new,
                        net_interstate = net_interstate_new, net_migration = net_migration_new),
            by = c("city","quarter")) |>
  left_join(dwell_new |> filter(quarter > ANCHOR, quarter <= TARGET_END) |>
              transmute(city, quarter, dwell_total = dwell_total_new,
                        dwell_private = dwell_private_new, dwell_public = dwell_public_new),
            by = c("city","quarter")) |>
  left_join(inv_commit |> filter(quarter > ANCHOR, quarter <= TARGET_END),
            by = c("city","quarter")) |>
  left_join(nom_visa |> filter(quarter > ANCHOR, quarter <= TARGET_END),
            by = c("city","quarter"))

# --- housing services & GFCF tails: ABS state final demand CSVs --------------
read_sfd <- function(file, sector = NULL) {
  x <- read_csv(file.path(RAW, file), show_col_types = FALSE) |>
    filter(Region %in% names(STATE2CITY)) |>
    mutate(city = recode(Region, !!!STATE2CITY),
           quarter = as.yearqtr(gsub("-Q"," Q", TIME_PERIOD)), val = as.numeric(OBS_VALUE))
  if (!is.null(sector)) x <- x |> filter(Sector == sector)
  x |> select(city, quarter, val)
}
hsv_src <- read_sfd("ABS_ANA_SFD_1_0_0_C_FCE_RDSH__20_2_3_4_5_6_1_Q.csv")
hsv_tail <- purrr::map_dfr(CITIES, function(c)
  hsv_src |> filter(city == c) |> transmute(quarter, value = val) |>
    chain_tail(panel, "housing_services", c, ANCHOR, TARGET_END))
ext <- ext |> left_join(hsv_tail, by = c("city","quarter")) |> rename(housing_services = value)

gfcf_priv_src <- read_sfd("ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_20_2_3_4_5_6_1_Q.csv", "Private")
gfcf_pub_src  <- read_sfd("ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_20_2_3_4_5_6_1_Q.csv", "Public")
gfcf_tail <- function(src, basecol) purrr::map_dfr(CITIES, function(c)
  src |> filter(city == c) |> transmute(quarter, value = val) |>
    chain_tail(panel, basecol, c, ANCHOR, TARGET_END))
ext <- ext |>
  left_join(gfcf_tail(gfcf_priv_src, "gfcf_private"), by = c("city","quarter")) |> rename(gfcf_private = value) |>
  left_join(gfcf_tail(gfcf_pub_src,  "gfcf_public"),  by = c("city","quarter")) |> rename(gfcf_public = value)

# (labor_income reconstruction intentionally dropped: near-collinear with
#  wpi*employment and used by no estimation script. See optimization note #8.)

# =============================================================================
# D. STITCH, DERIVE, WRITE
# =============================================================================
panel_combined <- bind_rows(panel, ext) |> arrange(city, quarter) |>
  left_join(d2_credit, by = "quarter") |>   # national D2 credit, broadcast across cities
  group_by(city) |>
  mutate(
    nm_4q = zoo::rollsumr(net_migration, 4, fill = NA),
    dw_4q = zoo::rollsumr(dwell_total,   4, fill = NA),
    tightness = nm_4q / dw_4q,
    no_4q = zoo::rollsumr(net_overseas, 4, fill = NA),
    tightness_no = no_4q / dw_4q,
    price_rent = prop_price / rents,
    real_rent  = rents / wpi,
    user_cost  = cash_rate * prop_price,
    inv_intensity = inv_commit / employment
  ) |>
  select(-nm_4q, -dw_4q, -no_4q) |>
  ungroup() |>
  mutate(post_2017 = as.integer(quarter >= as.yearqtr("2017 Q1")),
         treated   = as.integer(city == "Sydney"),
         treat_post = treated * post_2017,
         post = post_2017)

# Coverage report.
vars <- c("rents","prop_price","dwell_total","net_migration","wpi","employment",
          "gfcf_public","gfcf_private","housing_services","cash_rate","inv_commit",
          "inv_credit_nat","nom_temp_share","nom_student_share")
cat("\n--- last non-NA quarter per variable ---\n")
for (v in vars) {
  last_q <- panel_combined |> filter(!is.na(.data[[v]])) |> summarise(m = max(quarter)) |> pull(m)
  cat(sprintf("  %-18s ends %s\n", v, format(last_q)))
}

saveRDS(panel_combined, file.path(CLEAN, "panel_combined.rds"))
panel_csv <- panel_combined |>
  mutate(year = as.integer(floor(as.numeric(quarter))),
         qtr  = as.integer(round((as.numeric(quarter) - year) * 4) + 1),
         quarter = format(quarter))
write_csv(panel_csv, file.path(CLEAN, "panel_combined.csv"))
write_csv(panel_csv, file.path(OUTDIR, "panel_combined.csv"))   # single source of truth
message("Wrote panel_combined.{rds,csv} (", nrow(panel_combined), " rows).")

# =============================================================================
# E. OPTIONAL price-panel diagnostics (old 02). Set BUILD_DIAGNOSTICS=TRUE to run.
# =============================================================================
if (isTRUE(getOption("BUILD_DIAGNOSTICS", FALSE))) {
  library(ggplot2)
  bn <- as.numeric(BASE_QTR)
  pl <- price |> mutate(qn = as.numeric(quarter), city = factor(city, levels = CITIES))
  ggsave(file.path(OUTDIR, "diag_price_levels.png"),
         ggplot(pl, aes(qn, prop_price, colour = city)) +
           geom_vline(xintercept = bn, linetype = "dashed", colour = "grey50") +
           geom_line(linewidth = 0.7) + scale_colour_manual(values = CITY_COLS) +
           labs(title = "Reconstructed price index by city", x = NULL,
                y = "Index (2011-12 = 100)") + theme_minimal(),
         width = 9, height = 5, dpi = 150)
  ggsave(file.path(OUTDIR, "diag_sydney_splice_vs_bis.png"),
         ggplot(syd_check, aes(as.numeric(quarter))) +
           geom_line(aes(y = spl_idx, colour = "Splice"), linewidth = 0.8) +
           geom_line(aes(y = bis_idx, colour = "BIS"), linewidth = 0.8) +
           labs(title = "Sydney: splice vs BIS (=100 at splice quarter)",
                x = NULL, y = "Index", colour = NULL) + theme_minimal(),
         width = 9, height = 5, dpi = 150)
  message("Wrote price-panel diagnostics to ", OUTDIR, "/")
}