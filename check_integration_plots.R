# =============================================================================
# check_integration_plots.R
# Visual integrity checks for the integrated panel_combined.csv, focused on the
# newly integrated investor-lending series (inv_commit, ABS LEND_HOUSING) and
# the reconstructed price. Look for: continuity across the 2024Q3 anchor,
# sensible cross-city ordering, expected seasonality, and no scale breaks.
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

library(dplyr); library(tidyr); library(ggplot2); library(zoo)

# Shared paths/constants from setup when present; else stand alone against the
# project layout. Defining these BEFORE the read fixes an ordering bug in the
# original (CLEAN/CITY_LEVELS were referenced above their definition).
if (file.exists("00_setup.R")) {
  source("00_setup.R")                       # CLEAN, OUTDIR, CITIES, as.yearqtr
  OUT         <- OUTDIR
  CITY_LEVELS <- CITIES
} else {
  CLEAN <- "clean"; OUT <- "output"
  dir.create(OUT, showWarnings = FALSE)
  CITY_LEVELS <- c("Sydney","Melbourne","Brisbane","Adelaide","Perth")
  if (!exists("as.yearqtr")) library(zoo)
}
ANCHOR_N <- as.numeric(as.yearqtr("2024 Q3"))

PANEL <- file.path(CLEAN, "panel_combined.csv")
d <- read.csv(PANEL, stringsAsFactors = FALSE) |>
  mutate(quarter = as.yearqtr(quarter),
         qn = as.numeric(quarter),
         city = factor(city, levels = CITY_LEVELS)) |>
  arrange(city, quarter)

# 1. Investor lending levels by city, with anchor marker -----------------------
p1 <- ggplot(filter(d, !is.na(inv_commit)), aes(qn, inv_commit, colour = city)) +
  geom_vline(xintercept = ANCHOR_N, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.6) +
  labs(title = "Investor lending commitments (inv_commit), ABS LEND_HOUSING",
       subtitle = "Dashed line = 2024Q3 anchor; check for any join discontinuity",
       x = NULL, y = "New commitments to investors ($m, Original)", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "check_inv_commit_levels.png"), p1, width = 9, height = 5, dpi = 150)

# 2. Four-quarter rolling sum: strips seasonality, exposes any level break -----
d <- d |> group_by(city) |>
  mutate(inv_commit_4q = zoo::rollsumr(inv_commit, 4, fill = NA)) |> ungroup()
p2 <- ggplot(filter(d, !is.na(inv_commit_4q)), aes(qn, inv_commit_4q, colour = city)) +
  geom_vline(xintercept = ANCHOR_N, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.6) +
  labs(title = "Investor lending, 4-quarter rolling sum (deseasonalised view)",
       subtitle = "A smooth curve across the anchor confirms a clean single-source join",
       x = NULL, y = "Trailing 4-quarter sum ($m)", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "check_inv_commit_4qsum.png"), p2, width = 9, height = 5, dpi = 150)

# 3. Within-year seasonal profile: confirms the series is Original, not SA ------
seas <- d |> filter(!is.na(inv_commit)) |>
  mutate(qtr = as.integer(format(quarter, "%q")),
         year = as.integer(floor(qn)),
         cy = paste(city, year)) |>
  group_by(cy) |> mutate(dev = inv_commit / mean(inv_commit)) |> ungroup() |>
  group_by(qtr) |> summarise(dev = mean(dev), .groups = "drop")
p3 <- ggplot(seas, aes(factor(qtr), dev)) +
  geom_col(fill = "steelblue") + geom_hline(yintercept = 1, linetype = "dotted") +
  labs(title = "Mean within-year seasonal profile of inv_commit",
       subtitle = "Visible Q1 trough / Q4 peak => Original (unadjusted), as selected",
       x = "Calendar quarter", y = "Ratio to within-year mean") +
  theme_minimal()
ggsave(file.path(OUT, "check_inv_commit_seasonality.png"), p3, width = 6, height = 4, dpi = 150)

# 4. Reconstructed price by city: continuity / no splice break -----------------
p4 <- ggplot(filter(d, !is.na(prop_price)), aes(qn, prop_price, colour = city)) +
  geom_vline(xintercept = ANCHOR_N, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.6) +
  labs(title = "Reconstructed property price index (prop_price)",
       subtitle = "Check for kinks at the 2024Q3 anchor and at the rebase window",
       x = NULL, y = "Index (2011-12 = 100)", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "check_prop_price_levels.png"), p4, width = 9, height = 5, dpi = 150)

# 5. Derived inv_intensity sanity (inv_commit / employment) --------------------
p5 <- ggplot(filter(d, !is.na(inv_intensity)), aes(qn, inv_intensity, colour = city)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Investor intensity (inv_commit / employment)",
       x = NULL, y = "inv_commit per employed person", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "check_inv_intensity.png"), p5, width = 9, height = 5, dpi = 150)

message("Wrote 5 check figures to ", normalizePath(OUT))