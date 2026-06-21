# =============================================================================
# plot_migration_composition.R
# Time-series figures for the migration visa-composition series (ABS OMAD).
# Derives the same net-NOM-by-visa-group shares as section A4 of 01_build_panel.R
# directly from the OMAD CSV, so the figures can be produced without a panel
# rebuild. Net NOM by group = migrant arrivals - migrant departures; shares are
# net-of-departures ratios using the published aggregate labels (no subgroup
# summing, to avoid double-counting nested student categories).
# =============================================================================

# Namespace protection (avoid MASS/stats masking dplyr verbs)
select <- dplyr::select;       filter    <- dplyr::filter
mutate <- dplyr::mutate;       slice     <- dplyr::slice
recode <- dplyr::recode;       rename    <- dplyr::rename
summarise <- dplyr::summarise; summarize <- dplyr::summarize
arrange <- dplyr::arrange;     count     <- dplyr::count;  lag <- dplyr::lag

library(dplyr); library(tidyr); library(ggplot2); library(zoo); library(readr)

# Use the project's shared paths/filenames when run inside the pipeline; fall
# back to standalone definitions so the script still runs from raw/ without a
# panel rebuild (its original design). 00_setup.R defines F_OMAD, OUTDIR, RAW.
if (file.exists("00_setup.R")) {
  source("00_setup.R")
  OMAD <- F_OMAD                 # raw/ABS_OMAD_VISA_..._AUS_Q.csv
  OUT  <- OUTDIR                 # output/
} else {
  OMAD <- file.path("raw",
    "ABS_OMAD_VISA___11_12_15_22_23_24_25_1009_1010_1020_1030_1040_1041_2203_2208_01_02_03_1_2_3_4_5_AUS_Q.csv")
  OUT  <- "output"
  dir.create(OUT, showWarnings = FALSE)
}
CITY_LEVELS <- c("Sydney","Melbourne","Brisbane","Adelaide","Perth")
STATE2CITY  <- c("New South Wales"="Sydney","Victoria"="Melbourne",
                 "Queensland"="Brisbane","South Australia"="Adelaide",
                 "Western Australia"="Perth")

# --- derive net NOM by visa group, then 4q-rolling guarded shares ------------
# Shares use FOUR-QUARTER ROLLING SUMS with a positive-denominator guard, matching
# section A4 of 01_build_panel.R. The raw quarterly ratio is unusable: total net
# NOM crosses zero / goes negative during the 2020-21 border closures, so a
# quarterly numerator/denominator explodes. The 4q rolling sum keeps the
# denominator positive and removes seasonality.
NOM_FLOOR  <- 5000           # persons per trailing 4q; below this the share is set NA
SHARE_BAND <- c(-0.05, 1.05) # plausible share range; outside this set NA

omad <- read_csv(OMAD, show_col_types = FALSE) |>
  filter(Region %in% names(STATE2CITY),
         `Visa and Citizenship Groups` %in%
           c("Total","Temporary visa - Total","Temporary visa - Student",
             "Permanent visa - Total")) |>
  mutate(city = recode(Region, !!!STATE2CITY),
         quarter = as.yearqtr(gsub("-Q"," Q", TIME_PERIOD)),
         grp = recode(`Visa and Citizenship Groups`,
                      "Total"                    = "nom_total",
                      "Temporary visa - Total"   = "nom_temp",
                      "Temporary visa - Student" = "nom_student",
                      "Permanent visa - Total"   = "nom_perm"),
         signed = ifelse(`Migration Type` == "Migrant arrivals", 1, -1) * as.numeric(OBS_VALUE))

net <- omad |>
  group_by(city, quarter, grp) |>
  summarise(nom = sum(signed, na.rm = TRUE), .groups = "drop") |>
  mutate(city = factor(city, levels = CITY_LEVELS),
         qn = as.numeric(quarter))

clamp_share <- function(s) ifelse(s >= SHARE_BAND[1] & s <= SHARE_BAND[2], s, NA_real_)
wide <- net |> select(city, quarter, qn, grp, nom) |>
  pivot_wider(names_from = grp, values_from = nom) |>
  arrange(city, quarter) |>
  group_by(city) |>
  mutate(nom_total_4q   = zoo::rollsumr(nom_total,   4, fill = NA),
         nom_temp_4q    = zoo::rollsumr(nom_temp,    4, fill = NA),
         nom_student_4q = zoo::rollsumr(nom_student, 4, fill = NA)) |>
  ungroup() |>
  mutate(ok = is.finite(nom_total_4q) & nom_total_4q > NOM_FLOOR,
         nom_temp_share    = clamp_share(ifelse(ok, nom_temp_4q    / nom_total_4q, NA_real_)),
         nom_student_share = clamp_share(ifelse(ok, nom_student_4q / nom_total_4q, NA_real_)))

# 1. Temporary-visa share of NOM, by city ------------------------------------
p1 <- ggplot(filter(wide, !is.na(nom_temp_share)),
             aes(qn, nom_temp_share, colour = city)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Temporary-visa share of net overseas migration",
       subtitle = "Net NOM (arrivals - departures), temporary visas / total, by city",
       x = NULL, y = "Temporary share of NOM", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "mig_temp_share.png"), p1, width = 9, height = 5, dpi = 150)

# 2. Student share of NOM, by city -------------------------------------------
p2 <- ggplot(filter(wide, !is.na(nom_student_share)),
             aes(qn, nom_student_share, colour = city)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Student-visa share of net overseas migration",
       subtitle = "Sydney and Melbourne sit highest; Brisbane lowest",
       x = NULL, y = "Student share of NOM", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "mig_student_share.png"), p2, width = 9, height = 5, dpi = 150)

# 3. Net NOM levels by visa group, Sydney (context for the shares) -----------
syd <- net |> filter(city == "Sydney",
                     grp %in% c("nom_total","nom_temp","nom_student","nom_perm")) |>
  mutate(grp = recode(grp, nom_total="Total", nom_temp="Temporary",
                      nom_student="Student", nom_perm="Permanent"))
p3 <- ggplot(syd, aes(qn, nom, colour = grp)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  labs(title = "Sydney: net NOM by visa group",
       subtitle = "Arrivals minus departures; note the COVID trough and rebound",
       x = NULL, y = "Net overseas migration (persons/qtr)", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "mig_sydney_levels.png"), p3, width = 9, height = 5, dpi = 150)

# 4. Student share of NOM with COVID guard visible (NA gap, not a spike) ------
# The share is already a 4q-rolling guarded quantity, so no further smoothing.
# Plotting it confirms the 2020-21 low-denominator window drops out cleanly as a
# gap rather than reappearing as the +1400%/-2600% spikes of the raw ratio.
p4 <- ggplot(filter(wide, !is.na(nom_student_share)),
             aes(qn, nom_student_share, colour = city)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Student-visa share of NOM (4q rolling, guarded)",
       subtitle = "COVID border-closure window drops out as a gap, not a spike",
       x = NULL, y = "Student share of NOM (4q)", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT, "mig_student_share_4q.png"), p4, width = 9, height = 5, dpi = 150)

message("Wrote 4 migration-composition figures to ", normalizePath(OUT))