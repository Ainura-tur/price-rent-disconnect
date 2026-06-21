# Consolidated pipeline

Nine numbered scripts reduced to four stages plus a shared setup file. Same
logic and outputs; less duplication, one SE convention, fewer files to keep in
sync on a data refresh. Three modern-API data sources (investor lending, RBA D2
credit, NOM by visa) were added after the consolidation; see "Data sources" and
"Mechanism and nonlinearity tests" below.

## Files

| File                     | Replaces / role                           |
|--------------------------|-------------------------------------------|
| `00_setup.R`             | copy-pasted namespace/paths/constants; ABS-read + chain helpers; the Driscoll-Kraay SE; now also the modern-API filenames and the migration-share guards |
| `01_build_panel.R`       | `01_build_price_panel.R` + `03_build_combined_panel.R` (+ optional diagnostics from `02_check_price_plots.R`) |
| `02_estimate.R`          | `04_estimate.R` + `06_price_iv_robustness.R` |
| `02a_migration_granularity.R` | migration-disaggregation robustness (numerator horse-race, overseas/interstate wedge + leave-one-city-out, price placebo); reads the panel from `01`, reuses the DK machinery; wired into `run_all.R` after `02` |
| `03_figures.R`           | `05_plot_divergence.R` + `07_local_projections.R` |
| `run_all.R`              | `00_master.R` |
| `verify_raw.R`           | read-only pre-run check: confirms `raw/` holds the 13 expected inputs under their canonical names; flags rename-needed / missing / stray files. Gated by `run_all.R` and mirrored by the `targets` input check |
| `figure1_transmission.R` | **removed** — Figure 1 is drawn natively in TikZ in the manuscript `.tex`; the R rendering was redundant and is no longer part of the pipeline |
| `plot_migration_composition.R` | standalone diagnostic figures for the NOM visa shares (not in `run_all.R`) |
| `check_integration_plots.R`    | standalone integrity figures for the integrated panel (not in `run_all.R`) |

## Run

Two equivalent ways to run the pipeline. They produce the same outputs in
`clean/` and `output/`; pick one.

**Linear (run everything, every time):**
```
Rscript run_all.R
```

**Dependency-tracked (rerun only what changed) — recommended for refreshes:**
```r
# install.packages("targets")   # once
targets::tar_make()             # build/refresh whatever is out of date
```
The `targets` pipeline (`_targets.R` + `R/functions.R`) wraps the same four
stage scripts as a dependency graph. Edit the rent equation in `02_estimate.R`
and `tar_make()` reruns only `02` and the figures that read its outputs; the
panel build (`01`) and migration-granularity stage (`02a`) are skipped because
their inputs are unchanged. Drop a new file in `raw/` and `01` plus everything
downstream goes stale automatically. Useful commands:

```r
targets::tar_visnetwork()   # see the graph and what is stale
targets::tar_outdated()     # list targets that would rebuild
targets::tar_read(estimation)   # pull a target's file paths back
```

The scripts are unchanged between the two entry points: `targets` sources each
stage in a clean environment exactly as `run_all.R` does, so `run_all.R` remains
a valid fallback that needs no extra package.

Optional price-panel diagnostics (old script 02):
```r
options(BUILD_DIAGNOSTICS = TRUE); source("01_build_panel.R")
```

## Data sources

The panel combines the original ABS xlsx workbooks (price 6416.0, migration
3101.0, completions 8752.0, lending 5601.0, rents 6401.0, WPI 6345.0,
employment 6202.0, state final demand) with three modern-API dataflows added
later. Filenames for the three are centralised in `00_setup.R` (`F_LEND`,
`F_OMAD`, `F_D2`) since the long DSD-encoded names are the volatile part.

- **Investor lending** (`F_LEND`, ABS Lending Indicators `LEND_HOUSING`). The
  API successor to 5601.0 Table 14. Supplies `inv_commit` (investor housing
  new-loan commitments, $m, Original) for the five mainland states, 2006Q3
  onward. It spans both history and forecast, so it serves the panel join and
  the extension tail from one internally consistent source (no growth-chaining
  needed for this series). It reconciles to the unit with the legacy series.

- **RBA D2 national housing credit** (`F_D2`, `d02hist.xlsx`). National investor
  and owner-occupier credit stocks plus the net loan-purpose switching flow.
  National only (broadcast across cities), so it carries time variation only and
  is NOT a cross-city headline regressor. It enters in three roles: the external
  shift in the Bartik IV, the 2015-17 reclassification check, and the national
  investor-credit-share composition check. Derived: `inv_credit_nat`,
  `oo_credit_nat`, `net_switch_nat`, `inv_credit_nat_adj` (switching-adjusted),
  and the `dln_*` growth rates.

- **Overseas migration by visa** (`F_OMAD`, ABS `OMAD_VISA`). Quarterly Net
  Overseas Migration by visa-and-citizenship group, by state, on the proper
  12/16-month-rule concept, 2006Q3-2025Q3. The published "Total" reconciles to
  the unit with the panel's existing `net_overseas` (3101.0), so `net_overseas`
  is left untouched; what this adds is the housing-relevant COMPOSITION of
  migration. Derived: `nom_temp_share`, `nom_student_share`.

### Migration visa-share construction (important)

The visa shares are built from **four-quarter rolling sums**, not the raw
quarterly ratio, and are guarded. The raw quarterly share is unusable: total net
NOM crosses zero and goes negative during the 2020-21 border closures, so a
quarterly numerator/denominator explodes (shares of +1400% / -2600%). The
guards, defined once in `00_setup.R`:

- `NOM_FLOOR <- 5000` — minimum trailing-4q total NOM for a meaningful share;
  below it the share is `NA`.
- `SHARE_BAND <- c(-0.05, 1.05)` — plausible range; values outside are `NA`.

The 2020-21 window is therefore `NA` by construction and drops out of any
specification using the shares (~39 of 385 city-quarters NA: warm-up + COVID).
A skewness review (`density_regressors.png`) confirmed no transformation is
warranted: the differenced-log estimating variables are near-symmetric, the
shares are only mildly skewed and bounded, and tightness is best left in levels
(logging it makes skew far worse because it nears zero).

## What changed beyond merging

1. **Single SE convention.** Every coefficient table uses Driscoll-Kraay
   (`plm::vcovSCC`, via `dk_coeftest()` in `00_setup.R`), matching the
   manuscript. Previously the static tables used Arellano `vcovHC` while the
   LP used DK; the code and the paper now agree.

2. **Price reconstruction computed once.** `01_build_panel.R` builds the price
   index once (stage A) and reuses it for both the price-panel output and the
   swap into the combined panel.

3. **ABS read-and-chain deduplicated.** The ~8 near-identical
   read-slice-parse-aggregate-chain blocks call `read_abs_xlsx_col()`,
   `to_quarterly()`, `chain_tail()`, and `build_xlsx_tail()`.

4. **Loud failure on stale external data.** `assert_tail_ok()` checks each tail
   covers every city-quarter and is finite; the hand-typed cash-rate path is
   asserted to reach `TARGET_END`; the lending and migration reads `stopifnot`
   that all five cities are present, so a layout change fails rather than
   producing silent NAs.

5. **LP `pdata.frame` built once.** The horizon loop precomputes all leads as
   columns and sweeps the formula, instead of rebuilding the panel 26 times.

6. **`labor_income` reconstruction dropped.** Near-collinear with
   `wpi * employment` and used by no estimation script.

## Mechanism and nonlinearity tests (02_estimate.R, 02a_migration_granularity.R)

Beyond the headline rent/price equations, the divergence test, the Bartik IV,
and the original robustness suite (7.1-7.7), three analyses were added using the
new data. The first two came back as clean, well-documented NULLS; the third
(migration disaggregation) is more nuanced and is reported as a hedged check, not
a respecification. All three show the headline result does not depend on the finer
mechanism stories.

- **Section 9 — migration composition.** Tests whether the rent response to
  tightness strengthens when migration skews toward renters (student/temporary
  visas). Includes a power diagnostic (9.0) showing the share's within-city SD
  is ~0.96 of its overall SD, so the share genuinely moves and a null is
  informative rather than underpowered. Result: the tightness x student-share
  interaction is ~0 (p~0.82), the temporary-share version ~0, and the price
  placebo ~0. **No composition channel; report as a null.**

- **Section 10 — tightness nonlinearity.** Tests whether tightness passes
  through to rents convexly (harder when already tight). The natural-spline Wald
  mildly rejects linearity (p~0.04) but the within-city above-median threshold
  is null and wrong-signed, and the price placebo is the larger/more-significant
  term — the opposite of the convexity prediction. **No interpretable convexity;
  the linear tightness term in the headline equation is retained.**

- **02a — migration disaggregation.** Tests whether the single tightness
  numerator (4q net migration / 4q completions) hides structure, via three cuts.
  (1) A numerator horse-race: rebuilding tightness from renter-relevant subsets
  (overseas only, overseas ex-students, temp-share-weighted overseas) does NOT
  beat the omnibus on common-sample fit (within-R2 0.62 vs 0.51-0.60); the only
  notable shift is the overseas ex-students slope roughly doubling (0.012 vs
  0.006), i.e. students are the low-rent-slope component. (2) An overseas-vs-
  interstate split: separate slopes reject equality on the full sample
  (chi2_1=4.08, p=0.04, interstate ~2x overseas), BUT leave-one-city-out shows
  the rejection is carried entirely by Perth — dropping Perth flips the contrast
  sign and collapses the interstate slope onto the overseas one. The wedge is a
  Perth (mining-cycle) phenomenon, not a general property of the capitals. (3) A
  price-side placebo is UNINFORMATIVE, not clean: the price equation will not
  tolerate a migration regressor without its wage coefficient turning large and
  negative (~ -1.2 to -1.6), so it is evidence for nothing either way.
  **Verdict: the omnibus tightness is adequate for the panel; report the
  student-dilution magnitude and the Perth-specific interstate slope as features
  to note, claim no migration-to-price channel. The five-cluster caveat is acute
  here because one city carries the only rejection.** Corresponds to the
  "Disaggregating the migration flow" robustness paragraph and Table
  `tab:mig_loo` (leave-one-city-out) in the manuscript.

### What was deliberately NOT added

These were considered and rejected on methodological grounds; do not re-add
without a specific reason:

- **A bimodal student-share dummy / regime interaction.** The bimodality is the
  COVID structural break, not two economic regimes. With Section 9 already a
  well-powered null, splitting the share by its own value would be circular and
  read as a specification search.
- **GMM / Arellano-Bond.** Needs large N, small T; this panel is N=5, T~77, the
  opposite. It would be invalid.
- **More instruments or SE variants.** The five-city first-stage F is the
  binding constraint (3-5); more instruments manufacture false precision.
  Driscoll-Kraay already handles heteroskedasticity, autocorrelation, and
  cross-sectional dependence, so White/Breusch-Pagan would be redundant.

Identification rests on the predetermined (lagged) specifications, the
local-projection timing, and the persistently null rent interaction — not on a
strong IV. The five-cluster SE limitation applies to ALL standard errors, not
only the IV; the wild-cluster bootstrap (7.4, Webb weights) is the honest
small-N check.

## Outstanding / future

- **Vacancy rate** (purchase initiated). The one high-value addition still
  pending: a direct measure of rental-market tightness to validate the
  constructed `tightness` proxy (4q migration / 4q completions). Slots into the
  7.2 tightness-sensitivity block when it arrives. Decide before integrating
  whether it REPLACES the proxy as headline or VALIDATES it as robustness; the
  latter needs no rewrite of the rent sections.
- **2025Q4 `net_overseas`.** Genuinely unpublished on every NOM-concept source
  to date; the panel's final quarter carries a chained/forecast value. Closed
  only by the next ABS quarterly NOM release, not by any annual or
  border-movement file. Footnote as provisional.

## To refresh data next quarter

1. Edit `TARGET_END` in `00_setup.R`.
2. Drop the new ABS files in `raw/`; if the API filenames changed, update
   `F_LEND` / `F_OMAD` / `F_D2` in `00_setup.R`.
3. Update the hand-typed `cash_path` in `01_build_panel.R` (asserted to reach
   `TARGET_END`, so it fails loudly if stale).
4. Check the per-city column maps for the xlsx tails — the only other per-release
   thing; `assert_tail_ok()` will flag a layout change.
5. Rerun `Rscript run_all.R`. The coverage report at the end of stage 1 prints
   the last non-NA quarter per variable; confirm the new sources reach the
   expected quarter (note the OMAD visa shares end one quarter before the panel
   and have the COVID NA gap by design).