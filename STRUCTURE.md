# Repository structure

This file maps the repository: which script reads and writes which files, and
which steps depend on which. For how the pipeline works and what each check
finds, see `PIPELINE_README.md`. On a data refresh, this map shows which files
to touch.

## Directory tree

```
project-root/
│
├── run_all.R                      entry point: Rscript run_all.R
│                                  checks raw/ first (verify_raw.R), then runs the
│                                  13 steps below, each in its own R session
├── verify_raw.R                   read-only check of raw/ (present? canonically
│                                  named? strays?); exits 0 or 1
├── 00_setup.R                     helper, not a step: paths, constants, namespace
│                                  protection, ABS read/chain helpers,
│                                  dk_coeftest()/dk_se(), modern-API filenames
│                                  (F_LEND/F_OMAD/F_D2), migration-share guards
│                                  (NOM_FLOOR, SHARE_BAND); sourced by 01, 01a, 02,
│                                  02a and 03
├── 00_run_log.R                   helper, not a step: every script sources it and
│                                  writes its log to run_logs/
│
├── 01_build_panel.R               01   the analysis panel
├── 01a_build_test_panel.R         01a  adjusted credit, investor share, lags, sample flags
├── 02_estimate.R                  02   rent equation, divergence, robustness, mechanism checks
├── 02a_migration_granularity.R    02a  migration split by type (not reported in the paper)
├── 02b_break_predictive_tests.R   02b  break, Granger and stacked price-rent tests
├── 02c_main_tables_sa.R           02c  Tables 2, 4 and 5; Appendix C checks
├── 02d_unit_root_tests.R          02d  unit-root table (Table 8)
├── 03_figures.R                   03   earlier figures and local projections
├── 03b_local_projections.R        03b  local projections, adjusted credit (Figure 5)
├── 03c_lp_iv_apra.R               03c  instrumented local projections (Figure 6)
├── 03d_cr_test_apra.R             03d  correlation-restriction test of the instruments
├── 03e_cr_table_figure.R          03e  Table 6 and Figure 7
├── 03f_descriptive_figures.R      03f  Figures 2, 3, 4 and 8
│
├── lib/ivcrtest/                  routines of the correlation-restriction test, read by 03d
│
│   standalone diagnostics (not run by run_all.R; run by hand):
├── plot_migration_composition.R   NOM visa-share diagnostic figures
├── check_integration_plots.R      integrity figures for the panel
│
├── README.md, PIPELINE_README.md, STRUCTURE.md (this file)
├── manuscript/                    LaTeX source and PDF of the June 2026 version
│
├── raw/        inputs (you supply them; the scripts only read them)
├── clean/      intermediate panels (written by 01 and 01a)
├── output/     results: tables, series, figures
└── run_logs/   one log per script run, and run_history.csv
```

Table and figure numbers refer to the October 2026 revision of the paper.

## Naming convention

Numbered scripts are the steps that `run_all.R` runs, in the order above.
Steps 01, 02, 02a and 03 are the June 2026 pipeline. They read
`clean/panel_combined`. Steps 01a, 02b to 02d and 03b to 03f were added for the
October 2026 revision. Except for 01a, which builds it, they read
`clean/panel_tests`. Unnumbered scripts are helpers (`00_setup.R`,
`00_run_log.R`, `verify_raw.R`) or diagnostics run by hand.

The two diagnostics source `00_setup.R` when it is present (for `OUTDIR`,
`F_OMAD` and `CITIES`) and otherwise define what they need. Both write to
`output/`.

> **Figure 1 is TikZ, not R.** The transmission diagram (Figure 1) is drawn in
> a `tikzpicture` inside the manuscript `.tex`. An earlier R version
> (`figure1_transmission.R`) is no longer part of the pipeline.

## raw/: inputs

| File | Source and role | Read by |
|------|-----------------|---------|
| `641601.xlsx` | ABS 6416.0 price index, per-city historical leg | 01 |
| `bis_dp_search_export_20260615-045639.csv` | BIS Sydney property price index, full sample | 01 |
| `ABS_RES_DWELL_ST_1_0_0__1_2_3_4_5_Q.csv` | ABS mean dwelling price by state, price continuation leg | 01 |
| `310102.xlsx` | ABS 3101.0 population: net overseas and net interstate migration | 01 |
| `87520039.xlsx` | ABS 8752.0 Table 39 dwelling completions (total, private, public) | 01 |
| `6401010.xlsx` | ABS 6401.0 rents, per-city sheets | 01 |
| `634502b.xlsx` | ABS 6345.0 Wage Price Index | 01 |
| `62020010.xlsx` | ABS 6202.0 employment | 01 |
| `ABS_ANA_SFD_1_0_0_C_FCE_RDSH__20_2_3_4_5_6_1_Q.csv` | state final demand: housing services consumption | 01 |
| `ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_20_2_3_4_5_6_1_Q.csv` | state final demand: GFCF (private and public) | 01 |
| `ABS_LEND_HOUSING_1_1_..._AUS_Q.csv` (`F_LEND`) | new housing loan commitments by state (5601.0 successor): investor and owner-occupier, original and seasonally adjusted | 01, 01a |
| `ABS_OMAD_VISA_..._AUS_Q.csv` (`F_OMAD`) | net overseas migration by visa and citizenship group, by state | 01 |
| `d02hist.xlsx` (`F_D2`) | RBA D2 national housing credit (investor and owner-occupier stock, switching flow) | 01 |
| `panel_main.csv` | the earlier paper panel, into which 01 swaps the rebuilt series | 01 |

`00_setup.R` holds the three modern-API filenames as `F_LEND`, `F_OMAD` and
`F_D2`, because their long names are the part that changes on a refresh.

> **Not committed.** Three files may sit in a working `raw/` folder, but no
> script reads them, so the repository leaves them out: `560114.xlsx` (legacy
> 5601.0 lending, replaced by `F_LEND`), `f01dhist.xls` (RBA F1 interest rates)
> and the all-groups OMAD export `ABS_OMAD_VISA,+..._AUS.Q.csv` (replaced by
> `F_OMAD`). See "Files deliberately NOT committed" in `raw/README.md`. The
> committed input set is 14 files.

## clean/: intermediate panels

| File | Written by | Read by |
|------|-----------|---------|
| `price_panel.rds`, `.csv` | 01 | none (record of the price reconstruction) |
| `panel_combined.rds`, `.csv` | 01 | 01a, 02, 02a, 03 |
| `panel_tests.rds`, `.csv` | 01a | 02b, 02c, 02d, 03b, 03c, 03d, 03f |
| `panel_tests_stacked.rds`, `.csv` | 01a | 02b |

`panel_combined` is the analysis panel. `panel_tests` adds the seasonally
adjusted credit series, the investor share of new lending, lags and sample
flags. Only 01 and 01a read `raw/`; every later step reads one of these panels.
The October steps read the `.csv` copy when the `.rds` is missing.

## output/: results

### 01: panel

```
panel_combined.csv            copy of the panel, for inspection
diag_price_levels.png         only with options(BUILD_DIAGNOSTICS = TRUE)
diag_sydney_splice_vs_bis.png
```

### 01a: test panel

```
panel_tests_checks.txt        coverage and consistency checks of the new series
```

### 02: rent equation, divergence, robustness

```
est_rent.txt              est_rent_mg.txt          est_rent_r2.txt
est_price.txt             est_price_predetermined.txt
est_rent_divergence.txt   est_rent_divergence_pre.txt
est_price_divergence.txt  est_price_divergence_pre.txt
est_price_divergence_covid.txt   est_price_divergence_sydney.txt
est_price_sydney_totalslope.txt

robust_tightness.txt          robust_rent_stability.txt
robust_credit_share.txt       robust_credit_share_control.txt
robust_switchflow_control.txt robust_switchflow_diagnostic.txt
robust_cointegration.txt      robust_lp_lags.txt   robust_wildboot.txt

mech_power_diagnostic.txt
mech_rent_share_level.txt
mech_rent_tightness_x_student.txt   mech_rent_tightness_x_temp.txt
mech_price_tightness_x_student.txt
nlin_rent_tightness_spline.txt
nlin_rent_tightness_threshold.txt   nlin_price_tightness_threshold.txt

alt_rent_trend.txt   alt_price_spline.txt   alt_cointegration_westerlund.txt
alt_rent_rolling_coef.csv
rolling_financial_r2.csv
```

### 02a: migration split by type

```
granular_rent_tightness.txt          numerator comparison, one file per variant:
granular_rent_tightness_no_chk.txt     total / overseas only /
granular_rent_tightness_no_stu.txt     overseas without students /
granular_rent_tightness_rw.txt         renter-weighted overseas
granular_rent_split_flows.txt        overseas and interstate, separate slopes
granular_rent_split_waldeq.txt       Wald test of equal slopes (needs car)
granular_rent_split_loo.txt          the same, leaving out one city at a time
granular_price_placebo.txt           price placebo, with total tightness
granular_price_placebo_alone.txt     price placebo, on its own
```

### 02b: break, Granger and stacked tests

```
tests_break_dated.csv   tests_break_sup.csv   tests_break_path.csv
tests_granger.csv       tests_granger_dh.csv  tests_stacked.csv
tests_summary.txt
```

### 02c: main tables on adjusted credit

```
table_est_summary.tex     Table 2
table_price_eq.tex        Table 4
table_divergence.tex      Table 5
main_tables_estimates.csv every estimate, at full precision
main_tables_summary.txt   the numbers quoted in the text
```

### 02d: unit-root tests

```
table_unit_root.tex       Table 8
unit_root_tests.csv       panel statistics
unit_root_by_city.csv     city ADF tests
```

### 03: earlier figures and local projections

```
local_projection_irf.csv      local_projection_irf.png
lp_rent_lag_sensitivity.csv   fig_lp_rent_lag_sensitivity.png
fig_rent_rolling_coef.png
fig_divergence.png            fig_price_rent_gap.png
fig_real_rent.png             fig_rental_trajectories.png
```

The last four have the same names as 03f's figures. 03f runs later and
overwrites them; the paper uses 03f's versions.

### 03b: local projections on adjusted credit

```
fig_lp_irf_adjusted.png       Figure 5
fig_lp_rent_lags_adjusted.png
lp_irf_revision.csv           lp_summary.txt
```

### 03c: instrumented local projections

```
fig_lpiv_main.png             Figure 6
fig_lpiv_share.png
lpiv_results.csv              lpiv_summary.txt
```

### 03d: correlation-restriction test

```
cr_results.csv                cr_summary.txt
cr_cache/                     one result per specification
```

03d reuses a cached result while the data, settings and `lib/ivcrtest` code
behind it are unchanged.

### 03e: Table 6 and Figure 7

```
table_cr_apra.tex             Table 6
fig_cr_valid_range.pdf        Figure 7
fig_cr_valid_range.png
```

### 03f: descriptive figures

```
fig_rental_trajectories.png   Figure 2
fig_price_rent_gap.png        Figure 3
fig_real_rent.png             Figure 4
fig_divergence.png            Figure 8
rolling_partial_r2.csv        descriptive_figures_summary.txt
```

Figure 1 is TikZ in the `.tex`, and Table 7 lists the data sources, so no
script produces them.

### Standalone diagnostics (run by hand)

`plot_migration_composition.R` reads the OMAD file in `raw/` directly, so it
needs no panel. It writes to `output/`:

```
mig_temp_share.png        temporary-visa share of NOM, by city
mig_student_share.png     student share of NOM, by city
mig_sydney_levels.png     net NOM by visa group, Sydney
mig_student_share_4q.png  student share, with the 2020-21 gap visible
```

`check_integration_plots.R` reads `clean/panel_combined.csv` and writes to
`output/`:

```
check_inv_commit_levels.png        investor lending by city, with the 2024 Q3 anchor
check_inv_commit_4qsum.png         four-quarter rolling sum (shows level breaks)
check_inv_commit_seasonality.png   within-year profile of the original series
check_prop_price_levels.png        rebuilt price index by city (splice continuity)
check_inv_intensity.png            investor lending over employment
```

## run_logs/

Every script writes `run_logs/<script>_<date>_<time>.log`. The log has a
header (start time, folder, R version, git commit), everything the script
printed, its messages and warnings, and a footer with the run time, the status,
a count of warnings by message and `sessionInfo()`. Each run also adds a row to
`run_logs/run_history.csv`. `run_all.R` writes its own log with the step
summary. 03d also writes `03d_cr_progress.log` while it computes.

## Step dependencies

```
00_setup.R, 00_run_log.R ── sourced by the steps

raw/ ──▶ 01 ──▶ clean/panel_combined ──┬──▶ 02  ──▶ 03
                                       ├──▶ 02a
                                       └──▶ 01a (also reads F_LEND) ──▶ clean/panel_tests
                                                 │
                                                 ├──▶ 02b, 02c, 02d, 03b, 03f
                                                 └──▶ 03c ──┐
                                                      03d ──┴──▶ 03e
                                            (03d also reads lib/ivcrtest)
```

`run_all.R` skips a step when a step it needs failed in the same run, so no
step reads stale files. It prints each step's status, time and log file. Run
from the command line, it exits with status 1 if any step failed or was
skipped.

## Data that ends early

- **Net migration** ends in 2025 Q3, one quarter before the other series,
  because ABS publishes it later. Tightness, built from it, also ends in
  2025 Q3. It enters the equations lagged one quarter, so 2025 Q4 is still the
  last estimation quarter.
- **OMAD visa shares** (`nom_temp_share`, `nom_student_share`) also end in
  2025 Q3. They are `NA` across the 2020-21 border closures by construction,
  and in the first quarters, which the four-quarter sums need. A specification
  that uses them drops those quarters: the rent equation's sample falls from
  370 to 342 city-quarters.

## Refreshing data next quarter

1. Edit `TARGET_END` in `00_setup.R`.
2. Put the new files in `raw/`. If the API filenames changed, update `F_LEND`,
   `F_OMAD` or `F_D2` in `00_setup.R`, and `F_LEND` in section 0 of
   `01a_build_test_panel.R` (used only when `00_setup.R` is absent).
3. Update the hand-typed `cash_path` in `01_build_panel.R`. A check stops the
   run if it does not reach `TARGET_END`.
4. Check the per-city column maps for the xlsx tails; `assert_tail_ok()` flags
   a layout change.
5. Run `Rscript run_all.R`. The coverage report of step 01 prints the last
   quarter with data for each variable; confirm that the new files reach the
   expected quarter. New data changes every result of 03d, so its next run
   takes a few hours.
