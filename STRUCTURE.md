# Repository structure

Full layout of scripts, inputs, intermediates, and outputs for the rent/price
divergence pipeline. For *how* the pipeline works and what each test concludes,
see `PIPELINE_README.md`; this file is the map of *what lives where* and *what
depends on what*, so a data refresh shows exactly which files to touch.

## Directory tree

```
project-root/
│
├── run_all.R                      entry point: Rscript run_all.R [--bootstrap]
│                                  sources 00_setup.R; runs 01 -> 01b -> 02 -> 02a
│                                  -> 02c -> 02d -> 02f -> 03 -> 03f -> 03g -> 04
│                                  -> 04b, with --bootstrap also 05 -> 05c -> 05b,
│                                  then 04c (once bootstrap draws exist) and 06
│                                  (pre-flight: verify_raw.R); logs to logs/
│
├── verify_raw.R                   read-only pre-run check of raw/ inputs
│                                  (present? canonically named? strays?) exit 0/1
│
├── _targets.R                     ALTERNATIVE entry point: targets::tar_make()
│                                  dependency-tracked; reruns only stale stages
├── R/
│   └── functions.R                stage wrappers + file-listing helpers for _targets.R
│   _targets/                      (auto-created cache; gitignore it)
│
├── 00_setup.R                     sourced by every stage (not itself a step):
│                                  paths, constants, namespace protection,
│                                  ABS read/chain helpers, dk_coeftest()/dk_se(),
│                                  modern-API filenames (F_LEND/F_OMAD/F_D2),
│                                  migration-share guards (NOM_FLOOR, SHARE_BAND)
│
├── 01_build_panel.R               STAGE 1  panel construction
├── 01b_new_lease_rents.R          STAGE 1b rents on new leases (needs raw_rev/)
├── 02_estimate.R                  STAGE 2  rent/price equations, divergence, IV
├── 02a_migration_granularity.R    STAGE 2a migration-disaggregation robustness
├── 02c_sa_additions.R             STAGE 2c dynamic rent equation, local projections
├── 02d_sa_tables.R                STAGE 2d appendix tables (C.2, C.3, E, E.1), Tables 1-2
├── 02f_new_lease_tests.R          STAGE 2f new-lease rents against the model (Appendix F)
├── 03_figures.R                   STAGE 3  Figure 1, divergence figures, local projections
├── 03f_missing_figures.R          STAGE 3f APRA instruments, correlation-restriction test
├── 03g_cr_endpoints.R             STAGE 3g MCUB endpoints (optional; needs ivcrtest)
├── 04_model_matching.R            STAGE 4  model estimation by impulse-response matching
├── 04b_identification.R           STAGE 4b distance profiles and sensitivity
├── 04c_inflation.R                STAGE 4c measured inflation (runs after the bootstrap)
├── 05_bootstrap.R                 STAGE 5  block bootstrap of the model, in 22 chunks
├── 05c_small_sample.R             STAGE 5c small-sample Monte Carlo
├── 05b_bootstrap_summary.R        STAGE 5b bootstrap intervals and figure bands
├── 06_paper_figures.R             STAGE 6  redraws eight paper figures in one style (last)
│
│   run by hand (NOT in run_all.R):
├── fetch_rent_reports.R           downloads the SA bond workbooks and NSW report PDFs
├── plot_migration_composition.R   NOM visa-share diagnostic figures
├── check_integration_plots.R      integrated-panel integrity figures
│
├── PIPELINE_README.md
├── STRUCTURE.md                   (this file)
│
├── manuscript/ the paper (PDF of the current version; LaTeX source of June 2026)
├── raw/        INPUTS  (committed; treated as read-only)
├── raw_rev/    INPUTS  state bond reports for 01b (you supply; NOT committed)
├── clean/      INTERMEDIATE  (written by stage 01, read by later stages)
├── clean_rev/  INTERMEDIATE  (written by stage 01b, read by 02f; new_lease_rents.csv committed)
├── output/     RESULTS  (tables, series, figures)
└── logs/       per-run logs
```

## Naming convention

Numbered scripts are pipeline stages run by `run_all.R` (`01`, `01b`, `02`,
`02a`, `02c`, `02d`, `02f`, `03`, `03f`, `03g`, `04`, `04b`, with `--bootstrap`
also `05`, `05c` and `05b`, then `04c` and `06`). Two run out of numerical
order: `04c` reads the bootstrap draws, so it follows `05b`, and `06` redraws
figures that earlier stages also write, so it runs last. `01b` runs only when
`raw_rev/` exists, and `02f` only when `clean_rev/new_lease_rents.csv` exists or
`01b` will write it. README.md maps each table and figure in the paper to its
output file. The `a` suffix denotes a stage that depends on
and extends the same-numbered stage (`02a` reads the panel and reuses `02`'s
inference machinery, but is logically a continuation of estimation). Unnumbered
`.R` files are either sourced helpers (`00_setup.R`, `R/functions.R`) or hand-run
diagnostics not in the master run.

> Two standalone diagnostics had misspelled filenames as first uploaded
> (`plot_migration_compisition.R`, `check_integartion_plots.R`); the correct
> names are `plot_migration_composition.R` and `check_integration_plots.R`, as
> referenced here and in `PIPELINE_README.md`. Both now source `00_setup.R` when
> present (using `OUTDIR`, `F_OMAD`, `CITIES`) and fall back to standalone
> definitions when run alone, writing to `output/` either way.

> **Figure 1 is drawn in R.** `03_figures.R` draws the transmission diagram
> (Figure 1) with ggplot2 and needs no data. It writes
> `output/fig_transmission.pdf`, which the paper includes, and a PNG preview.
> The earlier `figure1_transmission.R` and the TikZ version in the `.tex` are
> superseded; a copy of either is stale.

## raw/ — inputs

| File | Source / role | Consumed by |
|------|---------------|-------------|
| `641601.xlsx` | ABS 6416.0 price index, per-city historical leg | 01 |
| `bis_dp_search_export_20260615-045639.csv` | BIS Sydney property price index, full sample | 01 |
| `ABS_RES_DWELL_ST_1_0_0__1_2_3_4_5_Q.csv` | ABS mean dwelling price by state, price continuation leg | 01 |
| `310102.xlsx` | ABS 3101.0 population: net overseas + net interstate migration | 01 |
| `87520039.xlsx` | ABS 8752.0 Table 39 dwelling completions (total/private/public) | 01 |
| `6401010.xlsx` | ABS 6401.0 rents, per-city sheets | 01 |
| `634502b.xlsx` | ABS 6345.0 wage price index | 01 |
| `62020010.xlsx` | ABS 6202.0 employment | 01 |
| `ABS_ANA_SFD_1_0_0_C_FCE_RDSH__20_2_3_4_5_6_1_Q.csv` | state final demand: housing services consumption | 01 |
| `ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_20_2_3_4_5_6_1_Q.csv` | state final demand: GFCF (Private / Public) | 01 |
| `ABS_LEND_HOUSING_1_1_..._AUS_Q.csv`  (= `F_LEND`) | investor housing commitments; 5601.0 successor; serves join + tail | 01 |
| `ABS_OMAD_VISA_..._AUS_Q.csv`  (= `F_OMAD`) | NOM by visa-and-citizenship group, by state | 01 |
| `d02hist.xlsx`  (= `F_D2`) | RBA D2 national housing credit (investor/oo stock, switching flow) | 01 |
| `panel_main.csv` | existing paper panel; base into which reconstructed series are swapped | 01 |

The three modern-API filenames are centralised in `00_setup.R` as `F_LEND`,
`F_OMAD`, `F_D2` because their long DSD-encoded names are the volatile part.

> **Not committed (confirmed dead).** Three files may sit in a working `raw/` but
> are read by no script and are excluded from the repo: `560114.xlsx` (legacy
> 5601.0 lending, superseded by `F_LEND`; appears only in a comment),
> `f01dhist.xls` (RBA F1 interest rates, referenced nowhere), and the alternative
> all-groups OMAD export `ABS_OMAD_VISA,+..._AUS.Q.csv` (superseded by the
> specific-group `F_OMAD`). See "Files deliberately NOT committed" in
> `raw/README.md`. The committed input set is 14 files.

## raw_rev/ — state bond reports (not committed)

Rents on new leases (Online Appendix F) come from the bond reports of four
state agencies, about 180 MB in all. They are not in the repository; README.md
says where to download each. `01b_new_lease_rents.R` reads:

| Folder | Source | Coverage |
|------|---------------|-------------|
| `raw_rev/nsw/dcj_xlsx/` | NSW DCJ Rent and Sales Report: rent tables (June 2017 on) and the time series "Rents, Trend March 1990 - June 2017, Metropolitan LGAs" | Greater Sydney, 1990 on |
| `raw_rev/nsw/dcj_pdf/` | DCJ report PDFs, March 2008 to March 2017 (optional check; `fetch_rent_reports.R nsw`) | Greater Sydney |
| `raw_rev/vic/` | Homes Victoria, quarterly median rents by LGA; optionally the Rental Report tables (check) | Metropolitan Melbourne, 1999 on |
| `raw_rev/qld/` | Queensland RTA bond statistics | Five Greater Brisbane LGAs, 2012 on |
| `raw_rev/sa/` | SA Private Rental Report, one workbook per quarter (`fetch_rent_reports.R sa`) | Metropolitan Adelaide, 2008 on |

Without `raw_rev/`, `run_all.R` skips `01b` and runs `02f` on the committed
`clean_rev/new_lease_rents.csv`.

## clean/ — intermediates (stage 01 writes; later stages read)

| File | Written by | Read by |
|------|-----------|---------|
| `price_panel.rds`, `price_panel.csv` | 01 | (price reconstruction artefact) |
| `panel_combined.rds` | 01 | 02, 02a, 02c, 02d, 03, 03f, 04, 05 |
| `panel_combined.csv` | 01 | (inspection; also copied to `output/`) |

`panel_combined.rds` is the single analysis panel. Stages 02c, 03f, 04 and 05
also read the seasonally adjusted lending series from `F_LEND` in `raw/`, and
so does 02d for the investor share (Table 2 and Table E.1); every other stage
works from the panel.

## clean_rev/ — new-lease rents (stage 01b writes; 02f reads)

| File | Written by | Read by |
|------|-----------|---------|
| `new_lease_rents.csv` | 01b | 02f (one row per city and quarter: log change, seasonally adjusted change, index) |
| `new_lease_cells.csv` | 01b | (cell-level medians, bonds and weights) |
| `new_lease_checks.txt` | 01b | (coverage, weights, seasonality and source checks) |

Only `new_lease_rents.csv` is committed, with `clean_rev/README.md`, which
describes its columns and gives the sources, licences and attribution. The
other two files are git-ignored.

## output/ — results

### Stage 02 — headline, divergence, robustness

```
est_rent.txt              est_rent_mg.txt
est_price.txt             est_price_predetermined.txt
est_rent_divergence.txt   est_rent_divergence_pre.txt
est_price_divergence.txt  est_price_divergence_pre.txt
est_price_divergence_covid.txt   est_price_divergence_sydney.txt

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

### Stage 02a — migration-disaggregation robustness

```
granular_rent_tightness.txt          numerator horse-race, one file per variant:
granular_rent_tightness_no_chk.txt     omnibus / overseas-only /
granular_rent_tightness_no_stu.txt     overseas ex-students /
granular_rent_tightness_rw.txt         renter-weighted overseas
granular_rent_split_flows.txt        overseas vs interstate, separate slopes
granular_rent_split_waldeq.txt       Wald test of slope equality
granular_rent_split_loo.txt          leave-one-city-out  -> backs Table tab:mig_loo
granular_price_placebo.txt           price placebo (3a, overlapping)
granular_price_placebo_alone.txt     price placebo (3b, non-overlapping)
```

The `granular_rent_split_loo.txt` and `granular_price_placebo_alone.txt` files
back the manuscript's "Disaggregating the migration flow" paragraph and Table
`tab:mig_loo`; keep them with the replication bundle.

### Stage 03 — figures and local projections

```
fig_transmission.pdf          fig_transmission.png       (Figure 1)
local_projection_irf.csv      local_projection_irf.png
lp_rent_lag_sensitivity.csv   fig_lp_rent_lag_sensitivity.png
fig_divergence.png            fig_price_rent_gap.png
fig_real_rent.png             fig_rental_trajectories.png
fig_rent_rolling_coef.png
```

`fig_divergence.png` (Figure E.2) is redrawn by stage 06.

### Stage 02f — rents on new leases (Online Appendix F)

```
new_lease_tests.txt     new_lease_lp.csv     new_lease_boot.csv
fig_new_lease_lp.png    (Figure F.1)
```

### Stages 02c and 02d — seasonally adjusted credit

```
sa_A_price.txt      sa_B_rent.txt       sa_B_omega.csv     sa_C_lp.csv
sa_D_channel.txt    sa_F_oo.txt         sa_G_iv.csv        sa_H_presplice.txt
fig_lp_irf_adjusted.png                 fig_lp_postimpact.png

sa2_S1_stacked.txt  sa2_S2_breaks.txt   sa2_S3_granger.txt sa2_S4_seasonal.txt
sa2_S5_sydney_cities.txt                sa2_S6_cash.txt    sa2_S7_reclass.txt
sa2_S8_summary.txt  sa2_S9_unitroot.txt (Table E.1)
```

### Stages 03f and 03g — APRA instruments

```
lpiv_apra.csv           fig_lpiv_main.png
cr_valid_ranges.csv     fig_cr_valid_range.png
cr_mcub_endpoints.csv   (03g, only with ivcrtest)
```

### Stages 04 and 04b — the model

```
model_estimates.csv     model_profile_J.csv        model_fit.csv
model_decomposition.csv model_counterfactual.csv   model_state_dependence.csv
model_summary.txt       fig_model_fit.png          fig_model_counterfactual.png

model_ident.txt         model_profile.csv          model_profile_omega.csv
model_sensitivity.csv   fig_model_profile.png
```

### Stages 05, 05c and 05b — bootstrap and Monte Carlo

```
boot_panel.rds          cached panel with all leads and lags
boot_b<block>_<first>_<last>.rds   one chunk of draws
mc_small_<first>_<last>.rds        one chunk of pseudo-samples
model_small_sample.txt  model_small_sample.csv
boot_summary.csv        boot_shares.csv            boot_counterfactual_draws.csv
fig_model_fit.png       fig_model_counterfactual.png   (redrawn with bands)
```

### Stage 04c — credit conditions and measured inflation (Section 6.6)

```
model_inflation.txt     model_inflation.csv        fig_model_inflation.png  (Figure 9)
```

### Stage 06 — the paper's figures in one style

Redraws from saved results, at the size each prints in the paper (600 dpi):

```
fig_lp_irf_adjusted.png (Figure 5)     fig_model_fit.png (Figure 6)
fig_model_profile.png (Figure 7)       fig_model_counterfactual.png (Figure 8)
fig_lpiv_main.png (Figure D.1)         fig_cr_valid_range.png (Figure D.2)
fig_lp_postimpact.png (Figure E.1)     fig_divergence.png (Figure E.2)
```

Stages 02c, 03, 03f, 04, 04b, 05c and 05b write older versions of these eight
files, so 06 runs last.

### Stage 01 — optional diagnostics  (only if `BUILD_DIAGNOSTICS = TRUE`)

```
diag_price_levels.png
diag_sydney_splice_vs_bis.png
```

`output/panel_combined.csv` is also written by stage 01 as the single-source copy.

### Standalone diagnostics  (run by hand; NOT in `run_all.R`)

`plot_migration_composition.R` — reads `raw/` OMAD directly (no panel rebuild
needed), writes to `output/`:

```
mig_temp_share.png        temporary-visa share of NOM, by city
mig_student_share.png     student share of NOM, by city
mig_sydney_levels.png     net NOM levels by visa group, Sydney (context)
mig_student_share_4q.png  student share with COVID NA gap visible
```

`check_integration_plots.R` — reads `clean/panel_combined.csv`, writes to
`output/`:

```
check_inv_commit_levels.png        investor lending by city, with 2024Q3 anchor
check_inv_commit_4qsum.png         4q rolling sum (exposes level breaks)
check_inv_commit_seasonality.png   within-year profile (confirms Original, not SA)
check_prop_price_levels.png        reconstructed price by city (splice continuity)
check_inv_intensity.png            derived inv_commit / employment sanity
```

## logs/

`run_all.R` opens `logs/run_log_<YYYYMMDD_HHMMSS>.txt` and sinks both output and
messages there (split to console). One file per run; includes `sessionInfo()`,
a checksum for every script, warnings, the output of each bootstrap chunk, and
per-stage OK/FAIL with timings. `logs/sessionInfo_<YYYYMMDD_HHMMSS>.txt` is
written at the start of each run, so it lists only the packages loaded by then.

## Stage dependency summary

```
00_setup.R ─── sourced by ALL stages (constants, helpers, DK SE)
                 │
raw/ ──────────▶ 01_build_panel.R ──▶ clean/panel_combined.rds ──┬─▶ 02_estimate.R              ──▶ output/est_*, robust_*, mech_*, nlin_*, alt_*, rolling_*
                                                                 ├─▶ 02a_migration_granularity.R ──▶ output/granular_*
                                                                 ├─▶ 02c_sa_additions.R          ──▶ output/sa_*
                                                                 ├─▶ 02d_sa_tables.R             ──▶ output/sa2_*
                                                                 ├─▶ 03_figures.R                ──▶ output/fig_*, local_projection_*, lp_*
                                                                 └─▶ 03f_missing_figures.R ──▶ 03g_cr_endpoints.R

output/sa_B_omega.csv, sa_C_lp.csv ──▶ 04_model_matching.R ──▶ 04b_identification.R
output/model_estimates.csv ──▶ 05_bootstrap.R ──▶ output/boot_b*.rds ──▶ 05c_small_sample.R ──▶ 05b_bootstrap_summary.R
output/boot_b8_*.rds, model_*.csv ──▶ 04c_inflation.R
raw_rev/ ──▶ 01b_new_lease_rents.R ──▶ clean_rev/new_lease_rents.csv ──▶ 02f_new_lease_tests.R (with clean/panel_combined.rds)
output/ (saved results of 02, 02c, 03f, 04, 04b, 05) ──▶ 06_paper_figures.R   (last)
```

`02_estimate.R` writes `rolling_financial_r2.csv`, which `03_figures.R` reads.
Stage 04 needs the outputs of 02c, and stage 05 needs the outputs of 02c and 04.
Stage 04c needs the block-8 bootstrap draws, and 06 needs every stage before it.
Any stage can fail without stopping the later stages from starting;
`run_all.R` reports per-stage status and exits non-zero if any failed. If a
bootstrap chunk fails, it skips 05c and 05b and reports the failure.

## Provisional / known-incomplete data

- **2025Q4 `net_overseas`** carries a chained/forecast value; genuinely
  unpublished on every NOM-concept source to date. Footnote as provisional;
  closed only by the next ABS quarterly NOM release.
- **OMAD visa shares** (`nom_temp_share`, `nom_student_share`) end one quarter
  before the panel and are `NA` across the 2020-21 border-closure window by
  construction (~39 of 385 city-quarters NA: warm-up + COVID + out-of-band
  guard). Any specification using the shares drops those quarters.

## Refreshing data next quarter

1. Edit `TARGET_END` in `00_setup.R`.
2. Drop new ABS files in `raw/`; if API filenames changed, update `F_LEND` /
   `F_OMAD` / `F_D2` in `00_setup.R`.
3. Update the hand-typed `cash_path` in `01_build_panel.R` (asserted to reach
   `TARGET_END`, so it fails loudly if stale).
4. Check the per-city column maps for the xlsx tails; `assert_tail_ok()` flags a
   layout change.
5. `Rscript run_all.R`. The stage-01 coverage report prints the last non-NA
   quarter per variable; confirm the new sources reach the expected quarter
   (OMAD shares end one quarter early and carry the COVID NA gap by design).
