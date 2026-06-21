# Repository structure

Full layout of scripts, inputs, intermediates, and outputs for the rent/price
divergence pipeline. For *how* the pipeline works and what each test concludes,
see `PIPELINE_README.md`; this file is the map of *what lives where* and *what
depends on what*, so a data refresh shows exactly which files to touch.

## Directory tree

```
project-root/
│
├── run_all.R                      entry point: Rscript run_all.R
│                                  sources 00_setup.R; runs 01 -> 02 -> 02a -> 03
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
├── 02_estimate.R                  STAGE 2  rent/price equations, divergence, IV
├── 02a_migration_granularity.R    STAGE 2a migration-disaggregation robustness
├── 03_figures.R                   STAGE 3  divergence figures, local projections
│
│   standalone diagnostics (NOT in run_all.R; run by hand):
├── plot_migration_composition.R   NOM visa-share diagnostic figures
├── check_integration_plots.R      integrated-panel integrity figures
│
├── PIPELINE_README.md
├── STRUCTURE.md                   (this file)
│
├── raw/        INPUTS  (you supply; treated as read-only)
├── clean/      INTERMEDIATE  (written by stage 01, read by 02/02a/03)
├── output/     RESULTS  (tables, series, figures)
└── logs/       per-run logs
```

## Naming convention

Numbered scripts are pipeline stages run in order by `run_all.R`
(`01`, `02`, `02a`, `03`). The `a` suffix denotes a stage that depends on and
extends the same-numbered stage (`02a` reads the panel and reuses `02`'s
inference machinery, but is logically a continuation of estimation). Unnumbered
`.R` files are either sourced helpers (`00_setup.R`, `R/functions.R`) or hand-run
diagnostics not in the master run.

> Two standalone diagnostics had misspelled filenames as first uploaded
> (`plot_migration_compisition.R`, `check_integartion_plots.R`); the correct
> names are `plot_migration_composition.R` and `check_integration_plots.R`, as
> referenced here and in `PIPELINE_README.md`. Both now source `00_setup.R` when
> present (using `OUTDIR`, `F_OMAD`, `CITIES`) and fall back to standalone
> definitions when run alone, writing to `output/` either way.

> **Figure 1 is TikZ, not R.** The policy-transmission diagram (Figure 1) is
> drawn natively in a `tikzpicture` inside the manuscript `.tex`. An earlier R
> rendering (`figure1_transmission.R` → `output/figures/housing_paradox_*.png/pdf`)
> duplicated it and has been removed from the pipeline: `03_figures.R` no longer
> sources it, and the `targets` graph no longer tracks those outputs. If a copy
> of `figure1_transmission.R` or the `housing_paradox_*` files remains in the
> repo, they are stale and can be deleted.

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
> `raw/README.md`. The committed input set is 13 files.

## clean/ — intermediates (stage 01 writes; 02/02a/03 read)

| File | Written by | Read by |
|------|-----------|---------|
| `price_panel.rds`, `price_panel.csv` | 01 | (price reconstruction artefact) |
| `panel_combined.rds` | 01 | 02, 02a, 03 |
| `panel_combined.csv` | 01 | (inspection; also copied to `output/`) |

`panel_combined.rds` is the single analysis panel. Every estimation and figure
stage reads it; nothing downstream re-reads `raw/`.

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
local_projection_irf.csv      local_projection_irf.png
lp_rent_lag_sensitivity.csv   fig_lp_rent_lag_sensitivity.png
fig_divergence.png            fig_price_rent_gap.png
fig_real_rent.png             fig_rental_trajectories.png
fig_rent_rolling_coef.png
```

Figure 1 (transmission diagram) is TikZ in the `.tex`, not an R output.

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
messages there (split to console). One file per run; includes `sessionInfo()`
and per-stage OK/FAIL with timings.

## Stage dependency summary

```
00_setup.R ─── sourced by ALL stages (constants, helpers, DK SE)
                 │
raw/ ──────────▶ 01_build_panel.R ──▶ clean/panel_combined.rds ──┬─▶ 02_estimate.R              ──▶ output/est_*, robust_*, mech_*, nlin_*, alt_*, rolling_*
                                                                 ├─▶ 02a_migration_granularity.R ──▶ output/granular_*
                                                                 └─▶ 03_figures.R                ──▶ output/fig_*, local_projection_*, lp_*
```

Only stage 01 touches `raw/`. Stages 02, 02a, 03 each read `panel_combined.rds`
and are independent of one another (any can fail without blocking the others;
`run_all.R` reports per-stage status and exits non-zero if any failed).

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
```
