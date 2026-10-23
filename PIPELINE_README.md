# Pipeline

This file explains how the pipeline works: the data, how the scripts came to
be, what changed for the October 2026 revision of the paper, and what the
mechanism checks find. `README.md` lists the 13 steps, the tables and figures
each one produces, and how to run them. `STRUCTURE.md` maps every input,
intermediate and output file.

## Run

```
Rscript run_all.R            # every step, 01 to 03f
Rscript run_all.R 02c 02d    # only these steps, by code or file name
```

When a run includes step 01 or 01a, `run_all.R` first runs `verify_raw.R`,
which checks that `raw/` holds the 14 input files under their expected names.
It then runs each step in its own R session, as if you ran it by hand from the
repository root, so no step sees another step's objects. A step whose input
step failed in the same run is skipped. Every script writes a log to
`run_logs/`.

Optional diagnostics of the price panel:

```r
options(BUILD_DIAGNOSTICS = TRUE); source("01_build_panel.R")
```

## Data sources

The panel combines the ABS xlsx workbooks (price 6416.0, migration 3101.0,
completions 8752.0, rents 6401.0, wages 6345.0, employment 6202.0), state final
demand, and three modern-API dataflows. `00_setup.R` holds the filenames of the
three dataflows (`F_LEND`, `F_OMAD`, `F_D2`), because their long names are the
part that changes on a refresh.

- **Investor lending** (`F_LEND`, ABS Lending Indicators `LEND_HOUSING`), the
  successor to 5601.0 Table 14. Step 01 takes `inv_commit`, the original value
  of new investor housing loan commitments ($m), for the five mainland states
  from 2006 Q3. The file covers the whole sample, so it serves both the panel
  join and the extension tail without growth-chaining, and it matches the
  legacy series to the unit. Step 01a reads the same file again for the
  seasonally adjusted values of investor and owner-occupier commitments. The
  seasonally adjusted investor value is the paper's main credit measure. The
  investor share of new lending, investor over investor plus owner-occupier
  commitments, both seasonally adjusted, is the second. The original series
  remains as a comparison.

- **RBA D2 national housing credit** (`F_D2`, `d02hist.xlsx`): national investor
  and owner-occupier credit stocks, and the net flow of loans switched between
  the two purposes. The series are national, so they vary only over time and
  cannot be a cross-city headline regressor. They serve three checks: the
  external shift in the Bartik IV, the 2015-17 reclassification check and the
  national investor-share check. Derived series: `inv_credit_nat`,
  `oo_credit_nat`, `net_switch_nat`, `inv_credit_nat_adj` (adjusted for
  switching) and their `dln_*` growth rates.

- **Overseas migration by visa** (`F_OMAD`, ABS `OMAD_VISA`): quarterly net
  overseas migration by visa and citizenship group, by state, on the 12/16-month
  rule, 2006 Q3 to 2025 Q3. Its total matches the panel's `net_overseas`
  (3101.0) to the unit, so `net_overseas` stays as it is. What the file adds is
  the composition of migration. Derived series: `nom_temp_share` and
  `nom_student_share`.

### Visa shares

The visa shares are ratios of four-quarter rolling sums, not of single
quarters. The quarterly ratio is unusable: total net overseas migration fell
below zero during the 2020-21 border closures, so the quarterly shares reach
values such as +1400 and -2600 percent. Two guards, defined in `00_setup.R`,
set a share to `NA` when it is not meaningful:

- `NOM_FLOOR <- 5000`: the minimum four-quarter total for a share;
- `SHARE_BAND <- c(-0.05, 1.05)`: the plausible range.

The 2020-21 quarters are therefore `NA` by construction. A specification that
uses the shares drops them, and the rent equation's sample falls from 370 to
342 city-quarters. The estimating variables need no transformation for skew:
the differenced logs are nearly symmetric, the shares are bounded and only
mildly skewed, and tightness stays in levels, because its log is far more
skewed near zero.

## June 2026: four stages from nine scripts

The June pipeline merged nine numbered scripts into four steps and a shared
setup file, with the same logic and outputs.

| File | Replaces |
|------|----------|
| `00_setup.R` | the namespace, path and constant blocks copied into each script; ABS read and chain helpers; the Driscoll-Kraay errors; the modern-API filenames and the visa-share guards |
| `01_build_panel.R` | `01_build_price_panel.R` and `03_build_combined_panel.R`, with the optional diagnostics of `02_check_price_plots.R` |
| `02_estimate.R` | `04_estimate.R` and `06_price_iv_robustness.R` |
| `02a_migration_granularity.R` | new: migration split by type (see below) |
| `03_figures.R` | `05_plot_divergence.R` and `07_local_projections.R` |
| `run_all.R` | `00_master.R` |
| `verify_raw.R` | new: read-only check of `raw/` |

`figure1_transmission.R` was removed: Figure 1 is drawn in TikZ in the
manuscript.

Beyond the merge:

1. **One convention for standard errors.** Every panel regression uses
   Driscoll-Kraay errors (`plm::vcovSCC`, through `dk_coeftest()` in
   `00_setup.R`), as the paper does. The static tables had used Arellano
   errors while the local projections used Driscoll-Kraay.
2. **The price index is built once.** `01_build_panel.R` builds it in its
   stage A and reuses it for the price panel and for the combined panel.
3. **One set of ABS readers.** About eight near-identical
   read-slice-parse-chain blocks became `read_abs_xlsx_col()`,
   `to_quarterly()`, `chain_tail()` and `build_xlsx_tail()`.
4. **Stale data stops the run.** `assert_tail_ok()` checks that each tail
   covers every city-quarter with finite values; the hand-typed cash-rate path
   must reach `TARGET_END`; and the lending and migration reads require all
   five cities. A layout change therefore fails loudly instead of leaving
   silent `NA`s.
5. **The local projections build their panel once.** The horizon loop adds
   all leads as columns and reuses one panel, instead of rebuilding it 26
   times.

## October 2026 revision

1. **Seasonally adjusted credit.** The original investor-lending series is
   strongly seasonal: quarter-of-year and city dummies explain about 45 percent
   of its quarterly log change, against 1 percent for the adjusted series. Its
   seasonal noise also faded after 2017, which made the credit-price link look
   stronger after 2017 than it was. The revision's steps therefore use the
   seasonally adjusted series as the main measure (see "Data sources").
2. **Nine new steps.**
   - `01a_build_test_panel.R` builds `clean/panel_tests`: adjusted credit, the
     investor share, indicators of the APRA lending measures, lags and sample
     flags.
   - `02b_break_predictive_tests.R`: break tests at known and unknown dates,
     Granger tests, and stacked tests of the price and rent slopes.
   - `02c_main_tables_sa.R`: Tables 2, 4 and 5 and most Appendix C checks, on
     adjusted credit, with a wild cluster bootstrap over all 7,776 Webb draws.
   - `02d_unit_root_tests.R`: panel (Im-Pesaran-Shin) and city ADF tests of
     every series the equations use (Table 8).
   - `03b_local_projections.R`: local projections on adjusted credit
     (Figure 5).
   - `03c_lp_iv_apra.R`: local projections instrumented with the APRA
     investor-lending benchmark and interest-only cap (Figure 6).
   - `03d_cr_test_apra.R`: the correlation-restriction test of those
     instruments, with routines from `lib/ivcrtest`.
   - `03e_cr_table_figure.R`: Table 6 and Figure 7.
   - `03f_descriptive_figures.R`: Figures 2, 3, 4 and 8.
3. **The wage series is dated correctly.** In `panel_main.csv`, the wage price
   index at each quarter held the 6345.0 value of the next quarter, so wage
   growth was one quarter ahead of rent growth. Step 01 now takes the index by
   quarter from 6345.0, as it already did for migration and completions. The
   fix changes the wage and employment coefficients of the rent equation
   (Table 3) and every check that uses wage growth.
4. **Two unused series dropped.** `labor_income` and `prop_price_8cities` left
   the panel: no source extends them past 2024 Q3, and no estimation uses them.
5. **A log for every run.** `00_run_log.R` writes a log of each script to
   `run_logs/`. `run_all.R` now runs each step in its own R session and skips a
   step whose input step failed. The `{targets}` pipeline was removed, because
   it covered only the four June steps.
6. **Three changes to `02_estimate.R`.** The wild bootstrap seeds both random
   number generators it uses, R's (`set.seed()`) and dqrng's
   (`dqrng::dqset.seed()`), so its p-value repeats exactly. The rolling-window regressions report
   Driscoll-Kraay p-values, whose counts Section 4.2 quotes. The cointegration
   fallback judges each city's residual ADF statistic against the Engle-Granger
   5 percent critical value (about -3.42); the Pedroni critical values apply
   only after the group mean is standardised.

## Mechanism and nonlinearity checks

The numbers below come from the run of 23 October 2026.

- **Migration composition** (`02_estimate.R`, section 9). Does the rent
  response to tightness grow when more of the inflow rents, as students and
  temporary visa holders do? The student share moves enough to test this: its
  standard deviation within cities is 0.96 of its overall standard deviation.
  The interaction of tightness with the student share is 0.001 (p = 0.84), the
  temporary-share version is also null (p = 0.55), and so is the same
  interaction in the price equation (p = 0.96). The paper reports this null in
  Appendix C.

- **Tightness nonlinearity** (`02_estimate.R`, section 10). A natural spline in
  tightness mildly rejects linearity (chi-squared(2) = 6.7, p = 0.04), but not
  in the convex form that rental pressure would predict: the extra slope above
  each city's median tightness is negative and insignificant (-0.0026,
  p = 0.20), and the same split in the price equation is larger (0.012,
  p = 0.04). The rent equation keeps the linear term, and the paper makes no
  claim of convexity (Appendix C).

- **Migration split by type** (`02a_migration_granularity.R`; not reported in
  the paper).
  1. Different numerators for tightness, on a common sample of 342
     city-quarters: total net migration gives a within-R^2 of 0.55 and a slope
     of 0.0056. Overseas migration alone gives 0.51 (slope 0.0057), overseas
     migration without students 0.58 (0.0119), and renter-weighted overseas
     migration 0.49 (0.0073). Leaving out students raises the fit a little and
     about doubles the slope.
  2. Separate slopes for overseas and interstate migration: 0.0049 and 0.0102,
     and the test rejects equal slopes (chi-squared(1) = 4.62, p = 0.03).
     Leaving out one city at a time, the difference keeps its sign, with |t|
     between 1.9 and 4.2, except without Perth, where it reverses (t = 0.5).
     The difference rests on Perth.
  3. A price placebo: renter-weighted overseas migration adds nothing to the
     price equation alongside total tightness (-0.008, p = 0.12). On its own,
     it is positive and significant (0.008, p < 0.001), so this placebo does
     not cleanly keep migration out of prices.

### What was deliberately not added

These were considered and rejected on methodological grounds:

- **A dummy for the bimodal student share.** The two modes are the pandemic
  break, not two economic regimes. With the composition test already a
  well-powered null, splitting the share by its own value would be circular
  and would look like a specification search.
- **GMM or Arellano-Bond.** These need many units and few periods. This panel
  has 5 cities and about 78 quarters, the opposite.
- **More instruments or other standard errors for the Bartik IV.** Its
  first-stage F is about 5, and more instruments would manufacture precision.
  Driscoll-Kraay errors already allow for heteroskedasticity, autocorrelation
  and correlation across cities.

The results rest on the lagged-credit specifications, the timing of the local
projections, and the instrumented projections with the correlation-restriction
test of the APRA instruments (steps 03c to 03e), not on the Bartik IV. With
only five cities, every standard error has limited precision. The wild cluster
bootstrap of `02c_main_tables_sa.R` is the small-sample check the paper
reports.

## Outstanding

- **Vacancy rate.** A direct measure of rental tightness would validate the
  constructed proxy (four-quarter migration over four-quarter completions). It
  would fit into the tightness checks of `02_estimate.R` (section 7.2). Before
  adding it, decide whether it replaces the proxy as the headline measure or
  validates it as a robustness check; the second needs no rewrite of the rent
  sections.
- **Net migration for 2025 Q4.** ABS has not yet published it, so the panel
  leaves it missing. Net migration and tightness end in 2025 Q3; tightness
  enters lagged one quarter, so 2025 Q4 is still the last estimation quarter.

## Refreshing data next quarter

1. Edit `TARGET_END` in `00_setup.R`.
2. Put the new files in `raw/`. If the API filenames changed, update `F_LEND`,
   `F_OMAD` or `F_D2` in `00_setup.R`.
3. Update the hand-typed `cash_path` in `01_build_panel.R`. A check stops the
   run if it does not reach `TARGET_END`.
4. Check the per-city column maps for the xlsx tails; `assert_tail_ok()` flags
   a layout change.
5. Run `Rscript run_all.R`. The coverage report of step 01 prints the last
   quarter with data for each variable; confirm that the new files reach the
   expected quarter. The visa shares end one quarter before the panel and are
   missing in 2020-21 by design. New data changes every result of 03d, so its
   next run takes a few hours.
