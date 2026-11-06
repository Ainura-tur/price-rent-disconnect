# Credit, Fundamentals, and the Price–Rent Disconnect in Australian Housing Markets

Replication package for the paper *Credit, Fundamentals, and the Price–Rent
Disconnect in Australian Housing Markets: Evidence from Five Capital Cities,
2006–2025*.

The paper models a city housing market in which investors price dwellings under
a collateral constraint while rents clear against renter incomes, reprice only as
leases roll over, and respond to prices through slow tenure switching. It
estimates a rent equation and a price equation on the time dimension within five
capital cities, traces the timing with panel local projections, instruments
credit with APRA's 2015–2018 lending measures, and estimates the model by
matching its impulse responses to the projections.

## Quick start

From the repository root:

```sh
Rscript run_all.R               # every stage except the bootstrap (about 15 minutes)
Rscript run_all.R --bootstrap   # also the bootstrap and the Monte Carlo (about an hour)
```

From the R console, with the repository root as the working directory:

```r
system2(file.path(R.home("bin"), "Rscript"), c("run_all.R", "--bootstrap"))
```

`run_all.R` first checks the raw inputs (`verify_raw.R`), then runs the stages
below in order. It writes a log of the run to `logs/`, with `sessionInfo()`, a
checksum for every script, and each stage's output and warnings.

The bootstrap saves each chunk of 30 draws as it finishes, so an interrupted run
resumes where it stopped. Each draw sets its own seed, so the number of cores
does not change the results. On Windows, six chunks run side by side, each in
its own R process. On macOS and Linux, each chunk forks across all cores. The
times above are for a six-core laptop.

`output/` is not in the repository; a full run rebuilds it.

Online Appendix F reads `clean_rev/new_lease_rents.csv`, which the repository
includes, so `run_all.R` runs `02f_new_lease_tests.R` with no further downloads.
Rebuilding that file with `01b_new_lease_rents.R` needs the state bond reports
in `raw_rev/`, which the repository does not hold (see "Rents on new leases"
under Data). Without them, `run_all.R` skips `01b`.

The dependency-tracked alternative, `targets::tar_make()`, runs stages 01 to 04b
except `01b`, `02f` and `03g`. It does not run the bootstrap, the Monte Carlo,
`04c` or `06`.

## Pipeline

| Stage | Script | What it produces |
|---|---|---|
| 1 | `01_build_panel.R` | Analysis panel `clean/panel_combined.rds` |
| 1b | `01b_new_lease_rents.R` | Indices of rents on new leases for Sydney, Melbourne, Brisbane and Adelaide, `clean_rev/new_lease_rents.csv` (needs `raw_rev/`) |
| 2 | `02_estimate.R` | Tables 3–4, Table C.1, rolling partial R² and rolling rent coefficients, most Online Appendix E checks |
| 2a | `02a_migration_granularity.R` | Migration-disaggregation robustness |
| 2c | `02c_sa_additions.R` | Price momentum (Table 4), dynamic rent equation (ω), local projections (Figure 5, Figure E.1), channel test, owner-occupier credit, pre-splice sample |
| 2d | `02d_sa_tables.R` | Tables 1–2, Tables C.2–C.3, stacked slope tests, seasonal-adjustment checks, Sydney and city-by-city changes, cash-rate interaction and level, unit-root tests (Table E.1) |
| 2f | `02f_new_lease_tests.R` | Rents on new leases against the model: Tables F.1–F.2 and Figure F.1 (Online Appendix F) |
| 3 | `03_figures.R` | Figure 1 (`fig_transmission.pdf`) and Figures 2–4 |
| 3f | `03f_missing_figures.R` | APRA-instrumented local projections and the correlation-restriction test: Table D.1, Figures D.1–D.2 |
| 3g | `03g_cr_endpoints.R` | MCUB valid-range endpoints used in Figure D.2 (needs `ivcrtest`) |
| 4 | `04_model_matching.R` | Model estimates, decomposition, counterfactual, state dependence (Section 6) |
| 4b | `04b_identification.R` | Profiles of the matching distance and sensitivity of the responses to each parameter (Section 6.3, Figure 7) |
| 5 | `05_bootstrap.R` | Block bootstrap of the model: 399 draws with blocks of eight quarters and 120 each with blocks of four and twelve, saved in 22 chunks |
| 5c | `05c_small_sample.R` | Recentred-bootstrap Monte Carlo of the estimator's small-sample behaviour (Section 6.3) |
| 5b | `05b_bootstrap_summary.R` | Block-bootstrap intervals for Tables 5–6 and bands in Figures 6 and 8 |
| 4c | `04c_inflation.R` | Credit conditions and measured inflation: annual price and rent inflation in the model, contributions to CPI inflation, bootstrap timing (Section 6.6, Figure 9) |
| 6 | `06_paper_figures.R` | Redraws Figures 5–8, D.1–D.2 and E.1–E.2 in one style, at the size each prints in the paper |

Stages 5, 5c and 5b run only with `--bootstrap`. Stage 5c runs before 5b because
it sources `04_model_matching.R`, which redraws Figures 6 and 8 without bands.
Stage 4c reads the block-8 bootstrap draws, so it runs after them, or in a later
run that finds a full set in `output/`. Stage 6 runs last, because stages 02c,
03, 03f, 04, 04b, 05c and 05b write older versions of its eight figures.

Outputs go to `output/`. Each script's header lists the files it writes.
`03_figures.R` draws Figure 1; the paper includes `output/fig_transmission.pdf`.

## Where each result comes from

| Paper | File in `output/` |
|---|---|
| Figure 1 | `fig_transmission.pdf` |
| Figures 2–4 | `fig_rental_trajectories.png`, `fig_price_rent_gap.png`, `fig_real_rent.png` |
| Table 1 | `sa2_S8_summary.txt` (`table1`) |
| Table 2 | `sa2_S8_summary.txt` (`table2`) |
| Table 3 | `est_rent.txt`, `est_rent_mg.txt` |
| Table 4 | `sa_A_price.txt` |
| Dynamic rent equation, ω (Section 5.2) | `sa_B_rent.txt`, `sa_B_omega.csv` |
| Rent-equation stability, rolling coefficients (Online Appendix E) | `robust_rent_stability.txt`, `alt_rent_trend.txt`, `alt_rent_rolling_coef.csv` |
| Figure 5, Figure E.1, local projections | `sa_C_lp.csv`, `fig_lp_irf_adjusted.png`, `fig_lp_postimpact.png` |
| Channel test (Section 5.4) | `sa_D_channel.txt` |
| Tables 5–6, Figures 6 and 8 | `model_*.csv`, `boot_summary.csv`, `fig_model_fit.png`, `fig_model_counterfactual.png` |
| Identification (Section 6.3), Figure 7 | `model_ident.txt`, `model_profile.csv`, `model_profile_omega.csv`, `model_sensitivity.csv`, `fig_model_profile.png` |
| Small-sample behaviour (Section 6.3) | `model_small_sample.txt`, `model_small_sample.csv` |
| Credit conditions and measured inflation (Section 6.6), Figure 9 | `model_inflation.txt`, `model_inflation.csv`, `fig_model_inflation.png` |
| Table C.1 | `est_price_divergence*.txt`, `est_rent_divergence*.txt`; stacked test in `sa2_S1_stacked.txt` |
| Tables C.2–C.3 | `sa2_S2_breaks.txt`, `sa2_S3_granger.txt` |
| Table D.1, Figures D.1–D.2 | `lpiv_apra.csv`, `fig_lpiv_main.png`, `cr_valid_ranges.csv`, `fig_cr_valid_range.png`, `cr_mcub_endpoints.csv` |
| Online Appendix E | `sa2_S4`–`sa2_S7`, `sa_F_oo.txt`, `sa_H_presplice.txt`, `robust_*.txt`, `alt_price_spline.txt`, `mech_*.txt`, `nlin_*.txt` |
| Figure E.2 | `rolling_financial_r2.csv`, `fig_divergence.png` |
| Table E.1, and the autocorrelations in Section 4.3 | `sa2_S9_unitroot.txt` |
| Online Appendix F: Tables F.1–F.2, Figure F.1 | `new_lease_tests.txt`, `new_lease_lp.csv`, `new_lease_boot.csv`, `fig_new_lease_lp.png`; the index itself in `clean_rev/new_lease_rents.csv` |

The files for Figures 5–8, D.1–D.2 and E.1–E.2 are the versions that
`06_paper_figures.R` draws last.

## Requirements

R (≥ 4.1) with `dplyr`, `tidyr`, `readr`, `readxl`, `zoo`, `purrr`, `plm`,
`lmtest`, `sandwich`, `car`, `splines`, `ggplot2` and `parallel` (base R). The
final run used R 4.5.2 on Windows 11. A full run with R 4.5.3 on Linux,
bootstrap included, reproduces Tables 1–6, C.1–C.3, D.1, E.1 and F.1–F.2 and the
bootstrap results in Sections 6.3–6.6, with two small exceptions.
The data pin down κ and η only weakly (the ridge in Figure 7), so a few
bootstrap and Monte Carlo estimates stop at different points on the ridge on
different platforms. On Linux, the correlation of κ and η across the 399
draws is −0.53 (−0.52 in Section 6.3), and the Monte Carlo median of κ is
0.20, with 90 percent of the estimates between 0.02 and 0.68 (0.21, and 0.03
to 0.66, in Section 6.3).

Optional:
- `ivcrtest` for `03g_cr_endpoints.R`:
  `remotes::install_github("Ainura-tur/ivcrtest")`. Without it, `run_all.R`
  skips that stage; the endpoints it computes are already recorded in
  `03f_missing_figures.R`.
- `fixest` for the shift-share (Bartik) IV in section 5 of `02_estimate.R`. The
  paper no longer reports that IV, and the stage skips it when `fixest` is absent.
- `pco` for the panel cointegration checks in `02_estimate.R`. Without it, the
  stage falls back to group-mean residual ADF tests.
- `pdftools` for `01b_new_lease_rents.R` to read the DCJ report PDFs, which it
  uses only as a check on DCJ's time series.
- `jsonlite` and `curl` for `fetch_rent_reports.R`.
- `targets` for the dependency-tracked pipeline.

Figure 7's panel titles use Greek letters, which need R in a UTF-8 locale
(Windows R 4.2 or later, and most macOS and Linux set-ups).

## Layout

```
00_setup.R                     shared constants, paths, helpers, Driscoll–Kraay SE
01_build_panel.R ... 06_paper_figures.R   pipeline stages (table above)
run_all.R   / _targets.R       two entry points (see Quick start)
verify_raw.R                   pre-run check that raw/ holds the expected inputs
R/functions.R                  stage wrappers for the targets pipeline
fetch_rent_reports.R           downloads the SA bond workbooks and NSW report PDFs into raw_rev/
plot_migration_composition.R   standalone diagnostic (not in the master run)
check_integration_plots.R      standalone diagnostic (not in the master run)
manuscript/                    the paper: PDF of the November 2026 version; LaTeX source,
                               bibliography and figures of the June 2026 version
raw/                           input data; see raw/README.md for sources and licences
raw_rev/                       state bond reports for 01b (not in the repository; see Data)
clean_rev/                     rents on new leases from 01b; only new_lease_rents.csv is
                               committed (sources and licences in clean_rev/README.md)
clean/  output/  logs/         regenerated by the pipeline (git-ignored)
```

## Data notes

- **Credit.** Every estimate in the paper uses seasonally adjusted investor
  loan commitments (`inv_commit_sa`, ABS Lending Indicators). The original
  series (`inv_commit`) is kept for the checks that compare the two.
- **Dating.** Net migration, dwelling completions and the wage price index are
  dated by the quarter they describe. The legacy file `raw/panel_main.csv` held
  all three one quarter early. `01_build_panel.R` replaces them with the ABS
  releases (3101.0, 8752.0 and 6345.0), matched by series ID.
- **Rents on new leases.** `01b_new_lease_rents.R` chains quarterly changes in
  median rents of new bonds within fixed cells of dwelling type and bedrooms,
  weighted by each cell's share of new bonds. They enter only the tests of
  Online Appendix F. Perth has no published bond medians.

## Data

Raw inputs come from the Australian Bureau of Statistics (ABS), the Reserve Bank
of Australia (RBA), and the Bank for International Settlements (BIS). All three
permit reproduction with attribution, so the files are included here under their
respective source terms. **The repository code is MIT-licensed
([`LICENSE`](LICENSE)); the raw data is not — each data file retains its source's
licence.** Full per-file sourcing, series IDs, and attribution are in
[`raw/README.md`](raw/README.md).

If you re-download fresher data, only a few constants change; see "Refreshing
data next quarter" in `STRUCTURE.md`.

### Rents on new leases

Online Appendix F uses median rents of new bonds, which four state agencies
publish. The repository includes the index that `01b_new_lease_rents.R` builds
from them, `clean_rev/new_lease_rents.csv`; [`clean_rev/README.md`](clean_rev/README.md)
gives its sources, licences and attribution. The reports themselves are not in
the repository (about 180 MB). To rebuild the index, save them under `raw_rev/`
with the names as downloaded; `01b_new_lease_rents.R` lists what it reads in its
header.

| Folder | Source |
|---|---|
| `raw_rev/nsw/dcj_xlsx/` | NSW Department of Communities and Justice, [Rent and Sales Report](https://dcj.nsw.gov.au/about-us/families-and-communities-statistics/housing-rent-and-sales/rent-and-sales-report.html) and [previous reports](https://dcj.nsw.gov.au/about-us/families-and-communities-statistics/housing-rent-and-sales/previous-rent-and-sales-reports.html): the rent tables from June 2017 (issue 120) on, and the time series "Rents, Trend March 1990 - June 2017, Metropolitan LGAs" |
| `raw_rev/nsw/dcj_pdf/` | Optional: the DCJ report PDFs for March 2008 to March 2017, a check on the time series (`Rscript fetch_rent_reports.R nsw`) |
| `raw_rev/vic/` | Homes Victoria, [Rental Report - Quarterly: Quarterly Median Rents by LGA](https://discover.data.vic.gov.au/dataset/rental-report-quarterly-quarterly-median-rents-by-lga) (DataVic); optionally the Rental Report tables with the Metropolitan Rent Index, as a check |
| `raw_rev/qld/` | Queensland Residential Tenancies Authority, [median rents quarterly data](https://www.rta.qld.gov.au/forms-resources/rta-quarterly-data/median-rents-quarterly-data) |
| `raw_rev/sa/` | South Australian Housing Trust, [Private Rent Report](https://data.sa.gov.au/data/dataset/private-rent-report), one workbook per quarter (`Rscript fetch_rent_reports.R sa`) |

All four agencies publish these data under Creative Commons Attribution
licences: CC BY 4.0, except CC BY 3.0 Australia for the Queensland RTA.

## Citation

If you use this code or build on the analysis, please cite the paper (citation
to be added on publication) and attribute the data sources as set out in
`raw/README.md`.

## Licence

Code: MIT (see [`LICENSE`](LICENSE)). Data: see [`raw/README.md`](raw/README.md).
