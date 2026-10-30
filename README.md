# Credit, Fundamentals, and the Price–Rent Disconnect in Australian Housing Markets

Replication package for the paper *Credit, Fundamentals, and the Price–Rent
Disconnect in Australian Housing Markets: Evidence from Five Capital Cities,
2006–2025*.

The paper estimates two reduced-form equations on variation within each of
Australia's five largest capital cities over time. Rents track local
fundamentals: market tightness (net migration relative to dwelling
completions), wage growth and employment growth. Investor credit moves closely
with prices and hardly at all with rents, before and after 2017, and the link
does not strengthen significantly after 2017. Panel local projections show that
prices respond to a credit innovation first and rents follow slowly.
Instrumenting credit with the prudential regulator's 2015–2018 limits on
investor lending supports the delay in rents.

## Quick start

From the repository root:

```
Rscript run_all.R             # every step, 01 to 03f
Rscript run_all.R 02c 02d     # only these steps
```

When a run includes step 01 or 01a, `run_all.R` first runs `verify_raw.R`,
which checks that `raw/` holds the 14 input files under their expected names.
It then runs each step in its own R session, in the order below, and skips a
step whose input step failed. Every
script writes a log to `run_logs/`, and `run_logs/run_history.csv` gets one row
per run. In RStudio, source `run_all.R`; to run a subset there, set `RUN` near
the top of the file.

ABS Data Explorer downloads need renaming before a first run; see
[`raw/README.md`](raw/README.md). To check `raw/` on its own, run
`Rscript verify_raw.R`.

## Steps

| Step | Script | Needs | Main outputs |
|------|--------|-------|--------------|
| 01 | `01_build_panel.R` | `raw/` | `clean/panel_combined`, the analysis panel |
| 01a | `01a_build_test_panel.R` | 01 | `clean/panel_tests`: adjusted and original investor credit, the investor share, indicators of the APRA measures, lags and sample flags |
| 02 | `02_estimate.R` | 01 | the rent equation (Table 3), split-sample and rolling-window checks, and robustness checks in Appendix C |
| 02a | `02a_migration_granularity.R` | 01 | checks that split migration by type (not reported in the paper) |
| 02b | `02b_break_predictive_tests.R` | 01a | break tests and Granger tests (Tables 6 and 7), and stacked price–rent tests |
| 02c | `02c_main_tables_sa.R` | 01a | Tables 2, 4 and 5, and the Appendix C checks on adjusted credit, including the cash-rate interaction and the test of Proposition 3 (credit growth times the cash-rate level) |
| 02d | `02d_unit_root_tests.R` | 01a | the unit-root tests (Table C.1) |
| 03 | `03_figures.R` | 02 | figures and local projections of the earlier version, on the original credit series |
| 03b | `03b_local_projections.R` | 01a | local projections on adjusted credit (Figure 5) |
| 03c | `03c_lp_iv_apra.R` | 01a | local projections instrumented with the APRA measures (Figure 6) |
| 03d | `03d_cr_test_apra.R` | 01a | the correlation-restriction test of the APRA instruments |
| 03e | `03e_cr_table_figure.R` | 03c, 03d | Table 8 and Figure 7 |
| 03f | `03f_descriptive_figures.R` | 01a | Figures 2, 3, 4 and C.1 |

Table and figure numbers refer to the October 2026 revision of the paper.
`03f` runs after `03` because both write Figures 2, 3, 4 and C.1 under the same
file names, and the paper uses the versions from `03f`. Figure 1 is drawn in
TikZ in the manuscript.

Step 03d needs the routines in `lib/ivcrtest`. Its first run takes up to a few
hours. It caches its results in `output/cr_cache/`, so later runs take seconds.

See [`PIPELINE_README.md`](PIPELINE_README.md) for the methods and checks, and
[`STRUCTURE.md`](STRUCTURE.md) for the file map and the dependencies between
steps.

## Requirements

R 4.1 or later, with `dplyr`, `tidyr`, `readr`, `readxl`, `zoo`, `purrr`,
`tibble`, `plm`, `lmtest`, `sandwich`, `fixest` and `ggplot2`. A few checks in
`02_estimate.R` and `02a_migration_granularity.R` also use `car`,
`fwildclusterboot` (with `dqrng`) and `pco`. Without them, those checks are
skipped or computed another way, and the log says so. Step 03d also needs
`MASS` and `Matrix`, which come with R.

## Layout

```
run_all.R                      runs the pipeline (see Quick start)
verify_raw.R                   checks that raw/ holds the expected inputs
00_setup.R                     shared constants, paths, helpers, Driscoll–Kraay errors
00_run_log.R                   writes a log of every run to run_logs/
01_build_panel.R ... 03f_descriptive_figures.R    the 13 steps above
lib/ivcrtest/                  correlation-restriction test routines, used by 03d
plot_migration_composition.R   standalone diagnostic (not run by run_all.R)
check_integration_plots.R      standalone diagnostic (not run by run_all.R)
manuscript/                    LaTeX source and PDF of the June 2026 version
raw/                           input data; see raw/README.md for sources and licences
clean/  output/  run_logs/     written by the pipeline (git-ignored)
```

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

## Citation

If you use this code or build on the analysis, please cite the paper (citation
to be added on publication) and attribute the data sources as set out in
`raw/README.md`.

## Licence

Code: MIT (see [`LICENSE`](LICENSE)). Data: see [`raw/README.md`](raw/README.md).
