# Raw data: sources, series, and licences

Every file the pipeline reads from `raw/` is listed below with its source, the
exact dataset/series identifiers, and its licence. All three providers permit
reproduction **with attribution**, so the files are committed to this repository
under their respective terms. They are *not* covered by the repository's MIT code
licence — each file retains the licence of its source.

Two steps read `raw/`: `01_build_panel.R` reads every file, and
`01a_build_test_panel.R` reads the investor-lending file again for its
seasonally adjusted series. The three modern-API filenames are centralised in
`00_setup.R` as `F_LEND`, `F_OMAD`, `F_D2`, since the long DSD-encoded names
are the volatile part on a data refresh.

## Attribution (required if you reproduce these data)

- **ABS data** — Source: Australian Bureau of Statistics. Licensed under
  [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)
  (CC BY 4.0). See the [ABS copyright page](https://www.abs.gov.au/) and
  [Attributing ABS material](https://www.abs.gov.au/websitedbs/d3310114.nsf/Home/Attributing+ABS+Material).
- **RBA data** — Source: Reserve Bank of Australia. The D2 housing-credit table is
  RBA *Financial Data* under Section 5 of the
  [RBA Copyright and Disclaimer Notice](https://www.rba.gov.au/copyright/),
  which permits reproduction and publication with attribution (`Source: RBA`),
  provided no RBA endorsement is implied. Only RBA-sourced credit aggregates are
  used here (no third-party-sourced series).
- **BIS data** — Source: Bank for International Settlements. Used under the
  [BIS Terms of permitted use of BIS statistics](https://data.bis.org/help/legal):
  use is unrestricted provided the BIS is cited as the source, the use is not
  misleading and implies no BIS endorsement, and — for any commercial
  publication — inclusion results in no additional charge to users (this is a
  free, openly accessible academic repository, so that condition is met).

## File inventory

| File (in `raw/`) | Source | Dataset / series | Licence / terms |
|------------------|--------|------------------|-----------------|
| `641601.xlsx` | ABS | 6416.0 Residential Property Price Indexes, per-city series (IDs in `IDMAP_6416`, `00_setup.R`) | CC BY 4.0 |
| `ABS_RES_DWELL_ST_1_0_0__1_2_3_4_5_Q.csv` | ABS | Total Value of Dwellings — mean price of residential dwellings, by state | CC BY 4.0 |
| `310102.xlsx` | ABS | 3101.0 National, State and Territory Population — net overseas & net interstate migration | CC BY 4.0 |
| `87520039.xlsx` | ABS | 8752.0 Building Activity, Table 39 — dwelling completions (total/private/public) | CC BY 4.0 |
| `6401010.xlsx` | ABS | 6401.0 Consumer Price Index — rents, per-city | CC BY 4.0 |
| `634502b.xlsx` | ABS | 6345.0 Wage Price Index | CC BY 4.0 |
| `62020010.xlsx` | ABS | 6202.0 Labour Force — employment, by state | CC BY 4.0 |
| `ABS_LEND_HOUSING_1_1_..._AUS_Q.csv` (`F_LEND`) | ABS | Lending Indicators `LEND_HOUSING` 1.1 — new housing loan commitments by state, investor and owner-occupier, original and seasonally adjusted (5601.0 successor) | CC BY 4.0 |
| `ABS_OMAD_VISA_..._AUS_Q.csv` (`F_OMAD`) | ABS | Overseas Migration `OMAD_VISA` — NOM by visa-and-citizenship group, by state | CC BY 4.0 |
| `ABS_ANA_SFD_1_0_0_C_FCE_RDSH__..._Q.csv` | ABS | State Final Demand — household final consumption, housing services | CC BY 4.0 |
| `ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_..._Q.csv` | ABS | State Final Demand — gross fixed capital formation (Private / Public) | CC BY 4.0 |
| `d02hist.xlsx` (`F_D2`) | RBA | Statistical Table D2 — Lending and Credit Aggregates (investor `DLCACIHN`, owner-occupier `DLCACOHN`, net switching `DLCANS`) | RBA Financial Data terms |
| `bis_dp_search_export_20260615-045639.csv` | BIS | Residential Property Prices — Australia (Sydney series), BIS Data Portal export | BIS terms of permitted use |
| `panel_main.csv` | own construction | base paper panel into which reconstructed series are swapped | this repository (MIT) |
| `f01hist.xlsx` (`F_F11`), optional | RBA | Statistical Table F1.1 — Interest Rates and Yields, Money Market, Monthly (cash rate target, monthly average, `FIRMMCRT`) | RBA Financial Data terms |

`f01hist.xlsx` is optional. When it is present, `01_build_panel.R` takes every
quarter's cash rate from it: the mean of the three monthly averages of the cash
rate target, each rounded to two decimals as the table shows them. Built this
way, it reproduces the cash rate in `panel_main.csv` in all 73 quarters from
2006 Q3 to 2024 Q3. Without it, as for the June 2026 inputs, the history comes
from `panel_main.csv` and 2024 Q4 to 2025 Q4 from a hand-typed path, so the
June 2026 results reproduce. That path holds end-of-quarter targets for 2025
Q1–Q3, where F1.1 gives quarterly averages; the difference moves no estimate
in `02b` or `02c` by more than 0.001.

## Files deliberately NOT committed

Three files may appear in a working `raw/` folder but are **not pipeline inputs**
and are excluded from the repository. They are listed here so a replicator does
not mistake their absence for a missing dependency:

- `ABS_OMAD_VISA,+..1+2+3+4+5+6+AUS.Q.csv` — an earlier/alternative OMAD export
  (all visa groups). Superseded by the specific-group export `F_OMAD`
  (`ABS_OMAD_VISA___11_12_..._AUS_Q.csv`), which is the only OMAD file the code
  reads. Not used.
- `f01dhist.xls` — RBA Statistical Table F1, the daily interest-rate table.
  Referenced by no script; the pipeline reads the monthly table F1.1
  (`f01hist.xlsx`) instead.
- `560114.xlsx` — legacy ABS 5601.0 Table 14 investor lending. Superseded by the
  Lending Indicators successor `F_LEND`; appears only in an explanatory comment
  in `01_build_panel.R`, never read.

## ABS filename normalisation (important)

The ABS Data Explorer exports CSVs with punctuation in the filename, e.g.
`ABS,LEND_HOUSING,1.1+...20.1+2+3+4+5+6+7+8+AUS.Q.csv`. The pipeline expects the
punctuation replaced by underscores (the canonical names in the inventory above
and in `00_setup.R`). After downloading, **rename each ABS CSV to its canonical
name by matching the dataflow ID below**, not by guessing — the filename encodes
the dataflow *and the selected dimension codes*, so a wrong name can also signal
the wrong column selection.

| Dataflow (ABS Data Explorer) | Canonical filename in `raw/` |
|------------------------------|------------------------------|
| `LEND_HOUSING` 1.1 (investor housing commitments, by state) | `ABS_LEND_HOUSING_1_1_______10_20_1_2_3_4_5_6_7_8_AUS_Q.csv` |
| `OMAD_VISA` (NOM by visa-and-citizenship group, by state) | `ABS_OMAD_VISA___11_12_15_22_23_24_25_1009_1010_1020_1030_1040_1041_2203_2208_01_02_03_1_2_3_4_5_AUS_Q.csv` |
| `RES_DWELL_ST` 1.0.0 (mean dwelling price, by state) | `ABS_RES_DWELL_ST_1_0_0__1_2_3_4_5_Q.csv` |
| `ANA_SFD` 1.0.0 — FCE_RDSH (housing services consumption) | `ABS_ANA_SFD_1_0_0_C_FCE_RDSH__20_2_3_4_5_6_1_Q.csv` |
| `ANA_SFD` 1.0.0 — GFC_PSS_GSS (GFCF, Private/Public) | `ABS_ANA_SFD_1_0_0_C_GFC_PSS_GSS_20_2_3_4_5_6_1_Q.csv` |

Rule of thumb: take the ABS download name, drop the leading `ABS,`, and replace
every `,` `.` `+` (and any spaces) with `_`, preserving underscore runs. **Verify
the result against the table** — if it differs by more than punctuation (e.g. a
missing or extra dimension-code segment, such as a `10` present in the canonical
name but absent from your download), you have re-exported a *different* selection
and should re-export with the dimensions shown rather than force-rename. The
numbered `.xlsx` workbooks, `d02hist.xlsx`, the BIS export, and `panel_main.csv`
already have clean names and are not renamed.

## Re-downloading / refreshing

- **ABS** — via the ABS Data Explorer / data API (the `ABS_*` CSVs) or Time
  Series workbooks (the numbered `.xlsx`). Dataflow IDs are embedded in the
  filenames; `00_setup.R` documents the per-city column maps used by the readers.
- **RBA D2 and F1.1** — download the current D2 workbook and table F1.1
  (`f01hist.xlsx`) from the
  [RBA statistical tables](https://www.rba.gov.au/statistics/tables/); the series
  IDs above are stable across releases. F1.1 must hold all three months of every
  quarter to `TARGET_END`, or step 01 stops and says which quarter is short.
- **BIS** — re-export the Australia residential property price series from the
  [BIS Data Portal](https://data.bis.org/); the committed file is a dated search
  export (15 June 2026). The `00_setup.R` filename constant isolates this path,
  so a fresh export can be dropped in and the pipeline rerun.

On any refresh: edit `TARGET_END` and, if filenames changed, `F_LEND`/`F_OMAD`/
`F_D2` in `00_setup.R`; put the latest `f01hist.xlsx` in the raw-data folder, so
the cash rate needs no hand-typing; then `Rscript run_all.R`. See "Refreshing
data next quarter" in `STRUCTURE.md`.

## Provisional values

- **Net migration for 2025 Q4** is not yet published, so the panel leaves it
  missing. Net migration and tightness end in 2025 Q3, one quarter before the
  other series.
- **OMAD visa shares** end one quarter before the panel and are `NA` across the
  2020–21 border-closure window by construction.
