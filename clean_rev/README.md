# Rents on new leases: `new_lease_rents.csv`

This folder holds the indices of rents on new leases that Online Appendix F
uses. `01b_new_lease_rents.R` builds them from the bond reports of four state
agencies, and `02f_new_lease_tests.R` reads them. Only `new_lease_rents.csv`
is in the repository, so Appendix F runs without the reports. The reports
themselves (about 180 MB) are not in the repository. The main README.md says
where to download them; running `01b` on them rebuilds this file, together
with `new_lease_cells.csv` and `new_lease_checks.txt`.

## Contents

One row per city and quarter: Sydney 1990Q1 to 2026Q1, Melbourne 1999Q2 to
2026Q2, Brisbane 2012Q1 to 2026Q3 and Adelaide 2008Q2 to 2026Q2. Perth has
no published bond medians.

| Column | Meaning |
|---|---|
| `city`, `quarter`, `year`, `qtr` | City and quarter |
| `dlog` | Quarterly change in log rents on new leases: the change in each cell's median rent of new bonds, weighted by the cell's share of new bonds over the whole sample. Missing when the cells with a change hold less than half of the weight. |
| `dlog_sa` | `dlog` less its mean for that quarter of the year, plus its overall mean |
| `index`, `index_sa` | Index levels that cumulate `dlog` and `dlog_sa`, 2011–12 = 100 |
| `cover` | Share of the weight held by the cells with a change in that quarter |
| `n_cells` | Number of cells with a change in that quarter |

The cells are bedrooms for Sydney, dwelling type by bedrooms for Melbourne
and Adelaide, and dwelling type by bedrooms by local government area for
Brisbane. The header of `01b_new_lease_rents.R` gives the details.

## Sources, licences and attribution

The indices are the authors' calculation from the medians and bond counts
that the agencies publish. They are not the agencies' data as published, and
the agencies do not endorse them. Each agency publishes its data under a
Creative Commons Attribution licence and disclaims responsibility for their
accuracy and for any use made of them.

| City | Source | Licence | Attribution |
|---|---|---|---|
| Sydney | NSW Department of Communities and Justice, [Rent and Sales Report](https://dcj.nsw.gov.au/about-us/families-and-communities-statistics/housing-rent-and-sales/rent-and-sales-report.html): rent tables for June 2017 to March 2026, and the time series "Rents, Trend March 1990 - June 2017, Metropolitan LGAs" | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) | © State of New South Wales (Department of Communities and Justice). [Copyright and disclaimer](https://dcj.nsw.gov.au/statements/copyright-and-disclaimer.html) |
| Melbourne | Homes Victoria, [Rental Report - Quarterly: Quarterly Median Rents by LGA](https://discover.data.vic.gov.au/dataset/rental-report-quarterly-quarterly-median-rents-by-lga) (DataVic) | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) | Homes Victoria |
| Brisbane | Queensland Residential Tenancies Authority, [median rents quarterly data](https://www.rta.qld.gov.au/forms-resources/rta-quarterly-data/median-rents-quarterly-data) | [CC BY 3.0 AU](https://creativecommons.org/licenses/by/3.0/au/) | © The State of Queensland Residential Tenancies Authority 2011. [Copyright](https://www.rta.qld.gov.au/copyright) and [disclaimer](https://www.rta.qld.gov.au/disclaimer) |
| Adelaide | South Australian Housing Trust, [Private Rent Report](https://data.sa.gov.au/data/dataset/private-rent-report) (data.sa.gov.au) | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) | South Australian Housing Trust |

The attribution column uses the wording that DCJ and the RTA ask for on their
copyright pages. The repository's MIT licence covers the code, not this file.
