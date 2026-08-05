# PEP County Characteristics — reference for school-age (5–17) population

Population Estimates Program (PEP) county files. Same program as the total
population series already in `wv_2011_2024.csv`, so pulling school-age from here
avoids mixing PEP with the ACS 5-year rolling average.

WV state FIPS = **54**. Files are one per state; the `-54` suffix is West Virginia.

---

## Current vintage: 2025 (covers 2020–2025, released June 2026)

Landing page:
<https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-detail.html>

| Dataset | What it gives | WV direct link |
|---|---|---|
| **CC-EST2025-AGESEX** | Selected age groups + sex. Has `AGE513_TOT` (5–13) and `AGE1417_TOT` (14–17) — sum for 5–17. Smallest file, easiest option. | <https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/counties/asrh/cc-est2025-agesex-54.csv> |
| **CC-EST2025-SYASEX** | **Single year of age** + sex. Filter age 5–17 directly, no band arithmetic. Most precise. | <https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/counties/asrh/cc-est2025-syasex-54.csv> |
| **CC-EST2025-ALLDATA** | Age × sex × race × Hispanic origin. Largest; only needed for demographic breakdowns. | <https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/counties/asrh/cc-est2025-alldata-54.csv> |

File layouts (column definitions — read before parsing):

- AGESEX: <https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2025/CC-EST2025-AGESEX.pdf>
- SYASEX: <https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2025/CC-EST2025-SYASEX.pdf>
- ALLDATA: <https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2025/CC-EST2025-ALLDATA.pdf>

Methodology statement:
<https://www2.census.gov/programs-surveys/popest/technical-documentation/methodology/2020-2025/methods-statement-v2025.pdf>

---

## Prior decade: Vintage 2019 (covers 2010–2019)

Needed to reach back to 2011. Landing page:
<https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html>

Datasets confirmed on that page: **CC-EST2019-AGESEX** and **CC-EST2019-ALLDATA**
(no SYASEX for this decade). Direct links follow the same convention — verify in
a browser before scripting, since I could not confirm these two by fetch:

- `https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-54.csv`
- `https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata-54.csv`

Layout PDFs live under
<https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/>

---

## Browsing the whole archive

- All PEP datasets: <https://www2.census.gov/programs-surveys/popest/datasets/>
- All file layouts + methodology: <https://www2.census.gov/programs-surveys/popest/technical-documentation/>
- Searchable index of every file on the FTP site: <https://www2.census.gov/programs-surveys/popest/FTP2_Key.xlsx>

---

## Gotchas

**`YEAR` is a code, not a calendar year — and the key DIFFERS BY VINTAGE.**
Both keys below are confirmed from the official layout PDFs.

Vintage 2025 (`CC-EST2025-AGESEX`) — one April row:

| YEAR | Meaning |
|---|---|
| 1 | 4/1/2020 estimates base |
| 2 | 7/1/2020 |
| 3 | 7/1/2021 |
| 4 | 7/1/2022 |
| 5 | 7/1/2023 |
| 6 | 7/1/2024 |
| 7 | 7/1/2025 |

Vintage 2019 (`CC-EST2019-AGESEX`) — **two** April rows, so July starts at 3:

| YEAR | Meaning |
|---|---|
| 1 | 4/1/2010 Census population |
| 2 | 4/1/2010 estimates base |
| 3 | 7/1/2010 |
| 4 | 7/1/2011 |
| … | … |
| 12 | 7/1/2019 |

Because of that extra row, a decoding formula written for one vintage is off by
one on the other. Applying Vintage 2025's `base + YEAR - 2` to the 2019 file
produces 2010–2020 — wrong, and a year longer than the file covers.

**Safest approach: anchor from the last code, not the first.** `max(YEAR)` is
always the final July estimate, so `year = last_july_year - (max(YEAR) - YEAR)`
is correct for both vintages regardless of how many April rows precede. April
rows decode below the requested range and filter out on their own. This is what
`decode_pep()` in `census_data.qmd` does.

**Filter `SUMLEV == 50` and `COUNTY != 0`** to exclude any state roll-up row.

**5–17 is exact.** `AGE513_TOT` is ages 5–13 and `AGE1417_TOT` is 14–17 —
contiguous, so the sum has no gap and no double count. Both columns appear in
both vintages under the same names. (`AGEGRP`-banded ALLDATA files cannot do
this: their bands are 0-4 / 5-9 / 10-14 / 15-19, and 15-19 would need splitting.)

**Vintages supersede each other.** Every new release revises the entire series
back to the last census, so 2020–2025 figures from Vintage 2025 will not match
Vintage 2024. Don't mix vintages within one series.

**The decade boundary is a real seam.** Vintage 2019 is benchmarked to the 2010
census, Vintage 2025 to 2020. Values around 2019/2020 will not line up cleanly —
the same seam `census_data.qmd` already handles for total population.

---

## Alternative: SAIPE

Publishes "Population ages 5 to 17" directly, annually, for counties **and
school districts** — the official definition used for Title I allocation, and a
better geographic match for a district-level dashboard.

- Program: <https://www.census.gov/programs-surveys/saipe.html>
- School district methodology: <https://www.census.gov/programs-surveys/saipe/technical-documentation/methodology/school-districts/overview-school-district.html>
- API (not in tidycensus; use `censusapi::getCensus(name = "timeseries/poverty/saipe")`):
  <https://www.census.gov/data/developers/data-sets/Poverty-Statistics.html>
