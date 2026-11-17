# zenoQC

<!-- badges: start -->
[![R-CMD-check](https://github.com/ricoderks/ZenoQC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricoderks/ZenoQC/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A `golem` Shiny app that tracks the status of the **ZenoTOF 7600+** from its
SCIEX *Positive/Negative Quick Status Check* reports (`.xps`).

Users upload the reports. The app extracts every value into a SQLite database
and shows interactive `plotly` trends per polarity.

## Run

```r
# from the project folder
golem::run_dev()            # development (dev/run_dev.R)
# or, installed:
zenoQC::run_app()
zenoQC::run_app(db_path = "/shared/zenoqc/zenoqc.sqlite")   # other database
```

`app.R` is included for Posit Connect / Shiny Server deployments.

## Database

SQLite file, by default `~/zenoqc/zenoqc.sqlite`. Change it in
`inst/golem-config.yml` (`db_path`), with the environment variable `ZENOQC_DB`,
or with `run_app(db_path = ...)`.

| table | content |
|---|---|
| `reports` | one row per report: date/time, polarity, serial number, overall result, file name |
| `sections` | per step: result, TOF calibration slope/delay (and previous values), number of spectra |
| `ion_metrics` | per step × calibrant ion: mass, resolution, sum intensity, ppm error with min/max/status |
| `channel_alignment` | channel 1/3/4 vs 2 slope & intercept (initial, actual, limits, status) |

A report is identified by serial number + polarity + acquisition time, so
uploading the same report twice (even renamed) stores it only once.

Bulk import a folder of reports:

```r
zenoQC::import_folder("Docs/xps")
```

## App pages

- **Overview**: latest positive/negative result, reports in database, pass/fail timeline per step, latest key values.
- **Positive mode / Negative mode**: mass accuracy (ppm, ±2 ppm band), resolution (with per-ion minimum), sum intensity (log) and Zeno on/off intensity ratio, TOF calibration slope & delay, detector channel alignment.
- **Upload**: select one or more `.xps` files, preview (new / already in database / error), save.
- **History**: all stored reports, full values of a selected report, CSV export, delete with confirmation.

## Development

```r
devtools::test()     # parser, database and plot tests (also parses all files in Docs/xps)
devtools::check()
```
