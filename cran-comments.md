## Test environments

* Local: R 4.5.2 on macOS (arm64)
* GitHub Actions: macOS, Windows, Ubuntu — R-release / devel / oldrel-1
  (`.github/workflows/R-CMD-check.yml`)

## R CMD check results

`R CMD check --as-cran` locally: **0 errors | 0 warnings | 2 notes**

Notes:

* **New submission** — expected for a first CRAN upload.
* **unable to verify current time** — transient clock/network note on the
  check host; not package-related.

## Dependency notes for CRAN

* **`openstac` is not listed in Suggests/Remotes.** It is not on CRAN.
  Docker/server deployments that need STAC collection routes install it from
  GitHub separately:
  `remotes::install_github("Open-Earth-Monitor/openstac", ref = "dev")`.
* Suggests **`sits`** and **`terra`** are optional for the ML process backend
  and raster extent inference; tests skip when they are unavailable.

## Downstream dependencies

None known on CRAN.
