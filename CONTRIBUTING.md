# Contributing to openeocraft

Thanks for contributing. By participating you agree to follow our
[Code of Conduct](CODE_OF_CONDUCT.md).

## Development setup

1. Clone the repository and open it in R / RStudio / Cursor.
2. Install package dependencies (CRAN Suggests as needed):

   ```r
   install.packages(c("devtools", "testthat", "withr", "terra"))
   # Optional STAC helpers (not on CRAN yet):
   # remotes::install_github("Open-Earth-Monitor/openstac", ref = "dev")
   ```

3. Load for interactive work:

   ```r
   devtools::load_all()
   ```

## Workflow

* Prefer small, focused pull requests against `dev` / `clean-up`.
* Add or update `tests/testthat` for behavior changes.
* Run `devtools::test()` locally before opening a PR.
* For documentation: `devtools::document()`.
* For a CRAN-like gate: `devtools::check(args = "--as-cran")`.

## Style

* Match existing R style in `R/` (snake_case, roxygen2 markdown).
* Do not commit secrets, credentials, or large binary fixtures outside `inst/`.
* Keep examples free of network calls and long downloads (`\dontrun` /
  `skip_on_cran()` when needed).

## Reporting issues

Use GitHub Issues with a minimal reproducible example when possible.
