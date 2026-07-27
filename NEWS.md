# NEWS.md

## openeocraft 0.3.1

* openEO L1/L2 compliance hardening: auth status codes, sync `/result`,
  job logs/results (`partial`, `DELETE`), `/me`, billing stub, OIDC discovery
  stub, pagination on `/jobs` and `/processes`.
* User-defined processes: `/process_graphs` CRUD, `namespace` / UDP-by-`process_id`,
  and `from_parameter` evaluation.
* Infer finished-job STAC extents from raster outputs when metadata is missing.
* CRAN preparation: drop non-CRAN `Remotes`/`openstac` Suggests (soft dependency),
  package topic, NEWS, contribution docs, and quieter `.onLoad`.

## openeocraft 0.3.0

* Initial public openEO craft backend with Plumber routes, process graphs,
  batch jobs, and sits-backed ML processes.
