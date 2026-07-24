.onLoad <- function(libname, pkgname) {
    # Intentionally empty: do not mutate the user environment on attach/load.
    # Worker and server entrypoints call configure_openeocraft_runtime()
    # explicitly (see R/runtime-config.R and docker/plumber.R).
    invisible(NULL)
}
