test_that("configure_openeocraft_runtime sets unset thread-limit vars to 1", {
    # Isolate env changes to this test.
    withr::local_envvar(c(
        OMP_NUM_THREADS = NA,
        MKL_NUM_THREADS = NA,
        OPENBLAS_NUM_THREADS = NA,
        TORCH_NUM_THREADS = NA
    ))

    expect_invisible(configure_openeocraft_runtime())

    # Outside Docker (no /.dockerenv), the function only sets thread limits.
    skip_if(file.exists("/.dockerenv"))
    expect_equal(Sys.getenv("OMP_NUM_THREADS"), "1")
    expect_equal(Sys.getenv("MKL_NUM_THREADS"), "1")
    expect_equal(Sys.getenv("OPENBLAS_NUM_THREADS"), "1")
    expect_equal(Sys.getenv("TORCH_NUM_THREADS"), "1")
})

test_that("configure_openeocraft_runtime preserves already-set thread vars", {
    withr::local_envvar(c(OMP_NUM_THREADS = "8"))
    configure_openeocraft_runtime()
    expect_equal(Sys.getenv("OMP_NUM_THREADS"), "8")
})
