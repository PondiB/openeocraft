test_that("from_parameter is recognized by arg_type", {
    x <- list(from_parameter = "collection_id")
    expect_equal(arg_type(x), "parameter_reference")
})

test_that("shipped process JSON files have summary and categories", {
    proc_dir <- system.file("ml/processes", package = "openeocraft")
    files <- list.files(proc_dir, pattern = "\\.json$", full.names = TRUE)
    expect_gt(length(files), 0L)
    for (f in files) {
        p <- jsonlite::read_json(f)
        expect_true(
            is.character(p$summary) && nzchar(p$summary),
            info = basename(f)
        )
        expect_true(
            is.list(p$categories) && length(p$categories) > 0,
            info = basename(f)
        )
    }
})
