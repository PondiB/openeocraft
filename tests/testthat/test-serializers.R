test_that(".serializer_read_file returns raw bytes for an existing file", {
    tmp <- withr::local_tempfile()
    payload <- as.raw(c(10, 20, 30))
    writeBin(payload, tmp)
    expect_equal(.serializer_read_file(tmp), payload)
    # Accepts a length-1 vector (uses first element).
    expect_equal(.serializer_read_file(c(tmp)), payload)
})

test_that(".serializer_read_file errors on missing or invalid paths", {
    expect_error(
        .serializer_read_file(tempfile()),
        "Result file not found"
    )
    expect_error(.serializer_read_file(""), "Result file not found")
})

test_that("get_serializer dispatches to a plumber serializer for each format", {
    formats <- c(
        "openeo_json", "openeo_gtiff", "openeo_netcdf",
        "openeo_tar", "openeo_rds"
    )
    for (fmt in formats) {
        obj <- structure(list(), class = fmt)
        ser <- get_serializer(obj)
        # plumber serializers are functions.
        expect_true(is.function(ser))
    }
})

test_that("plumb_reg_serializers runs and registers without error", {
    # Registration side effect lives in plumber's internal registry; we only
    # assert our wrapper executes without error (plumber may emit an
    # informational message when re-registering).
    expect_no_error(suppressMessages(plumb_reg_serializers()))
})
