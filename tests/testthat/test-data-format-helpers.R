test_that("format_ext maps format identifiers to extensions", {
    expect_equal(format_ext("gtiff"), ".tif")
    expect_equal(format_ext("netcdf"), ".nc")
    expect_equal(format_ext("rds"), ".rds")
    expect_equal(format_ext("json"), ".json")
    expect_null(format_ext("unknown"))
})

test_that("ext_format maps file names to format identifiers", {
    expect_equal(ext_format("result.tif"), "gtiff")
    expect_equal(ext_format("cube.nc"), "netcdf")
    expect_equal(ext_format("model.rds"), "rds")
    expect_equal(ext_format("meta.json"), "json")
    expect_null(ext_format("archive.zip"))
})

test_that("ext_content_type maps file names to MIME types", {
    expect_equal(ext_content_type("result.tif"), "image/tiff")
    expect_equal(ext_content_type("cube.nc"), "application/netcdf")
    expect_equal(ext_content_type("model.rds"), "application/rds")
    expect_equal(ext_content_type("meta.json"), "application/json")
    # Nested paths and multiple dots resolve to the final extension.
    expect_equal(ext_content_type("/tmp/a.b/result.tif"), "image/tiff")
})

test_that("format_content_type is case-insensitive", {
    expect_equal(format_content_type("GTIFF"), "image/tiff")
    expect_equal(format_content_type("NetCDF"), "application/netcdf")
    expect_equal(format_content_type("rds"), "application/rds")
    expect_equal(format_content_type("json"), "application/json")
})

test_that("data_serializer sets the correct Content-Type and body bytes", {
    tmp <- withr::local_tempfile()
    payload <- as.raw(c(1, 2, 3, 4, 5))
    writeBin(payload, tmp)

    cases <- list(
        list(class = "openeo_json", type = "application/json"),
        list(class = "openeo_gtiff", type = "image/tiff"),
        list(class = "openeo_netcdf", type = "application/netcdf"),
        list(class = "openeo_rds", type = "application/rds"),
        list(class = "openeo_tar", type = "application/x-tar")
    )

    for (case in cases) {
        res <- mock_res()
        x <- structure(list(data = tmp), class = case$class)
        out <- data_serializer(x, res)
        expect_equal(res$getHeader("Content-Type"), case$type)
        expect_equal(out$getHeader("Content-Type"), case$type)
        expect_equal(res$body, payload)
    }
})
