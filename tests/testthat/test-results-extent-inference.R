test_that("infer_results_extent derives a WGS84 bbox from raster outputs", {
    skip_if_not_installed("terra")

    job_dir <- withr::local_tempdir()

    # UTM zone 33N raster (a small footprint over central Europe).
    r <- terra::rast(
        nrows = 10, ncols = 10,
        xmin = 300000, xmax = 310000,
        ymin = 5800000, ymax = 5810000,
        crs = "EPSG:32633"
    )
    terra::values(r) <- seq_len(100)
    terra::writeRaster(r, file.path(job_dir, "band1.tif"))
    # Sidecar files (leading underscore) must be ignored.
    terra::writeRaster(r, file.path(job_dir, "_ignore.tif"))

    local_mocked_bindings(job_get_dir = function(api, user, job_id) job_dir)

    ext <- infer_results_extent(
        structure(list(), class = "openeo_v1"), "user", "job"
    )

    expect_type(ext, "list")
    bbox <- ext$spatial$bbox[[1]]
    expect_length(bbox, 4L)
    # Reprojected UTM 33N footprint sits around ~12E / ~52N.
    expect_gt(bbox[[1]], 10)
    expect_lt(bbox[[1]], 20)
    expect_gt(bbox[[2]], 45)
    expect_lt(bbox[[2]], 60)
    expect_gte(bbox[[3]], bbox[[1]])
    expect_gte(bbox[[4]], bbox[[2]])
    # Temporal cannot be inferred from a footprint: left open.
    expect_equal(ext$temporal$interval, list(list(NULL, NULL)))
})

test_that("infer_results_extent returns NULL when there are no raster files", {
    job_dir <- withr::local_tempdir()
    writeLines("{}", file.path(job_dir, "_collection.json"))

    local_mocked_bindings(job_get_dir = function(api, user, job_id) job_dir)

    ext <- infer_results_extent(
        structure(list(), class = "openeo_v1"), "user", "job"
    )
    expect_null(ext)
})
