test_that("get_serializer.openeo_gtiff returns a plumber serializer", {
    ser <- get_serializer.openeo_gtiff(structure(list(), class = "openeo_gtiff"))
    expect_type(ser, "closure")
    expect_true(all(c("val", "req", "res") %in% names(formals(ser))))
})

test_that("job_check requires process and fills defaults", {
    expect_error(job_check(list()), "process")
    expect_error(job_check(NULL), "Missing")

    out <- job_check(list(process = list(process_id = "load_collection")))
    expect_equal(out$plan, "Free")
    expect_equal(out$log_level, "Info")
    expect_equal(out$budget, 0)
    expect_type(out$links, "list")

    partial <- job_check(list(title = "t"), partial = TRUE)
    expect_equal(partial$title, "t")
})

test_that("GET /jobs populates self link", {
    api <- mock_create_openeo_v1()
    token <- readLines(file(system.file("mock/token", package = "openeocraft")))
    req <- mock_req("/jobs", method = "GET", HTTP_AUTHORIZATION = token)
    res <- mock_res()
    result <- api_jobs_list(api, req, res)
    expect_true(length(result$links) >= 1L)
    rels <- vapply(result$links, `[[`, character(1), "rel")
    expect_true("self" %in% rels)
})

test_that("atomic_save_rds round-trips", {
    tmpdir <- tempfile("atomic-")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
    path <- file.path(tmpdir, "obj.rds")
    atomic_save_rds(list(a = 1L), path)
    expect_true(file.exists(path))
    expect_equal(readRDS(path)$a, 1L)
})
