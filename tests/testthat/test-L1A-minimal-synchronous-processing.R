# Mock API object
api <- mock_create_openeo_v1()
token <- readLines(file(system.file("mock/token", package = "openeocraft")))
mock_job <- jsonlite::read_json(
    system.file("mock/mock-job.json", package = "openeocraft")
)
mock_badjob <- jsonlite::read_json(
    system.file("mock/mock-badjob.json", package = "openeocraft")
)

test_that("POST /result is supported (with authentication)", {
    req <- mock_req("/result", method = "POST")
    res <- mock_res()
    expect_error(api_result(api, req, res), "Token is missing")

    req <- mock_req("/result", HTTP_AUTHORIZATION = "Bearer bad", method = "POST")
    res <- mock_res()
    err <- tryCatch(
        api_result(api, req, res),
        error = function(e) e
    )
    expect_equal(err$status, 403L)

    # Valid auth still requires a process graph body
    req <- mock_req("/result", HTTP_AUTHORIZATION = token, method = "POST")
    res <- mock_res()
    expect_error(api_result(api, req, res), "Missing process graph")
})

test_that("POST /result > process: Only accepts valid process submissions", {
    req <- mock_req("/result", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_badjob
    res <- mock_res()
    expect_error(api_result(api, req, res), "Invalid process graph")
})

test_that("POST /result rejects non-free plans without billing", {
    req <- mock_req("/result", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job$process
    req$body$plan <- "enterprise"
    # plan on wrapper: pass as list with process + plan
    req$body <- list(process = mock_job$process, plan = "enterprise")
    res <- mock_res()
    err <- tryCatch(api_result(api, req, res), error = function(e) e)
    expect_equal(err$status, 402L)
})

test_that("data_serializer sets suitable Content-Type headers", {
    tmp <- tempfile(fileext = ".tif")
    writeBin(as.raw(1:10), tmp)
    on.exit(unlink(tmp), add = TRUE)
    res <- mock_res()
    data_serializer(structure(list(data = tmp), class = "openeo_gtiff"), res)
    expect_equal(res$getHeader("Content-Type"), "image/tiff")

    tmp2 <- tempfile(fileext = ".nc")
    writeBin(as.raw(1:10), tmp2)
    on.exit(unlink(tmp2), add = TRUE)
    res2 <- mock_res()
    data_serializer(structure(list(data = tmp2), class = "openeo_netcdf"), res2)
    expect_equal(res2$getHeader("Content-Type"), "application/netcdf")
})
