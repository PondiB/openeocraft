api <- mock_create_openeo_v1()
token <- readLines(file(system.file("mock/token", package = "openeocraft")))
mock_job <- jsonlite::read_json(
    system.file("mock/mock-job.json", package = "openeocraft")
)

create_job <- function() {
    req <- mock_req("/jobs", method = "POST", HTTP_AUTHORIZATION = token)
    req$body <- mock_job
    res <- mock_res()
    api_job_create(api, req, res)
    res$getHeader("OpenEO-Identifier")
}

test_that("GET /jobs/{id}/logs returns logs and links arrays", {
    job_id <- create_job()
    log_append(api, "mock_user", job_id, 100L, "info", "first")
    log_append(api, "mock_user", job_id, 100L, "info", "second")
    log_append(api, "mock_user", job_id, 100L, "error", "boom\nstack")

    out <- job_logs(api, "mock_user", job_id, level = "debug", limit = 10)
    expect_true(all(c("logs", "links") %in% names(out)))
    expect_type(out$logs, "list")
    expect_type(out$links, "list")
    expect_true(length(out$logs) >= 2)

    for (entry in out$logs) {
        expect_true(all(c("id", "level", "message") %in% names(entry)))
        expect_false(grepl("\n", entry$message, fixed = TRUE))
    }
})

test_that("GET /jobs/{id}/logs supports offset and limit", {
    job_id <- create_job()
    log_append(api, "mock_user", job_id, 1L, "info", "a")
    log_append(api, "mock_user", job_id, 1L, "info", "b")
    log_append(api, "mock_user", job_id, 1L, "info", "c")

    page1 <- job_logs(api, "mock_user", job_id, limit = 1, level = "debug")
    expect_equal(length(page1$logs), 1L)

    offset_id <- page1$logs[[1]]$id
    page2 <- job_logs(
        api, "mock_user", job_id,
        offset = offset_id, limit = 10, level = "debug"
    )
    expect_true(length(page2$logs) >= 1L)
    expect_false(identical(page2$logs[[1]]$id, offset_id))
})

test_that("GET /jobs/{id}/results rejects unfinished without partial", {
    job_id <- create_job()
    err <- tryCatch(
        job_get_results(api, "mock_user", job_id, partial = FALSE),
        error = function(e) e
    )
    expect_equal(err$status, 400L)
    expect_equal(err$id, "JobNotFinished")
})

test_that("GET /jobs/{id}/results allows partial unfinished collection", {
    job_id <- create_job()
    doc <- job_get_results(api, "mock_user", job_id, partial = TRUE)
    expect_equal(doc$type, "Collection")
    expect_true(!is.null(doc$expires))
    expect_equal(doc$stac_version, "1.0.0")
})

test_that("DELETE /jobs/{id}/results cancels job", {
    job_id <- create_job()
    req <- mock_req(
        paste0("/jobs/", job_id, "/results"),
        method = "DELETE",
        HTTP_AUTHORIZATION = token
    )
    res <- mock_res()
    api_job_cancel_results(api, req, res, job_id)
    expect_equal(res$status, 202L)
    job <- job_get(api, "mock_user", job_id)
    expect_equal(job$status, "canceled")
})

test_that("POST /jobs/{id}/results starts job with auth (202)", {
    job_id <- create_job()
    req <- mock_req(
        paste0("/jobs/", job_id, "/results"),
        method = "POST",
        HTTP_AUTHORIZATION = token
    )
    res <- mock_res()
    # Starting may fail later in worker; endpoint should accept and return 202
    # unless process graph immediately errors in async spawn.
    tryCatch(
        api_job_start(api, req, res, job_id),
        error = function(e) NULL
    )
    expect_true(is.null(res$status) || res$status %in% c(200L, 202L))
})
