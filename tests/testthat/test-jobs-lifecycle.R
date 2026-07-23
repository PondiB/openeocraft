# Build a fresh API backed by an isolated work_dir for each test.
new_test_api <- function(env = parent.frame()) {
    api <- mock_create_openeo_v1()
    api$work_dir <- withr::local_tempdir(.local_envir = env)
    api
}

expect_api_status <- function(expr, status) {
    err <- tryCatch(expr, error = function(e) e)
    expect_s3_class(err, "condition")
    expect_equal(err$status, status)
}

test_that("job_sync_id is a dated identifier", {
    expect_match(job_sync_id(), "^job-[0-9]{8}$")
})

test_that("atomic_save_rds writes atomically and round-trips", {
    dir <- withr::local_tempdir()
    f <- file.path(dir, "nested", "obj.rds")
    atomic_save_rds(list(a = 1), f)
    expect_true(file.exists(f))
    expect_equal(readRDS(f), list(a = 1))
    # No leftover temp files.
    expect_length(list.files(dirname(f), pattern = "^atomic_"), 0L)
})

test_that("job create/read/get/update-status round-trips via rds", {
    api <- new_test_api()
    job <- list(id = "j1", status = "created", title = "T", process = list())

    expect_equal(job_read_rds(api, "alice"), list())
    job_crt_rds(api, "alice", job)

    jobs <- job_read_rds(api, "alice")
    expect_named(jobs, "j1")
    expect_equal(job_get(api, "alice", "j1")$title, "T")

    updated <- job_upd_status(api, "alice", "j1", "running")
    expect_equal(updated$status, "running")
    expect_equal(job_get(api, "alice", "j1")$status, "running")

    # No-op when status unchanged.
    same <- job_upd_status(api, "alice", "j1", "running")
    expect_equal(same$status, "running")
})

test_that("job_get and job_upd_status error on unknown id", {
    api <- new_test_api()
    expect_api_status(job_get(api, "alice", "missing"), 500)
    expect_api_status(job_upd_status(api, "alice", "missing", "x"), 500)
})

test_that("job_info returns metadata or 404", {
    api <- new_test_api()
    job_crt_rds(api, "alice", list(id = "j1", status = "created"))
    expect_equal(job_info(api, "alice", "j1")$id, "j1")
    expect_api_status(job_info(api, "alice", "nope"), 404L)
})

test_that("job_new_dir and job_del_dir manage the job folder", {
    api <- new_test_api()
    job <- list(id = "j1")
    job_new_dir(api, "alice", job)
    dir <- job_get_dir(api, "alice", "j1")
    expect_true(dir.exists(dir))
    # Recreating wipes and recreates.
    writeLines("x", file.path(dir, "leftover.txt"))
    job_new_dir(api, "alice", job)
    expect_false(file.exists(file.path(dir, "leftover.txt")))
    job_del_dir(api, "alice", "j1")
    expect_false(dir.exists(dir))
})

test_that("log_append stores concise, single-line, id-tagged entries", {
    api <- new_test_api()
    job_new_dir(api, "alice", list(id = "j1"))

    long_msg <- paste(rep("x", 600), collapse = "")
    log_append(api, "alice", "j1", 100L, "error", "line1\nline2",
        data = list(call = "f()"))
    log_append(api, "alice", "j1", 0L, "info", long_msg)

    logs <- logs_read_rds(api, "alice", "j1")
    expect_length(logs, 2L)
    expect_equal(logs[[1]]$id, "j1-1")
    expect_equal(logs[[1]]$message, "line1") # newline truncation
    expect_equal(logs[[1]]$data$call, "f()")
    expect_equal(nchar(logs[[2]]$message), 500L) # 497 + "..."
    expect_match(logs[[2]]$message, "\\.\\.\\.$")
})

test_that("job_logs validates level and limit", {
    api <- new_test_api()
    job_new_dir(api, "alice", list(id = "j1"))
    expect_api_status(job_logs(api, "alice", "j1", level = "bogus"), 400L)
    expect_api_status(job_logs(api, "alice", "j1", limit = 0), 400L)
})

test_that("job_logs filters by level, applies offset and limit", {
    api <- new_test_api()
    job_new_dir(api, "alice", list(id = "j1"))
    logs <- list(
        list(id = "j1-1", level = "info", message = "a", time = Sys.time()),
        list(id = "j1-2", level = "debug", message = "b", time = Sys.time()),
        list(id = "j1-3", level = "error", message = "c", time = Sys.time()),
        list(id = "j1-4", level = "info", message = "d", time = Sys.time())
    )
    logs_save_rds(api, "alice", "j1", logs)

    # level=info excludes debug.
    res_info <- job_logs(api, "alice", "j1", level = "info")
    ids <- vapply(res_info$logs, function(x) x$id, character(1))
    expect_setequal(ids, c("j1-1", "j1-3", "j1-4"))

    # offset by id returns entries after it.
    res_off <- job_logs(api, "alice", "j1", offset = "j1-1", level = "info")
    ids_off <- vapply(res_off$logs, function(x) x$id, character(1))
    expect_setequal(ids_off, c("j1-3", "j1-4"))

    # numeric offset skips first N (of filtered set).
    res_num <- job_logs(api, "alice", "j1", offset = "2", level = "info")
    expect_length(res_num$logs, 1L)

    # limit truncates.
    res_lim <- job_logs(api, "alice", "j1", level = "info", limit = 1)
    expect_length(res_lim$logs, 1L)
})

test_that("job_update merges fields or 404s; job_estimate returns a message", {
    api <- new_test_api()
    job_crt_rds(api, "alice", list(
        id = "j1", status = "created", title = "old",
        description = "d", process = list(a = 1)
    ))
    out <- job_update(api, "alice", "j1", list(title = "new"))
    expect_equal(out$code, 200L)
    stored <- job_get(api, "alice", "j1")
    expect_equal(stored$title, "new")
    expect_equal(stored$status, "updated")

    expect_api_status(
        job_update(api, "alice", "nope", list(title = "x")), 404L
    )
    expect_match(job_estimate(api, "alice", "j1")$message, "cloud")
})

test_that("job_delete removes job and folder, 404 otherwise", {
    api <- new_test_api()
    job_crt_rds(api, "alice", list(id = "j1", status = "created"))
    job_new_dir(api, "alice", list(id = "j1"))
    job_delete(api, "alice", "j1")
    expect_false("j1" %in% names(job_read_rds(api, "alice")))
    expect_false(dir.exists(job_get_dir(api, "alice", "j1")))
    expect_api_status(job_delete(api, "alice", "nope"), 404L)
})

test_that("job_empty_collection reflects job status and identity", {
    col <- job_empty_collection(
        NULL, "alice",
        list(id = "j1", status = "running", title = "T", description = "D")
    )
    expect_equal(col$id, "j1")
    expect_equal(col$`openeo:status`, "running")
    expect_equal(col$type, "Collection")
    expect_equal(col$stac_version, "1.0.0")
})

test_that("job_check enforces process and fills defaults", {
    expect_api_status(job_check(NULL), 400L)
    expect_api_status(job_check(list(title = "x")), 400L) # no process
    expect_api_status(
        job_check(list(process = NULL), partial = TRUE), 400L
    )
    # partial returns input unchanged.
    expect_equal(
        job_check(list(title = "x"), partial = TRUE), list(title = "x")
    )
    normalized <- job_check(list(process = list(a = 1)))
    expect_equal(normalized$plan, "Free")
    expect_equal(normalized$budget, 0.0)
    expect_equal(normalized$log_level, "Info")
    expect_equal(normalized$links, list())
})

test_that("job_info_check requires the mandatory fields", {
    expect_silent(job_info_check(list(
        title = "t", description = "d", process = list()
    )))
    expect_api_status(job_info_check(list(title = "t")), 400L)
})

test_that("job_populate_links adds self and (when finished) results", {
    api <- new_test_api()
    api_attr(api, "api_base_url") <- "https://h"
    req <- mock_req("/jobs", method = "GET")

    created <- job_populate_links(
        list(id = "j1", status = "created"), api, req
    )
    rels <- vapply(created$links, function(x) x$rel, character(1))
    expect_true("self" %in% rels)
    expect_false("results" %in% rels)

    finished <- job_populate_links(
        list(id = "j1", status = "finished"), api, req
    )
    rels_f <- vapply(finished$links, function(x) x$rel, character(1))
    expect_true(all(c("self", "results") %in% rels_f))
})

test_that("job_get_results errors for missing job, job error and unfinished", {
    api <- new_test_api()
    # Missing job -> 404.
    expect_api_status(job_get_results(api, "alice", "nope"), 404L)

    # Job in error -> 424.
    job_crt_rds(api, "alice", list(id = "jerr", status = "error"))
    job_new_dir(api, "alice", list(id = "jerr"))
    expect_api_status(job_get_results(api, "alice", "jerr"), 424L)

    # Unfinished, non-partial -> 400 JobNotFinished.
    job_crt_rds(api, "alice", list(id = "jc", status = "created"))
    job_new_dir(api, "alice", list(id = "jc"))
    err <- tryCatch(job_get_results(api, "alice", "jc"), error = function(e) e)
    expect_equal(err$status, 400L)
    expect_equal(err$id, "JobNotFinished")

    # Unfinished but partial -> empty collection placeholder.
    partial <- job_get_results(api, "alice", "jc", partial = TRUE)
    expect_equal(partial$id, "jc")
    expect_equal(partial$type, "Collection")
})

test_that("job_get_results returns finished collection with links/expires", {
    api <- new_test_api()
    job_crt_rds(api, "alice", list(id = "jf", status = "finished"))
    job_new_dir(api, "alice", list(id = "jf"))
    dir <- job_get_dir(api, "alice", "jf")
    jsonlite::write_json(
        list(
            type = "Collection", id = "jf", stac_version = "1.0.0",
            extent = list(), links = list(),
            assets = list(data = list(href = "x.tif"))
        ),
        file.path(dir, "_collection.json"), auto_unbox = TRUE
    )
    req <- mock_req("/jobs", method = "GET")
    api_attr(api, "api_base_url") <- "https://h"

    col <- job_get_results(api, "alice", "jf", req = req)
    expect_equal(col$id, "jf")
    expect_true(!is.null(col$expires))
    # Asset without a title gets one derived from its key.
    expect_equal(col$assets$data$title, "data")
    rels <- vapply(col$links, function(x) x$rel, character(1))
    expect_true(all(c("self", "canonical") %in% rels))
})

test_that("job_get_results fills extent from rasters when available", {
    skip_if_not_installed("terra")
    api <- new_test_api()
    job_crt_rds(api, "alice", list(id = "jr", status = "finished"))
    job_new_dir(api, "alice", list(id = "jr"))
    dir <- job_get_dir(api, "alice", "jr")
    jsonlite::write_json(
        list(type = "Collection", id = "jr", extent = list()),
        file.path(dir, "_collection.json"), auto_unbox = TRUE
    )
    r <- terra::rast(
        nrows = 5, ncols = 5, xmin = 300000, xmax = 305000,
        ymin = 5800000, ymax = 5805000, crs = "EPSG:32633"
    )
    terra::values(r) <- seq_len(25)
    terra::writeRaster(r, file.path(dir, "out.tif"))

    col <- job_get_results(api, "alice", "jr")
    bbox <- col$extent$spatial$bbox[[1]]
    expect_length(bbox, 4L)
    expect_gt(bbox[[1]], 10)
    expect_lt(bbox[[1]], 20)
})

test_that("job_cancel_results clears artefacts but keeps logs and 404s", {
    api <- new_test_api()
    expect_api_status(job_cancel_results(api, "alice", "nope"), 404L)

    job_crt_rds(api, "alice", list(id = "jc", status = "running"))
    job_new_dir(api, "alice", list(id = "jc"))
    dir <- job_get_dir(api, "alice", "jc")
    writeLines("data", file.path(dir, "result.tif"))
    saveRDS(list(), file.path(dir, "logs.rds"))

    job_cancel_results(api, "alice", "jc")
    expect_false(file.exists(file.path(dir, "result.tif")))
    expect_true(file.exists(file.path(dir, "logs.rds")))
    expect_equal(job_get(api, "alice", "jc")$status, "canceled")
})

test_that("job_reconcile_bg_process handles missing and non-running jobs", {
    api <- new_test_api()
    expect_null(job_reconcile_bg_process(api, "alice", "nope"))

    job_crt_rds(api, "alice", list(id = "jf", status = "finished"))
    expect_equal(
        job_reconcile_bg_process(api, "alice", "jf")$status, "finished"
    )
})
