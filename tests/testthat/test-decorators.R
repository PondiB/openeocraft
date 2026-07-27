test_that("run_decorator raises a located error on unparseable source", {
    src <- c("#* @openeo-process", "this is (((not valid R")
    expect_error(
        run_decorator(NULL, src, file = "/tmp/procs.R", line = 1),
        "Cannot parse code"
    )
})

test_that("openeo-process scaffolds JSON from formals when missing", {
    api <- mock_create_openeo_v1()
    dir <- withr::local_tempdir()
    file <- file.path(dir, "processes.R")

    expr <- parse(text = "myproc <- function(a, b = 2) { a + b }")[[1]]
    `openeo-process`(api, list(expr), file = file)

    json_path <- file.path(dir, "processes", "myproc.json")
    expect_true(file.exists(json_path))

    process <- jsonlite::read_json(json_path)
    expect_equal(process$id, "myproc")
    expect_equal(process$`_generated_by`, "openeocraft")

    params <- process$parameters
    names_seen <- vapply(params, function(p) p$name, character(1))
    expect_setequal(names_seen, c("a", "b"))
    # `a` is required (no default); `b` is optional with default 2.
    a_par <- params[[which(names_seen == "a")]]
    b_par <- params[[which(names_seen == "b")]]
    expect_false(a_par$optional)
    expect_true(b_par$optional)
    expect_equal(b_par$default, 2)

    # Registered on the API process list.
    expect_true("myproc" %in% names(api_attr(api, "processes")))
})

test_that("openeo-process never overwrites a curated JSON descriptor", {
    api <- mock_create_openeo_v1()
    dir <- withr::local_tempdir()
    file <- file.path(dir, "processes.R")
    proc_dir <- file.path(dir, "processes")
    dir.create(proc_dir)
    curated <- file.path(proc_dir, "myproc.json")
    jsonlite::write_json(
        list(id = "myproc", summary = "curated"),
        curated, auto_unbox = TRUE
    )

    expr <- parse(text = "myproc <- function(a) { a }")[[1]]
    `openeo-process`(api, list(expr), file = file)

    process <- jsonlite::read_json(curated)
    expect_equal(process$summary, "curated")
    expect_null(process$`_generated_by`)
})
