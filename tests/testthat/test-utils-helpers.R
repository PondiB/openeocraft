test_that("random_id returns a hex string of the requested byte length", {
    id <- random_id(8)
    expect_type(id, "character")
    expect_match(id, "^[0-9a-f]+$")
    expect_equal(nchar(id), 16L) # 2 hex chars per byte
    # Two draws are (almost surely) different.
    expect_false(identical(random_id(8), random_id(8)))
})

test_that("is_absolute_url distinguishes absolute from relative URLs", {
    expect_true(is_absolute_url("https://host.tld/path"))
    expect_true(is_absolute_url("http://x/y"))
    expect_false(is_absolute_url("/jobs/123"))
    expect_false(is_absolute_url("relative/path"))
})

test_that("get_pages computes the number of pages from numberMatched", {
    expect_equal(get_pages(list(numberMatched = 25L), limit = 10), 3)
    expect_equal(get_pages(list(numberMatched = 20L), limit = 10), 2)
    expect_equal(get_pages(list(numberMatched = 0L), limit = 10), 0)
})

test_that("format_endpoint normalises plumber path params to openEO braces", {
    expect_equal(format_endpoint("/jobs/<job_id:str>"), "/jobs/{job_id}")
    expect_equal(format_endpoint("/jobs/<job_id>"), "/jobs/{job_id}")
    expect_equal(
        format_endpoint("/jobs/<job_id:str>/results"),
        "/jobs/{job_id}/results"
    )
})

test_that("get_link builds URLs with optional query parameters", {
    expect_equal(
        get_link("https://h", "/collections", "x"),
        "https://h/collections/x"
    )
    expect_equal(
        get_link("https://h", "/collections", limit = 5),
        "https://h/collections?limit=5"
    )
})

test_that("get_method reads the request method", {
    expect_equal(get_method(list(REQUEST_METHOD = "POST")), "POST")
})

test_that("len and param behave as thin wrappers", {
    expect_equal(len(1:4), 4L)
    p <- param("limit", default = 10)
    expect_named(p, "limit")
    expect_equal(p$limit, 10)
    p_req <- param("x")
    expect_named(p_req, "x")
})

test_that("transact runs commit and rolls back on error", {
    committed <- FALSE
    val <- transact(1 + 1, commit = {
        committed <- TRUE
    })
    expect_equal(val, 2)
    expect_true(committed)

    rolled_back <- FALSE
    expect_silent(
        transact(stop("boom"), rollback = {
            rolled_back <- TRUE
        })
    )
    expect_true(rolled_back)

    # Without a rollback handler, the error propagates.
    expect_error(transact(stop("boom")), "boom")
})
