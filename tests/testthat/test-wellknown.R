test_that("add_wellknown_version appends a version entry", {
    doc <- list()
    doc <- add_wellknown_version(
        doc, "1.2.0", "https://h/openeo/1.2", production = TRUE
    )
    expect_length(doc$versions, 1L)
    expect_equal(doc$versions[[1]]$api_version, "1.2.0")
    expect_equal(doc$versions[[1]]$url, "https://h/openeo/1.2")
    expect_true(doc$versions[[1]]$production)
})

test_that("add_wellknown_version keeps extra attrs and drops NULLs", {
    entry <- new_wellknown_version(
        "1.0.0", "https://h/v1", FALSE,
        title = "v1", note = NULL
    )
    expect_equal(entry$api_version, "1.0.0")
    expect_false(entry$production)
    expect_equal(entry$title, "v1")
    expect_null(entry$note)
})

test_that("update_wellknown_version replaces entries sharing api_version", {
    doc <- list()
    doc <- add_wellknown_version(doc, "1.0.0", "https://h/old")
    doc <- add_wellknown_version(doc, "1.0.0", "https://h/old2")
    doc <- add_wellknown_version(doc, "1.2.0", "https://h/other")
    doc <- update_wellknown_version(doc, "1.0.0", "https://h/new")

    versions <- vapply(doc$versions, function(x) x$api_version, character(1))
    urls <- vapply(doc$versions, function(x) x$url, character(1))
    expect_equal(sum(versions == "1.0.0"), 1L)
    expect_true("https://h/new" %in% urls)
    expect_false("https://h/old" %in% urls)
    expect_true("https://h/other" %in% urls)
})
