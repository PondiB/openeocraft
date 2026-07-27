test_that("make_url joins path segments", {
    expect_equal(
        make_url("https://host.tld", "/jobs", "abc", "results"),
        "https://host.tld/jobs/abc/results"
    )
})

test_that("make_url appends named query parameters", {
    url <- make_url("https://host.tld", "/jobs", limit = 10, page = 2)
    expect_equal(url, "https://host.tld/jobs?limit=10&page=2")
})

test_that("make_url without params has no query string", {
    expect_false(grepl("\\?", make_url("https://host.tld", "/processes")))
})

test_that("add_link appends a link with extra attributes", {
    doc <- list()
    doc <- add_link(doc, "self", "https://h/self", type = "application/json")
    expect_length(doc$links, 1L)
    expect_equal(doc$links[[1]]$rel, "self")
    expect_equal(doc$links[[1]]$href, "https://h/self")
    expect_equal(doc$links[[1]]$type, "application/json")
})

test_that("new_link drops NULL attributes but keeps rel and href", {
    link <- new_link("next", "https://h/next", type = NULL, title = "T")
    expect_equal(link$rel, "next")
    expect_equal(link$href, "https://h/next")
    expect_null(link$type)
    expect_equal(link$title, "T")
})

test_that("update_link replaces all links sharing a rel", {
    doc <- list()
    doc <- add_link(doc, "self", "https://h/old")
    doc <- add_link(doc, "self", "https://h/old2")
    doc <- add_link(doc, "root", "https://h/root")
    doc <- update_link(doc, "self", "https://h/new")

    rels <- vapply(doc$links, function(x) x$rel, character(1))
    hrefs <- vapply(doc$links, function(x) x$href, character(1))
    expect_equal(sum(rels == "self"), 1L)
    expect_true("https://h/new" %in% hrefs)
    expect_false("https://h/old" %in% hrefs)
    expect_true("https://h/root" %in% hrefs)
})

test_that("delete_link removes matching rels only", {
    doc <- list()
    doc <- add_link(doc, "next", "https://h/next")
    doc <- add_link(doc, "root", "https://h/root")
    doc <- delete_link(doc, "next")

    rels <- vapply(doc$links, function(x) x$rel, character(1))
    expect_false("next" %in% rels)
    expect_true("root" %in% rels)
})

test_that("links_navigation adds prev/next based on paging", {
    api <- mock_create_openeo_v1()
    req <- mock_req("/jobs", method = "GET")
    doc <- list(numberMatched = 25L, links = list())

    # Middle page: both prev and next present.
    doc2 <- links_navigation(
        doc, api, req,
        endpoint = "/jobs", limit = 10, page = 2, type = "application/json"
    )
    rels <- vapply(doc2$links, function(x) x$rel, character(1))
    expect_true("prev" %in% rels)
    expect_true("next" %in% rels)

    # First page: no prev.
    doc_first <- links_navigation(
        doc, api, req,
        endpoint = "/jobs", limit = 10, page = 1, type = "application/json"
    )
    rels_first <- vapply(doc_first$links, function(x) x$rel, character(1))
    expect_false("prev" %in% rels_first)
    expect_true("next" %in% rels_first)

    # Last page: no next.
    doc_last <- links_navigation(
        doc, api, req,
        endpoint = "/jobs", limit = 10, page = 3, type = "application/json"
    )
    rels_last <- vapply(doc_last$links, function(x) x$rel, character(1))
    expect_true("prev" %in% rels_last)
    expect_false("next" %in% rels_last)
})
