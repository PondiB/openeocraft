make_test_req <- function(path = "/collections/x") {
    list(
        rook.url_scheme = "https",
        HTTP_HOST = "h.tld",
        PATH_INFO = path
    )
}

test_that("parent_url and link_parent strip the last path segment", {
    api <- mock_create_openeo_v1()
    api_attr(api, "api_base_url") <- "https://h.tld"
    req <- make_test_req("/collections/x")

    doc <- link_parent(list(), api, req)
    parent <- Filter(function(l) l$rel == "parent", doc$links)[[1]]
    expect_equal(parent$href, "https://h.tld/collections")
    expect_equal(parent$type, "application/json")
})

test_that("links_navigagion_post adds prev/next links with a body", {
    api <- mock_create_openeo_v1()
    api_attr(api, "api_base_url") <- "https://h.tld"
    req <- make_test_req("/search")
    doc <- list(numberMatched = 25L, links = list())

    out <- links_navigagion_post(
        doc, api, req,
        endpoint = "/search", limit = 10, page = 2,
        type = "application/json", merge = FALSE
    )
    rels <- vapply(out$links, function(x) x$rel, character(1))
    expect_true(all(c("prev", "next") %in% rels))
    nxt <- Filter(function(l) l$rel == "next", out$links)[[1]]
    expect_equal(nxt$href, "https://h.tld/search")
    expect_equal(nxt$body$page, 3)
})

test_that("link_spec adds a service-spec link only when configured", {
    api <- mock_create_openeo_v1()
    api_attr(api, "api_base_url") <- "https://h.tld"
    req <- make_test_req("/")

    # No endpoint configured -> unchanged.
    expect_equal(link_spec(list(links = NULL), api, req), list(links = NULL))

    api_attr(api, "spec_endpoint") <- "/openapi.json"
    doc <- link_spec(list(), api, req)
    spec <- Filter(function(l) l$rel == "service-spec", doc$links)[[1]]
    expect_equal(spec$href, "https://h.tld/openapi.json")
    expect_match(spec$type, "openapi")
})

test_that("link_docs adds a service-doc link only when configured", {
    api <- mock_create_openeo_v1()
    api_attr(api, "api_base_url") <- "https://h.tld"
    req <- make_test_req("/")

    expect_equal(link_docs(list(links = NULL), api, req), list(links = NULL))

    api_attr(api, "docs_endpoint") <- "/docs"
    doc <- link_docs(list(), api, req)
    docs <- Filter(function(l) l$rel == "service-doc", doc$links)[[1]]
    expect_equal(docs$href, "https://h.tld/docs")
    expect_equal(docs$type, "text/html")
})
