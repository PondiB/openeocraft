test_that("make_fn builds a callable function from params and body", {
    f <- make_fn(alist(x = , y = 1), quote(x + y))
    expect_true(is.function(f))
    expect_equal(f(2), 3) # y defaults to 1
    expect_equal(f(2, 3), 5)
    expect_named(formals(f), c("x", "y"))
})

test_that("placeholders extracts schema-prefixed $ref names", {
    x <- list(
        list(`$ref` = "openeo:bbox", ref = "openeo:bbox"),
        list(`$ref` = "other:thing", ref = "other:thing"),
        list(nested = list(`$ref` = "openeo:datacube", ref = "openeo:datacube"))
    )
    out <- placeholders(x, schema = "openeo")
    expect_true("bbox" %in% out)
    expect_true("datacube" %in% out)
    expect_false(any(grepl("other", out)))
})

test_that("placeholders returns NULL for non-list input", {
    expect_null(placeholders("scalar"))
    expect_null(placeholders(42))
})

test_that("fake_req returns a plausible request skeleton", {
    req <- fake_req(path = "/jobs", body = list(a = 1))
    expect_equal(req$REQUEST_METHOD, "GET")
    expect_equal(req$body, list(a = 1))
    expect_true(all(
        c("HTTP_HOST", "SERVER_NAME", "SERVER_PORT") %in% names(req)
    ))
})

test_that("as_call and as_name build call objects", {
    cl <- as_call("sum", list(1, 2))
    expect_true(is.call(cl))
    expect_equal(eval(cl), 3)
    expect_true(is.name(as_name("foo")))
})

test_that("param builds required and defaulted formals", {
    req_par <- param("x")
    expect_named(req_par, "x")
    def_par <- param("n", default = 5)
    expect_equal(def_par$n, 5)
})
