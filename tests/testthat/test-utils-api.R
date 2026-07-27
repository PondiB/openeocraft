# Helper: capture the `status` field of an api_stop() error condition.
expect_api_status <- function(expr, status) {
    err <- tryCatch(expr, error = function(e) e)
    expect_s3_class(err, "condition")
    expect_equal(err$status, status)
}

test_that("api_cors_handler answers OPTIONS preflight with headers", {
    res <- mock_res()
    req <- mock_req("/", method = "OPTIONS")
    out <- api_cors_handler(req, res, origin = "*", methods = "GET, POST")
    expect_equal(res$getHeader("Access-Control-Allow-Origin"), "*")
    expect_equal(res$getHeader("Access-Control-Allow-Methods"), "GET, POST")
    expect_equal(res$status, 200L)
    expect_equal(out, list())
})

test_that("api_cors_handler forwards non-OPTIONS requests", {
    forwarded <- FALSE
    local_mocked_bindings(
        forward = function() forwarded <<- TRUE, .package = "plumber"
    )
    res <- mock_res()
    req <- mock_req("/", method = "GET")
    api_cors_handler(req, res)
    expect_true(forwarded)
    expect_equal(res$getHeader("Access-Control-Allow-Origin"), "*")
})

test_that("api_error_handler sets status, id and WWW-Authenticate on 401", {
    res <- mock_res()
    out <- api_error_handler(
        req = list(), res = res,
        err = list(status = 401L, message = "no token")
    )
    expect_equal(res$status, 401L)
    expect_equal(res$getHeader("WWW-Authenticate"), "Basic, Bearer")
    expect_equal(out$id, "HTTP401")
    expect_equal(out$code, 401L)
    expect_match(out$message, "no token")
})

test_that("api_error_handler defaults missing fields and keeps id + links", {
    res <- mock_res()
    out <- api_error_handler(
        list(), res,
        err = list(id = "Custom", links = list(list(rel = "about")))
    )
    expect_equal(res$status, 500)
    expect_equal(out$id, "Custom")
    expect_match(out$message, "Internal server error")
    expect_equal(out$links, list(list(rel = "about")))
})

test_that("api_stop raises a condition carrying status and id", {
    expect_api_status(api_stop(404L, "missing"), 404L)
    err <- tryCatch(
        api_stop(400L, "bad", id = "X"),
        error = function(e) e
    )
    expect_equal(err$id, "X")
})

test_that("api_success returns a code/message list", {
    expect_equal(api_success(200L, "ok"), list(code = 200L, message = "ok"))
})

test_that(".openeocraft_default_api_base_url honours env var, else NULL", {
    withr::local_envvar(c(OPENEOCRAFT_API_BASE_URL = "https://env.tld"))
    expect_equal(.openeocraft_default_api_base_url(), "https://env.tld")

    withr::local_envvar(c(OPENEOCRAFT_API_BASE_URL = NA))
    skip_if(file.exists("/.dockerenv"))
    expect_null(.openeocraft_default_api_base_url())
})

test_that("get_host prefers configured api_base_url", {
    api <- mock_create_openeo_v1()
    api_attr(api, "api_base_url") <- "https://configured.tld"
    expect_equal(
        get_host(api, mock_req("/", method = "GET")),
        "https://configured.tld"
    )
})

test_that("get_host derives host from request headers when unconfigured", {
    withr::local_envvar(c(OPENEOCRAFT_API_BASE_URL = NA))
    skip_if(file.exists("/.dockerenv"))
    api <- mock_create_openeo_v1()

    # HTTP_HOST branch
    req <- mock_req("/", method = "GET")
    expect_equal(get_host(api, req), "https://localhost")

    # SERVER_NAME + SERVER_PORT branch (no HTTP_HOST)
    req2 <- list(
        rook.url_scheme = "http", SERVER_NAME = "srv", SERVER_PORT = "8080"
    )
    expect_equal(get_host(api, req2), "http://srv:8080")
})

test_that("get_path and get_method read request fields", {
    req <- list(PATH_INFO = "/jobs/abc", REQUEST_METHOD = "POST")
    expect_equal(get_path(req), "/jobs/abc")
    expect_equal(get_method(req), "POST")
})

test_that("set_credentials creates an empty credential store", {
    api <- mock_create_openeo_v1()
    f <- withr::local_tempfile(fileext = ".rds")
    res <- withVisible(set_credentials(api, f))
    expect_false(res$visible)
    expect_true(file.exists(f))
    creds <- readRDS(f)
    expect_named(creds, c("users", "tokens"))
    expect_equal(api_attr(api, "credentials"), f)
})

test_that("new_credential stores a user/password", {
    api <- mock_create_openeo_v1()
    f <- withr::local_tempfile(fileext = ".rds")
    set_credentials(api, f)
    new_credential(api, "alice", "secret")
    creds <- readRDS(f)
    expect_equal(creds$users$alice$user, "alice")
    expect_equal(creds$users$alice$password, "secret")
})

test_that("new_token registers a token linked to a user", {
    creds <- empty_credentials()
    creds <- new_token(creds, "bob", valid_days = 1)
    tok <- creds$users$bob$token
    expect_type(tok, "character")
    expect_equal(creds$tokens[[tok]]$user, "bob")
    expect_true(creds$tokens[[tok]]$expiry > Sys.time())
})

test_that("get_token parses bearer, method/provider/token and legacy forms", {
    expect_equal(get_token(list(HTTP_AUTHORIZATION = "Bearer abc123")), "abc123")
    expect_equal(
        get_token(list(HTTP_AUTHORIZATION = "basic/oidc/xyz.token")),
        "xyz.token"
    )
    expect_equal(get_token(list(HTTP_AUTHORIZATION = "legacytoken")), "legacytoken")
    # Malformed method/provider (only one slash) -> NA sentinel
    expect_true(is.na(get_token(list(HTTP_AUTHORIZATION = "bad/token"))))
    # Missing header -> empty
    expect_length(get_token(list()), 0L)
    expect_length(get_token(list(HTTP_AUTHORIZATION = "")), 0L)
})

test_that("get_token_user validates presence, format, existence and expiry", {
    api <- mock_create_openeo_v1()
    f <- withr::local_tempfile(fileext = ".rds")
    creds <- list(
        users = list(alice = list(user = "alice")),
        tokens = list(
            good = list(expiry = Sys.time() + 3600, user = "alice"),
            stale = list(expiry = Sys.time() - 10, user = "bob")
        )
    )
    saveRDS(creds, f)
    api_attr(api, "credentials") <- f

    expect_equal(get_token_user(api, "good"), "alice")
    expect_api_status(get_token_user(api, character()), 401L) # missing
    expect_api_status(get_token_user(api, ""), 401L) # empty
    expect_api_status(get_token_user(api, NA_character_), 403L) # bad format
    expect_api_status(get_token_user(api, "nope"), 403L) # unknown
    expect_api_status(get_token_user(api, "stale"), 403L) # expired
})

test_that("assert_payment_allowed permits free plans, rejects paid ones", {
    expect_true(assert_payment_allowed("Free"))
    expect_true(assert_payment_allowed(NULL))
    expect_true(assert_payment_allowed(""))
    expect_api_status(assert_payment_allowed("premium"), 402L)
})

test_that("make_*_files_url embed a base64 user token", {
    host <- "https://h"
    token <- base64enc::base64encode(charToRaw("alice"))
    expect_equal(
        make_job_files_url(host, "alice", "job1", "_collection.json"),
        paste0(host, "/files/jobs/job1/_collection.json?token=", token)
    )
    expect_equal(
        make_workspace_files_url(host, "alice", "data", "x.tif"),
        paste0(host, "/files/root/data/x.tif?token=", token)
    )
})

test_that("api_user_workspace creates and returns the user directory", {
    api <- mock_create_openeo_v1()
    api$work_dir <- withr::local_tempdir()
    ws <- api_user_workspace(api, "alice")
    expect_true(dir.exists(ws))
    expect_match(ws, "workspace/alice$")
})

test_that("parse_pagination_limit handles default, empty and invalid input", {
    expect_null(parse_pagination_limit(list(args = list())))
    expect_null(parse_pagination_limit(list(args = list(limit = ""))))
    expect_equal(parse_pagination_limit(list(args = list(limit = "5"))), 5L)
    expect_api_status(
        parse_pagination_limit(list(args = list(limit = "0"))), 400L
    )
    expect_api_status(
        parse_pagination_limit(list(args = list(limit = "abc"))), 400L
    )
})

test_that("parse_pagination_page defaults to 1 and validates", {
    expect_equal(parse_pagination_page(list(args = list())), 1L)
    expect_equal(parse_pagination_page(list(args = list(page = ""))), 1L)
    expect_equal(parse_pagination_page(list(args = list(page = "3"))), 3L)
    expect_api_status(
        parse_pagination_page(list(args = list(page = "0"))), 400L
    )
})

test_that("paginate_resource_list slices items and adds paging links", {
    api <- mock_create_openeo_v1()
    api_attr(api, "api_base_url") <- "https://h"
    req <- mock_req("/jobs", method = "GET")
    items <- as.list(1:25)

    # No limit -> unchanged.
    none <- paginate_resource_list(items, list(), api, req, "/jobs")
    expect_equal(none$items, items)

    # Middle page: prev, next, first, last.
    mid <- paginate_resource_list(
        items, list(links = list()), api, req, "/jobs",
        limit = 10, page = 2
    )
    expect_equal(mid$items, as.list(11:20))
    rels <- vapply(mid$doc$links, function(x) x$rel, character(1))
    expect_setequal(rels, c("next", "prev", "first", "last"))

    # First page: next + last, no prev/first.
    first <- paginate_resource_list(
        items, list(links = list()), api, req, "/jobs",
        limit = 10, page = 1
    )
    rels_first <- vapply(first$doc$links, function(x) x$rel, character(1))
    expect_true(all(c("next", "last") %in% rels_first))
    expect_false("prev" %in% rels_first)

    # Last page: prev + first, no next.
    last <- paginate_resource_list(
        items, list(links = list()), api, req, "/jobs",
        limit = 10, page = 3
    )
    expect_equal(last$items, as.list(21:25))
    rels_last <- vapply(last$doc$links, function(x) x$rel, character(1))
    expect_true("prev" %in% rels_last)
    expect_false("next" %in% rels_last)
})
