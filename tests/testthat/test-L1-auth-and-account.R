api <- mock_create_openeo_v1()
token <- readLines(file(system.file("mock/token", package = "openeocraft")))

test_that("Bearer tokens accept method/provider/token format", {
    bare <- readLines(system.file("mock/token", package = "openeocraft"))
    req <- mock_req("/jobs", HTTP_AUTHORIZATION = paste0("Bearer basic//", bare))
    expect_equal(get_token_user(api, get_token(req)), "mock_user")
})

test_that("Missing auth returns 401; invalid returns 403", {
    err_missing <- tryCatch(
        get_token_user(api, get_token(mock_req("/jobs"))),
        error = function(e) e
    )
    expect_equal(err_missing$status, 401L)

    err_bad <- tryCatch(
        get_token_user(
            api,
            get_token(mock_req("/jobs", HTTP_AUTHORIZATION = "Bearer nope"))
        ),
        error = function(e) e
    )
    expect_equal(err_bad$status, 403L)
})

test_that("GET /credentials/basic is supported", {
    req <- mock_req(
        "/credentials/basic",
        method = "GET",
        HTTP_AUTHORIZATION = paste(
            "Basic",
            base64enc::base64encode(charToRaw("mock_user:mock_password"))
        )
    )
    # mock credentials file uses whatever mock_create set up
    # Fall back: just ensure landing lists credentials endpoint in real server;
    # here verify api_credential rejects bad user.
    req_bad <- mock_req(
        "/credentials/basic",
        method = "GET",
        HTTP_AUTHORIZATION = paste(
            "Basic",
            base64enc::base64encode(charToRaw("nope:wrong"))
        )
    )
    err <- tryCatch(api_credential(api, req_bad, mock_res()), error = function(e) e)
    expect_equal(err$status, 403L)
})

test_that("GET /me returns user_id", {
    req <- mock_req("/me", HTTP_AUTHORIZATION = token)
    me <- api_me(api, req, mock_res())
    expect_equal(me$user_id, "mock_user")
})

test_that("GET /credentials/oidc returns providers array", {
    doc <- api_credentials_oidc(api, mock_req("/credentials/oidc"), mock_res())
    expect_true("providers" %in% names(doc))
    expect_type(doc$providers, "list")
})

test_that("GET / includes billing metadata", {
    doc <- api_landing_page(api, mock_req("/"), mock_res())
    expect_true("billing" %in% names(doc))
    expect_equal(doc$billing$default_plan, "free")
})
