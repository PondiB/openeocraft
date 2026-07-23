new_test_api <- function(env = parent.frame()) {
    api <- mock_create_openeo_v1()
    api$work_dir <- withr::local_tempdir(.local_envir = env)
    # Simple arithmetic processes for UDP expansion tests.
    ns <- get_namespace(api)
    assign("add", function(x, y) x + y, envir = ns)
    assign("subtract", function(x, y) x - y, envir = ns)
    assign("multiply", function(x, y) x * y, envir = ns)
    api
}

mock_token <- function() {
    readLines(system.file("mock/token", package = "openeocraft"), warn = FALSE)[[1]]
}

# UDP: add_one(x) = add(x, 1) using from_parameter
add_one_udp_body <- function() {
    list(
        summary = "Add one",
        parameters = list(list(name = "x", schema = list(type = "number"))),
        process_graph = list(
            step = list(
                process_id = "add",
                arguments = list(
                    x = list(from_parameter = "x"),
                    y = 1
                ),
                result = TRUE
            )
        )
    )
}

test_that("process_graphs CRUD: put, get, list, delete", {
    api <- new_test_api()
    # Resolve token user from mock credentials
    token <- mock_token()
    user <- get_token_user(api, token)

    put <- process_graph_put(api, user, "add_one", add_one_udp_body())
    expect_true(put$created)
    expect_equal(put$udp$id, "add_one")

    got <- process_graph_get(api, user, "add_one")
    expect_equal(got$summary, "Add one")
    expect_true("step" %in% names(got$process_graph))

    # Replace → not created
    put2 <- process_graph_put(api, user, "add_one", add_one_udp_body())
    expect_false(put2$created)

    listed <- process_graphs_read(api, user)
    expect_named(listed, "add_one")
    meta <- process_graph_metadata(listed$add_one)
    expect_null(meta$process_graph)

    process_graph_delete(api, user, "add_one")
    expect_error(process_graph_get(api, user, "add_one"), class = "condition")
})

test_that("process_graph_check rejects invalid graphs", {
    expect_error(process_graph_check(NULL, "x"), class = "condition")
    expect_error(
        process_graph_check(list(summary = "s"), "x"),
        class = "condition"
    )
    # Two result nodes
    bad <- list(
        process_graph = list(
            a = list(process_id = "add", arguments = list(x = 1, y = 2), result = TRUE),
            b = list(process_id = "add", arguments = list(x = 1, y = 2), result = TRUE)
        )
    )
    expect_error(process_graph_check(bad, "x"), class = "condition")
})

test_that("api_process_graphs_* endpoints require auth and round-trip", {
    api <- new_test_api()
    token <- mock_token()
    api_attr(api, "api_base_url") <- "https://h"

    # Missing auth → 401
    err <- tryCatch(
        api_process_graphs_list(api, mock_req("/process_graphs"), mock_res()),
        error = function(e) e
    )
    expect_equal(err$status, 401L)

    body <- add_one_udp_body()
    req_put <- mock_req(
        "/process_graphs", method = "PUT",
        HTTP_AUTHORIZATION = token
    )
    req_put$body <- body
    res <- mock_res()
    out <- api_process_graph_put(api, req_put, res, "add_one")
    expect_equal(res$status, 201L)
    expect_equal(out, list())

    req_get <- mock_req("/process_graphs", HTTP_AUTHORIZATION = token)
    doc <- api_process_graphs_list(api, req_get, mock_res())
    expect_length(doc$processes, 1L)
    expect_null(doc$processes[[1]]$process_graph)

    one <- api_process_graph_get(api, req_get, mock_res(), "add_one")
    expect_equal(one$id, "add_one")
    expect_true(!is.null(one$process_graph))

    res_del <- mock_res()
    api_process_graph_delete(api, req_get, res_del, "add_one")
    expect_equal(res_del$status, 204L)
})

test_that("from_parameter is wired into pnode_args expressions", {
    pg <- list(
        process_graph = list(
            n = list(
                process_id = "add",
                arguments = list(
                    x = list(from_parameter = "x"),
                    y = 1
                ),
                result = TRUE
            )
        ),
        parameters = list(list(name = "x"))
    )
    fn <- pgraph_fn(pg, eval_env = new.env(parent = emptyenv()))
    # Formals include x; body references the symbol x
    expect_true("x" %in% names(formals(fn)))
})

test_that("namespace user and UDP-by-process_id expand and evaluate", {
    api <- new_test_api()
    token <- mock_token()
    user <- get_token_user(api, token)
    process_graph_put(api, user, "add_one", add_one_udp_body())

    # Call stored UDP with namespace = "user"
    pg_user_ns <- list(
        process_graph = list(
            out = list(
                process_id = "add_one",
                namespace = "user",
                arguments = list(x = 41),
                result = TRUE
            )
        )
    )
    result <- run_pgraph(api, list(), user, list(id = "j"), pg_user_ns)
    expect_equal(result, 42)

    # Same UDP via process_id with namespace omitted (fallback to user store)
    pg_fallback <- list(
        process_graph = list(
            out = list(
                process_id = "add_one",
                arguments = list(x = 9),
                result = TRUE
            )
        )
    )
    expect_equal(run_pgraph(api, list(), user, list(id = "j"), pg_fallback), 10)
})

test_that("unsupported namespace and unknown process error cleanly", {
    api <- new_test_api()
    token <- mock_token()
    user <- get_token_user(api, token)

    pg_bad_ns <- list(
        process_graph = list(
            out = list(
                process_id = "add",
                namespace = "https://example.com/remote",
                arguments = list(x = 1, y = 2),
                result = TRUE
            )
        )
    )
    err <- tryCatch(
        run_pgraph(api, list(), user, list(id = "j"), pg_bad_ns),
        error = function(e) e
    )
    expect_equal(err$status, 400L)

    pg_unknown <- list(
        process_graph = list(
            out = list(
                process_id = "no_such_udp",
                namespace = "user",
                arguments = list(),
                result = TRUE
            )
        )
    )
    err2 <- tryCatch(
        run_pgraph(api, list(), user, list(id = "j"), pg_unknown),
        error = function(e) e
    )
    expect_equal(err2$status, 400L)
})

test_that("circular UDP references are rejected", {
    api <- new_test_api()
    token <- mock_token()
    user <- get_token_user(api, token)

    # a calls b, b calls a
    process_graph_put(api, user, "a", list(
        process_graph = list(
            n = list(
                process_id = "b",
                namespace = "user",
                arguments = list(),
                result = TRUE
            )
        )
    ))
    process_graph_put(api, user, "b", list(
        process_graph = list(
            n = list(
                process_id = "a",
                namespace = "user",
                arguments = list(),
                result = TRUE
            )
        )
    ))

    pg <- list(
        process_graph = list(
            out = list(
                process_id = "a",
                namespace = "user",
                arguments = list(),
                result = TRUE
            )
        )
    )
    err <- tryCatch(
        run_pgraph(api, list(), user, list(id = "j"), pg),
        error = function(e) e
    )
    expect_equal(err$status, 400L)
    expect_match(err$message, "Circular")
})

test_that("is_pnode accepts optional namespace field", {
    node <- list(
        process_id = "add",
        namespace = "user",
        arguments = list(x = 1, y = 2),
        result = TRUE
    )
    expect_true(is_pnode(node))
    expect_true(is_pgraph(list(process_graph = list(n = node))))
})
