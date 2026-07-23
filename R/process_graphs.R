#' User-defined process graph (UDP) storage and CRUD
#'
#' Persists per-user process graphs under the workspace as
#' `process_graphs.rds`, mirroring the jobs index pattern. Used by
#' `/process_graphs` endpoints and by process-graph evaluation when a node
#' references a stored UDP via `process_id` / `namespace`.
#'
#' @name process_graph_helpers
#' @keywords internal
NULL

process_graphs_file <- function(api, user) {
    file.path(api_user_workspace(api, user), "process_graphs.rds")
}

process_graphs_read <- function(api, user) {
    file <- process_graphs_file(api, user)
    if (!file.exists(file)) {
        return(list())
    }
    readRDS(file)
}

process_graphs_write <- function(api, user, graphs) {
    file <- process_graphs_file(api, user)
    atomic_save_rds(graphs, file, "Could not save process graphs")
    invisible(NULL)
}

#' Validate and normalise a user-defined process payload
#'
#' @param body Request body list.
#' @param process_graph_id Id from the URL path (authoritative).
#' @return Normalised UDP list.
#' @keywords internal
process_graph_check <- function(body, process_graph_id) {
    if (is.null(body) || !is.list(body)) {
        api_stop(400L, "Missing process graph information")
    }
    if (!"process_graph" %in% names(body) || is.null(body$process_graph)) {
        api_stop(400L, "Invalid process graph: 'process_graph' is required")
    }
    pg_wrap <- list(process_graph = body$process_graph)
    if (!is_pgraph(pg_wrap)) {
        api_stop(
            400L,
            "Invalid process_graph: nodes need process_id/arguments ",
            "and exactly one result node"
        )
    }
    if (!is.null(body$id) && !identical(as.character(body$id), process_graph_id)) {
        api_stop(400L, "Body 'id' must match the process_graph_id path parameter")
    }
    list(
        id = process_graph_id,
        summary = body$summary,
        description = body$description,
        parameters = if (is.null(body$parameters)) list() else body$parameters,
        returns = body$returns,
        categories = body$categories,
        process_graph = body$process_graph,
        links = if (is.null(body$links)) list() else body$links
    )
}

#' @rdname process_graph_helpers
#' @keywords internal
process_graph_get <- function(api, user, process_graph_id, error = TRUE) {
    graphs <- process_graphs_read(api, user)
    if (!(process_graph_id %in% names(graphs))) {
        if (error) {
            api_stop(404L, "Process graph not found")
        }
        return(NULL)
    }
    graphs[[process_graph_id]]
}

#' @rdname process_graph_helpers
#' @keywords internal
process_graph_put <- function(api, user, process_graph_id, body) {
    udp <- process_graph_check(body, process_graph_id)
    graphs <- process_graphs_read(api, user)
    created <- !(process_graph_id %in% names(graphs))
    graphs[[process_graph_id]] <- udp
    process_graphs_write(api, user, graphs)
    list(udp = udp, created = created)
}

#' @rdname process_graph_helpers
#' @keywords internal
process_graph_delete <- function(api, user, process_graph_id) {
    graphs <- process_graphs_read(api, user)
    if (!(process_graph_id %in% names(graphs))) {
        api_stop(404L, "Process graph not found")
    }
    graphs[[process_graph_id]] <- NULL
    process_graphs_write(api, user, graphs)
    invisible(NULL)
}

#' Metadata-only view of a UDP (omit large process_graph for list responses)
#'
#' @keywords internal
process_graph_metadata <- function(udp) {
    udp$process_graph <- NULL
    udp
}

#' Build a resolver for process_id + namespace → UDP or NULL (predefined)
#'
#' Returns a list with `resolve(process_id, namespace)`, `push(id)`, and
#' `pop()`. `resolve` returns `NULL` for predefined processes (present in the
#' API R namespace) and a UDP list for stored user processes. Only the `"user"`
#' namespace (and implicit default → user fallback) is supported; URL
#' namespaces are deferred.
#'
#' @param api API object.
#' @param user Authenticated user id.
#' @keywords internal
make_process_resolver <- function(api, user) {
    ns_env <- get_namespace(api)
    stack <- new.env(parent = emptyenv())
    stack$ids <- character()

    list(
        push = function(process_id) {
            if (process_id %in% stack$ids) {
                api_stop(
                    400L,
                    "Circular user-defined process reference: ", process_id,
                    id = "ProcessGraphCircular"
                )
            }
            stack$ids <- c(stack$ids, process_id)
        },
        pop = function() {
            n <- length(stack$ids)
            if (n > 0L) {
                stack$ids <- stack$ids[-n]
            }
        },
        resolve = function(process_id, namespace = NULL) {
            ns <- namespace
            if (is.null(ns) || (is.character(ns) && !nzchar(ns))) {
                if (exists(process_id, envir = ns_env, inherits = FALSE)) {
                    return(NULL)
                }
                ns <- "user"
            }
            if (!identical(as.character(ns), "user")) {
                api_stop(
                    400L,
                    "Unsupported process namespace '", ns, "'",
                    id = "ProcessUnsupported"
                )
            }
            udp <- process_graph_get(api, user, process_id, error = FALSE)
            if (is.null(udp)) {
                api_stop(
                    400L,
                    "Unknown process '", process_id, "'",
                    id = "ProcessUnsupported"
                )
            }
            udp
        },
        ns_env = ns_env
    )
}
