# api_user_workspace(api, user) -> work_dir / <user>
# work_dir / <user> / jobs.rds
# work_dir / <user> / <job_id> /
# work_dir / <user> / files /

.job_status_error <- "error"
.job_status_finished <- "finished"

job_sync_id <- function() {
  format(Sys.time(), "job-%Y%m%d")
}
# list of named lists, each containing job details
job_read_rds <- function(api, user) {
  file <- file.path(api_user_workspace(api, user), "jobs.rds")
  if (!file.exists(file)) {
    return(list())
  }
  jobs <- readRDS(file)
  return(jobs)
}
job_save_rds <- function(api, user, job, jobs) {
  jobs[[job$id]] <- job
  file <- file.path(api_user_workspace(api, user), "jobs.rds")
  tryCatch(saveRDS(jobs, file), error = function(e) {
    api_stop(500, "Could not save the jobs file")
  })
  invisible(NULL)
}
job_crt_rds <- function(api, user, job) {
  jobs <- job_read_rds(api, user)
  job_save_rds(api, user, job, jobs)
}
job_upd_status <- function(api, user, job_id, status) {
  jobs <- job_read_rds(api, user)
  if (!job_id %in% names(jobs)) {
    api_stop(500, "Could not find sync job id")
  }
  job <- jobs[[job_id]]
  if (job$status != status) {
    job$status <- status
    job_save_rds(api, user, job, jobs)
  }
  job
}

job_get <- function(api, user, job_id) {
  jobs <- job_read_rds(api, user)
  if (!job_id %in% names(jobs)) {
    api_stop(500, "Could not find sync job id")
  }
  job <- jobs[[job_id]]
  job
}
job_delete_rds <- function(api, user, job, jobs) {
  if (!job$id %in% names(jobs))
    return(invisible(NULL))
  jobs[[job$id]] <- NULL
  file <- file.path(api_user_workspace(api, user), "jobs.rds")
  tryCatch(saveRDS(jobs, file), error = function(e) {
    api_stop(500, "Could not save the jobs index file")
  })
}
#' @export
job_get_dir <- function(api, user, job_id) {
  file.path(api_user_workspace(api, user), "jobs", job_id)
}
job_new_dir <- function(api, user, job) {
  job_dir <- job_get_dir(api, user, job$id)
  if (dir.exists(job_dir)) {
    unlink(job_dir, recursive = TRUE)
    if (dir.exists(job_dir)) {
      api_stop(500, "Could not delete the job ", job$id, "'s folder")
    }
  }
  dir.create(job_dir, recursive = TRUE)
  if (!dir.exists(job_dir)) {
    api_stop(500, "Could not create the job ", job$id, "'s folder")
  }
}
job_del_dir <- function(api, user, job_id) {
  job_dir <- job_get_dir(api, user, job_id)
  unlink(job_dir, recursive = TRUE)
  if (dir.exists(job_dir)) {
    api_stop(500, "Could not delete the job ", job_id, "'s folder")
  }
}

procs_read_rds <- function(api) {
  file <- file.path(api_workdir(api), "procs.rds")
  if (!file.exists(file)) {
    return(list())
  }
  procs <- readRDS(file)
  procs
}

procs_save_rds <- function(api, procs) {
  file <- file.path(api_workdir(api), "procs.rds")
  tryCatch(saveRDS(procs, file), error = function(e) {
    api_stop(500, "Could not save the procs file")
  })
  invisible(NULL)
}
logs_read_rds <- function(api, user, job_id) {
  file <- file.path(api_user_workspace(api, user), "jobs", job_id, "logs.rds")
  if (!file.exists(file)) {
    return(list())
  }
  logs <- readRDS(file)
  logs
}
logs_save_rds <- function(api, user, job_id, logs) {
  file <- file.path(api_user_workspace(api, user), "jobs", job_id, "logs.rds")
  tryCatch(saveRDS(logs, file), error = function(e) NULL)
  invisible(NULL)
}

# TODO: include all possible fields in here. Required parameters
#   must come before the `...` (ellipsis) parameter; optional parameters
#   comes after
log_append <- function(api, user, job_id, code, level, message, ...) {
  # TODO: log this
  # - solution to `path` field:
  # - introduce 'markers' into processes` function so that
  #   using the traceback mechanism we can distinguish
  #   functions that are processes from internal R functions.
  #   Then we can use this to create a openEO traceback to
  #   store in `path` field.
  # - solution to `usage` field:
  #   create `usage_*()` API to generate metrics to be included in
  #   `usage` field.
  logs <- logs_read_rds(api, user, job_id)
  logs[[length(logs) + 1]] <- list(
    id = job_id,
    code = code,
    level = level,
    message = message,
    time = Sys.time(), ...
  )
  logs_save_rds(api, user, job_id, logs)
}

#' @export
job_sync <- function(api, req, user, job_id) {
  job <- job_upd_status(api, user, job_id, "running")
  tryCatch({
    run_pgraph(api, req, user, job, job$process)
    job_upd_status(api, user, job_id, "finished")
  },
  # TODO: add more information of errors:
  # - traceback
  # - implement proc_stop() function that store more details like
  #   code?
  # - implement proc_warning() function to update logs for warning
  error = function(e) {
    code <- 100
    if ("code" %in% names(e)) {
      code <- e$code
    }

    process_str <- deparse(job$process, width.cutoff = 500L)
    error_str <- paste(c(e$message, process_str), collapse = "\n")

    job_upd_status(api, user, job_id, .job_status_error)
    log_append(api, user, job_id, code, "error", error_str)
    invisible(NULL)
  })
}
job_async <- function(api, req, user, job_id) {
  job_dir <- job_get_dir(api, user, job_id)
  # Update job status
  job_upd_status(api, user, job_id, "running")
  proc <- suppressMessages(callr::r_bg(
    func = function(api, req, user, job_id) {
      openeocraft::job_sync(api, req, user, job_id)
    },
    args = list(api, req, user, job_id),
    stdout = file.path(job_dir, "_stdout.log"),
    stderr = file.path(job_dir, "_stderr.log"),
    poll_connection = FALSE
  ))
  proc
}
#' Get job information/metadata
#'
#' @param job_id The identifier for the job
#' @export
job_info <- function(api, user, job_id) {
  jobs <- job_read_rds(api, user)

  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404L, "Job not found")
  }

  # Retrieve the job from the jobs_list
  job <- jobs[[job_id]]

  # Return all metadata for the job
  job
}

#' Update job
#'
#' @param job_id The identifier for the job
#' @param body The request body containing updates
#' @export
job_update <- function(api, user, job_id, job) {
  # TODO: all checks should be done in api_*() functions level
  # TODO: implement job_check partial parameter that does the check
  #   job fields independently.
  #job_check(job, partial = TRUE)

  jobs <- job_read_rds(api, user)

  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404L, "Job not found")
  }

  # Retrieve the job from the jobs_list
  current_job <- jobs[[job_id]]

  # Update job details based on the provided body
  job <- utils::modifyList(current_job, job)

  job$status <- "updated"
  job$updated <- Sys.time()

  # Update the job in the jobs_list
  job_save_rds(api, user, job, jobs)

  list(id = job_id, message = "Job updated", code = 200L)
}


#' Delete job
#'
#' @param job_id The identifier for the job
#' @export
job_delete <- function(api, user, job_id) {
  jobs <- job_read_rds(api, user)
  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404L, "Job not found")
  }
  removed_job <- jobs[[job_id]]
  job_delete_rds(api, user, removed_job, jobs)
  # Delete the folder associated with the job_id
  job_del_dir(api, user, job_id)
}
# Get an estimate for a job
#' @export
job_estimate <- function(api, user, job_id) {
  # This would likely call a function to calculate cost or duration based on job details
  # calculate_estimate(job_id) not implemented as depends on the cloud provider the software is running on
  return(list(message = "The cost estimates will depends on the cloud provider the software is running on"))
}

# Retrieve logs for a job
#' @export
job_logs <- function(api, user, job_id, offset = 0, level = "info", limit = 10) {
  level_list <- c("error", "warning", "info", "debug")
  offset <- as.integer(offset)
  if (is.na(offset)) offset <- 0
  if (!level %in% level_list) {
    api_stop(400L, "level must be one of ",
             paste0("'", level_list, "'", collapse = ", "))
  }
  limit <- as.integer(limit)
  if (limit < 1) {
    api_stop(400L, "limit parameter must be >= 1")
  }
  logs <- logs_read_rds(api, user, job_id)
  levels <- vapply(logs, \(log) log$level, character(1))
  selection <- match(levels, level_list) <= match(level, level_list)
  list(level = level, logs = logs[selection], links = list())
}

# Retrieve results for job results
#' @export
job_get_results <- function(api, user, job_id) {
  jobs <- job_read_rds(api, user)
  # Check if the job_id exists in the jobs_list
  if (!(job_id %in% names(jobs))) {
    api_stop(404L, "Job not found")
  }
  job <- jobs[[job_id]]
  if (job$status == .job_status_error) {
    api_stop(424, "Job returned an error")
  }
  results_path <- file.path(job_get_dir(api, user, job_id))
  if (!dir.exists(results_path)) {
    api_stop(404L, "No results found")
  }
  if (job$status != "finished") {
    return(job_empty_collection(api, user, job))
  }
  jsonlite::read_json(file.path(results_path, "_collection.json"))
}
#' @export
job_empty_collection <- function(api, user, job) {
  collection <- list(
    `openeo:status` = job$status,
    type = "Collection",
    stac_version = "1.0.0",
    id = job$id,
    title = job$title,
    description = job$description,
    license = "various",
    extent = list(),
    links = list(),
    assets = list()
  )
  collection
}
job_info_check <- function(job_info) {
  if (!all(c("title", "description", "process") %in% names(job_info))) {
    api_stop(400L, "Invalid job data")
  }
}
