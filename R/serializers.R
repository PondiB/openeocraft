#' @importFrom plumber register_serializer
#' @importFrom plumber serializer_json
#' @importFrom plumber serializer_rds
NULL

#' Register custom plumber serializers for openEO result wrappers
#'
#' @description
#' Registers `"serialize_result"`, which dispatches on `val$data` via
#' `get_serializer()`. Synchronous `POST /result` uses `data_serializer()`
#' instead; keep both paths aligned when adding formats (see
#' `DEVELOPMENT.md`).
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
plumb_reg_serializers <- function() {
    # register serialize function
    plumber::register_serializer("serialize_result", function() {
        function(val, req, res, error_handler) {
            fn <- get_serializer(val)
            fn(val$data, req, res, error_handler)
        }
    })
}

#' Choose a plumber serializer for an openEO result object
#'
#' @param data S3 object carrying openEO format metadata (class determines JSON,
#'   RDS, GeoTIFF, etc.).
#'
#' @return A plumber serializer function
#'   (`function(val, req, res, errorHandler)`).
#'
#' @keywords internal
get_serializer <- function(data) {
    UseMethod("get_serializer", data)
}

#' @describeIn get_serializer JSON body via \code{plumber::serializer_json()}
#' @param data Result object with class `openeo_json`.
#' @export
get_serializer.openeo_json <- function(data) {
    plumber::serializer_json()
}

#' Read a result file path as raw bytes for binary serializers
#'
#' @param path Character path to a file on disk.
#' @return Raw vector of file contents.
#' @keywords internal
.serializer_read_file <- function(path) {
    path <- path[[1L]]
    if (!is.character(path) || !nzchar(path) || !file.exists(path)) {
        stop("Result file not found for serialization", call. = FALSE)
    }
    readBin(path, what = "raw", n = file.info(path)$size)
}

#' @describeIn get_serializer GeoTIFF bytes with \code{image/tiff} content type
#' @param data Result object with class `openeo_gtiff`.
#' @export
get_serializer.openeo_gtiff <- function(data) {
    plumber::serializer_content_type("image/tiff", .serializer_read_file)
}

#' @describeIn get_serializer NetCDF / octet-stream file body
#' @param data Result object with class `openeo_netcdf`.
#' @export
get_serializer.openeo_netcdf <- function(data) {
    plumber::serializer_content_type(
        "application/octet-stream",
        .serializer_read_file
    )
}

#' @describeIn get_serializer Tar archive with \code{application/x-tar}
#' @param data Result object with class `openeo_tar`.
#' @export
get_serializer.openeo_tar <- function(data) {
    plumber::serializer_content_type("application/x-tar", .serializer_read_file)
}

#' @describeIn get_serializer Native R RDS via \code{plumber::serializer_rds()}
#' @param data Result object with class `openeo_rds`.
#' @export
get_serializer.openeo_rds <- function(data) {
    plumber::serializer_rds(version = "3")
}
