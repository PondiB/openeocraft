data_serializer <- function(x, res) {
  UseMethod("data_serializer", x)
}
#' @export
data_serializer.openeo_json <- function(x, res) {
  res$setHeader("Content-Type", "application/json")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}
#' @export
data_serializer.openeo_gtiff <- function(x, res) {
  res$setHeader("Content-Type", "image/tiff")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}
#' @export
data_serializer.openeo_netcdf <- function(x, res) {
  res$setHeader("Content-Type", "application/octet-stream")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}

#' @export
data_serializer.openeo_rds <- function(x, res) {
  res$setHeader("Content-Type", "application/rds")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}

#' @export
data_serializer.openeo_tar <- function(x, res) {
  res$setHeader("Content-Type", "application/x-tar")
  res$body <- readBin(x$data, what = "raw", n = file.info(x$data)$size)
  res
}

#' @export
format_ext <- function(format) {
  switch(format,
         gtiff = ".tif",
         netcdf = ".nc",
         rds = ".rds",
         json = ".json"
  )
}
#' @export
ext_format <- function(filename) {
  ext <- gsub("\\.([^.]+)$", "\\1", filename)
  switch(ext,
         tif = "gtiff",
         nc = "netcdf",
         rds = "rds",
         json = "json"
  )
}
#' @export
ext_content_type <- function(filename) {
  ext <- gsub(".*\\.([^.]+)$", "\\1", filename)
  switch(ext,
         tif = "image/tiff",
         nc = "application/octet-stream",
         rds = "application/rds",
         json = "application/json",
  )
}
#' @export
format_content_type <- function(format) {
  format <- tolower(format)
  switch(format,
         gtiff = "image/tiff",
         netcdf = "application/octet-stream",
         rds = "application/rds",
         json = "application/json"
  )
}

