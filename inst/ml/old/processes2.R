# TODO allow many files processes definition
#* @openeo-import math.R
#* @openeo-import data.R

.data_timeline <- function(data) {
  stars::st_get_dimension_values(data, "t")
}

.data_bands <- function(data) {
  bands <- "default"
  if ("bands" %in% base::names(stars::st_dimensions(data)))
    bands <- stars::st_get_dimension_values(data, "bands")
  bands
}

.data_get_time <- function(data, time) {
  i <- base::match(time, .data_timeline(data))
  if (base::any(base::is.na(i))) {
    time <- base::paste0(time[base::is.na(i)], collapse = ",")
    openeocraft::api_stop(404L, "Time(s) ", time, " not found")
  }
  if ("bands" %in% base::names(stars::st_dimensions(data)))
    return(data[,,,,i])
  data[,,,i]
}

.data_get_band <- function(data, band) {
  j <- base::match(band, .data_bands(data))
  if (base::any(base::is.na(j))) {
    band <- base::paste0(band[base::is.na(j)], collapse = ",")
    openeocraft::api_stop(404L, "Band(s) ", band, " not found")
  }
  dplyr::select(data, dplyr::all_of(j))
}

#* @openeo-process
load_collection <- function(id,
                            spatial_extent = NULL,
                            temporal_extent = NULL,
                            bands = NULL,
                            properties = NULL) {
  mock_img <- function(band, size, origin, res, times) {
    data <- terra::rast(
      nrows = size[[1]],
      ncols = size[[2]],
      nlyrs = base::length(times),
      xmin = origin[[1]],
      xmax = origin[[1]] + res[[1]] * size[[1]],
      ymin = origin[[2]],
      ymax = origin[[2]] + res[[2]] * size[[2]],
      crs = "WGS84",
      vals = matrix(stats::runif(size[[1]] * size[[2]] * base::length(times)),
                    ncol = base::length(times)),
      names = rep(band, base::length(times)),
      time = as.Date(times)
    )
    data
  }

  spatial_extent <- base::c(
    west = -45.0,
    east = -45.0 * 10 * 0.009,
    south = -10.0 * 10 * -0.009,
    north = -10.0
  )
  temporal_extent <- base::c(
    t0 = "2020-01-01",
    t1 = "2021-01-01"
  )
  bands <- base::c("B02", "B04", "B08")

  data <- terra::sds(
    mock_img(
      band = bands[[1]],
      size = base::c(10, 10),
      origin = base::c(-45.0, -10.0),
      res = base::c(0.009, 0.009),
      times = c("2020-01-01", "2021-01-01")
    ),
    mock_img(
      band = bands[[2]],
      size = base::c(10, 10),
      origin = base::c(-45.0, -10.0),
      res = base::c(0.009, 0.009),
      times = c("2020-01-01", "2021-01-01")
    ),
    mock_img(
      band = bands[[3]],
      size = base::c(10, 10),
      origin = base::c(-45.0, -10.0),
      res = base::c(0.009, 0.009),
      times = c("2020-01-01", "2021-01-01")
    )
  )
  terra::set.names(data, bands)
  # select by date:
  # data$B02[[terra::time(data$B02) >= as.Date("2020-07-01")]]
  data
}

#* @openeo-process
save_result <- function(data, format, options = NULL) {
  # format <- base::tolower(format)
  # supported_formats <- openeocraft::file_formats()
  # outputFormats <- supported_formats$output
  # if (!(format %in% base::tolower(base::names(outputFormats)))) {
  #   stop(base::paste("Format", format, "is not supported."))
  # }
  # # TODO: split data object into different files based on dates and
  # #  bands dimensions.
  # #  The number of output files will be: N(dates) * N(bands)
  # #  The format parameter just defines the file type to be saved in
  # #  this process.
  # api <- openeocraft::current_api()
  # user <- openeocraft::current_user()
  # job <- openeocraft::current_job()
  # req <- openeocraft::current_request()
  # job_dir <- openeocraft::job_get_dir(api, user, job$id)
  # host <- openeocraft::get_host(api, req)
  # times <- .data_timeline(data)
  # bands <- .data_bands(data)
  # # TODO implement generic function not relying on an specific data type
  # results <- base::list()
  # for (i in base::seq_along(times)) {
  #   assets_file <- base::lapply(base::seq_along(bands), \(j) {
  #     asset_file <- base::paste0(base::paste(bands[[j]], times[[i]], sep = "_"),
  #                                openeocraft::format_ext(format))
  #     stars::write_stars(.data_get_band(.data_get_time(data, times[[i]]), bands[[j]]),
  #                        base::file.path(job_dir, asset_file))
  #     asset_file
  #   })
  #   # TODO: implement asset tokens
  #   assets <- base::lapply(assets_file, \(asset_file) {
  #     asset_path <- base::file.path("/files/jobs", job$id, asset_file)
  #     base::list(
  #       href = openeocraft::make_url(
  #         host = host,
  #         asset_path,
  #         token = base64enc::base64encode(base::charToRaw(user))
  #       ),
  #       # TODO: implement format_content_type() function
  #       type = openeocraft::format_content_type(format),
  #       roles = base::list("data")
  #     )
  #   })
  #   base::names(assets) <- base::paste0(bands, "_", times[[i]],
  #                                       openeocraft::format_ext(format))
  #   results <- base::c(results, assets)
  # }
  # # TODO: where to out assets? links? assets?
  # collection <- openeocraft::job_empty_collection(api, user, job)
  # collection$assets <- results
  # jsonlite::write_json(
  #   x = collection,
  #   path = base::file.path(job_dir, "_collection.json"),
  #   auto_unbox = TRUE
  # )
  #
  # return(TRUE)
}

#* @openeo-process
reduce_dimension <- function(data, reducer, dimension, context = NULL) {
  reducer_fn <- function(data, context) {}
  base::body(reducer_fn) <- base::substitute(reducer)
  if (base::is.character(dimension))
    dimension <- base::match(dimension, base::dimnames(data))
  dims <- base::setdiff(base::seq_along(base::dimnames(data)), dimension)
  dimnames <- stars::st_dimensions(data)[[dimension]]$values
  stars::st_apply(data, dims, \(x) {
    base::names(x) <- dimnames
    reducer_fn(x, context = context)
  })
}

#* @openeo-process
multiply <- function(x, y) {
  x * y
}

#* @openeo-process
divide <- function(x, y) {
  x / y
}

#* @openeo-process
subtract <- function(x, y) {
  x - y
}

#* @openeo-process
add <- function(x, y) {
  x + y
}

#* @openeo-process
array_element <- function(data, index = NULL, label = NULL,
                          return_nodata = FALSE) {
  if (!base::is.null(index))
    return(data[[index]])
  if (!base::is.null(label))
    return(data[[label]])
  data
}

# TODO: implement extended annotations
#* @openeo-process sum
#*
#* This is the title
#*
#* This is the first line of the description
#* This is the second line of the description
#*
#* This is the third line of the description and the
#* previous line is a blank line.
#*
#* @param data string
#* @param ignore_nodata logical
#* @return data_cube
sum <- function(data, ignore_nodata = TRUE) {
  base::sum(base::unlist(data), na.rm = ignore_nodata)
}

#* @openeo-process
min <- function(data, ignore_nodata = TRUE) {
  base::min(base::unlist(data), na.rm = ignore_nodata)
}
