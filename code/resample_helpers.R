standardize_data_new = function(data, subset = TRUE){
  HEADER_TIMESTAMP = TIME = HEADER_TIME_STAMP = X = Y = Z = NULL
  rm(list = c("HEADER_TIMESTAMP", "HEADER_TIME_STAMP", "X",
              "Y", "Z", "TIME"))
  if (is.matrix(data)) {
    if (is.numeric(data)) {
      stopifnot(ncol(data) == 3)
      data = as.data.frame(data)
      colnames(data) = c("X", "Y", "Z")
    }
    else {
      stop("data is a matrix and cannot be coerced to necessary structure")
    }
  }
  colnames(data) = toupper(colnames(data))
  cn = colnames(data)
  if ("TIME" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    data = data %>% dplyr::rename(HEADER_TIME_STAMP = TIME)
  }
  if ("TM_DTTM" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    data = data %>% dplyr::rename(HEADER_TIME_STAMP = TM_DTTM)
  }
  if ("HEADER_TIMESTAMP" %in% cn && !"HEADER_TIME_STAMP" %in%
      cn) {
    data = data %>% dplyr::rename(HEADER_TIME_STAMP = HEADER_TIMESTAMP)
  }
  if ("HEADER_TIME_STAMP" %in% colnames(data)) {
    if (is.unsorted(data$HEADER_TIME_STAMP)) {
      stop("Time in data must be sorted before running!")
    }
  }
  if (subset) {
    data = data %>% dplyr::select(dplyr::any_of("HEADER_TIME_STAMP"),
                                  X, Y, Z)
  }
  stopifnot(all(c("X", "Y", "Z") %in% colnames(data)))
  data
}



run_resample = function(
    timestamp, x, y, z,
    time_interp,
    orig_tz,
    method = c("linear", "constant",
               "fmm", "periodic", "natural", "monoH.FC", "hyman"),
    ...) {
  method = match.arg(method)
  func = switch(
    method,
    linear = stats::approx,
    constant = stats::approx,
    fmm = stats::spline,
    periodic = stats::spline,
    natural = stats::spline,
    monoH.FC = stats::spline,
    hyman = stats::spline)
  x_out = func(x = timestamp,
               xout = time_interp,
               method = method,
               y = x,
               ...)$y
  rm(x)
  y_out = func(x = timestamp,
               xout = time_interp,
               method = method,
               y = y,
               ...)$y
  rm(y)
  z_out = func(x = timestamp,
               xout = time_interp,
               method = method,
               y = z,
               ...)$y
  rm(z)
  rm(timestamp)


  time_interp = round(time_interp, 3)
  time_interp = as.POSIXct(
    time_interp,
    tz = orig_tz,
    origin = lubridate::origin)

  out = data.frame(
    HEADER_TIMESTAMP = time_interp,
    X = x_out,
    Y = y_out,
    Z = z_out
  )
  out
}
#' Resample 3-axial input signal to a specific sampling rate
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `HEADER_TIME_STAMP`), and `X`, `Y`, `Z`
#' @param sample_rate sampling frequency, coercible to an integer.
#' This is the sampling rate you're sampling the data *into*.
#' @param ... additional arguments to pass to [stats::approx()] or
#' [stats::spline]
#' @param method method for interpolation. Options are
#' `"linear"/"constant"`, which uses `stats::approx`, or one of
#' `"fmm", "periodic", "natural", "monoH.FC", "hyman"`, which uses
#' `stats::spline`
#' @return A `data.frame`/`tibble` of `HEADER_TIME_STAMP` and `X`, `Y`, `Z`.
#' @export
#'
#' @examples
#' options(digits.secs = 3)
#' csv_file = system.file("test_data_bout.csv", package = "walking")
#' if (requireNamespace("readr", quietly = TRUE)) {
#'   x = readr::read_csv(csv_file, guess_max = Inf)
#'   colnames(x)[colnames(x) == "UTC time"] = "time"
#'
#'   res = resample_accel_data(data = x, sample_rate = 80)
#'   res = resample_accel_data(data = x, sample_rate = 100)
#'   res = resample_accel_data(data = x, sample_rate = 1)
#'   res = resample_accel_data_to_time(
#'     data = x,
#'     times = lubridate::floor_date(x$time, unit = "1 sec"),
#'   )
#'   res_nat = resample_accel_data_to_time(
#'     data = x,
#'     times = lubridate::floor_date(x$time, unit = "1 sec"),
#'     method = "natural"
#'   )
#' }
resample_accel_data = function(
    data,
    sample_rate,
    method = "linear",
    ...
) {
  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  sample_rate = as.integer(sample_rate)

  data = standardize_data_new(data)
  orig_tz = lubridate::tz(data$HEADER_TIME_STAMP)
  timestamp = as.numeric(data$HEADER_TIME_STAMP)
  x = data[["X"]]
  y = data[["Y"]]
  z = data[["Z"]]
  is_data_tibble = inherits(data, "tbl_df")
  rm(data)

  time_interp = timestamp - timestamp[1]
  time_interp = seq(time_interp[1],
                    time_interp[length(time_interp)],
                    (1/sample_rate)
  )
  time_interp = time_interp + timestamp[1]

  out = run_resample(
    timestamp = timestamp,
    x = x,
    y = y,
    z = z,
    method = method,
    time_interp = time_interp,
    orig_tz = orig_tz,
    ...)
  if (is_data_tibble) {
    out = dplyr::as_tibble(out)
  }
  return(out)
}


