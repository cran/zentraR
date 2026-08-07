#' Reshape tidy readings into wide format
#'
#' Turns the tidy "long" output of [zc_get_readings()] (one row per
#' measurement) into a wide table with one row per timestamp and one column per
#' measurement. This is the familiar "spreadsheet" layout for a single device.
#'
#' Note that the wide layout drops the per-reading `unit` and `error_code`
#' columns (a single measurement column cannot carry them). Keep the tidy long
#' form if you need units or quality flags alongside each value. When a device
#' reports the same measurement on more than one port, the port number is
#' appended to the column name to keep them distinct.
#'
#' @param readings A data frame from [zc_get_readings()] (tidy long format).
#' @param values_from Name of the column to spread into the wide cells.
#'   Defaults to `"value"`.
#' @param names_sep Separator used when a measurement name is combined with a
#'   port number. Defaults to `"_"`.
#'
#' @return A [tibble][tibble::tibble] with `device_id`, `datetime`, `timestamp`,
#'   and one column per measurement.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- zc_get_readings("z6-00930", start = Sys.Date() - 2)
#' zc_pivot_wider(r)
#' }
zc_pivot_wider <- function(readings, values_from = "value", names_sep = "_") {
  required <- c("device_id", "datetime", "timestamp", "measurement", "port_num", values_from)
  missing_cols <- setdiff(required, names(readings))
  if (length(missing_cols)) {
    cli::cli_abort(
      "{.arg readings} is missing column{?s} {.field {missing_cols}}. Did it come from {.fn zc_get_readings}?",
      class = "zentraR_invalid_request_error"
    )
  }

  if (nrow(readings) == 0) {
    return(tibble::tibble(
      device_id = character(),
      datetime = .POSIXct(numeric(), tz = "UTC"),
      timestamp = numeric()
    ))
  }

  ports_per_measurement <- tapply(
    readings$port_num, readings$measurement,
    function(x) length(unique(x))
  )
  use_port <- any(ports_per_measurement > 1, na.rm = TRUE)

  col_name <- if (use_port) {
    paste0(readings$measurement, names_sep, "p", readings$port_num)
  } else {
    readings$measurement
  }

  long <- tibble::tibble(
    device_id = readings$device_id,
    datetime = readings$datetime,
    timestamp = readings$timestamp,
    measure_col = col_name,
    measure_val = readings[[values_from]]
  )

  tidyr::pivot_wider(
    long,
    names_from = "measure_col",
    values_from = "measure_val"
  )
}

#' ZENTRA device and sensor error codes
#'
#' Returns the lookup table of `error_code` values that can appear on readings.
#' A code of `0` means the reading is valid; any non-zero code flags a device
#' or sensor problem.
#'
#' @return A [tibble][tibble::tibble] with columns `error_code` and
#'   `error_label`.
#' @export
#'
#' @examples
#' zc_error_codes()
zc_error_codes <- function() {
  tibble::tibble(
    error_code = c(
      0L, 128L, 129L, 130L, 131L, 132L, 133L, 134L, 136L, 137L,
      138L, 139L, 140L, 141L, 142L, 143L, 235L
    ),
    error_label = c(
      "No error (valid reading)",
      "Sensor calibration problem",
      "Excitation under-voltage",
      "Value temporarily out of range",
      "Sensor type not supported",
      "Sensor communication error",
      "Sensor reporting an error",
      "No response from sensor",
      "Value outside expected range",
      "Invalid value",
      "Alpha value too low",
      "Radiance value(s) too low",
      "Calculation exceeds limits",
      "Calculation error",
      "Value exceeds model limits",
      "No data available for the requested time/measurement",
      "Erased flash memory"
    )
  )
}

#' Add human-readable error labels to readings
#'
#' Joins the [zc_error_codes()] table onto a readings data frame, adding an
#' `error_label` column so quality flags are easy to read and filter.
#'
#' @param readings A data frame from [zc_get_readings()] with an `error_code`
#'   column.
#'
#' @return `readings` with an added `error_label` character column. Unknown
#'   codes get an `"Unknown error code (<n>)"` label.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- zc_get_readings("z6-00930", start = Sys.Date() - 2)
#' zc_label_errors(r)
#' }
zc_label_errors <- function(readings) {
  if (!"error_code" %in% names(readings)) {
    cli::cli_abort(
      "{.arg readings} must have an {.field error_code} column.",
      class = "zentraR_invalid_request_error"
    )
  }
  lookup <- zc_error_codes()
  idx <- match(readings$error_code, lookup$error_code)
  label <- lookup$error_label[idx]
  unknown <- is.na(label) & !is.na(readings$error_code)
  label[unknown] <- sprintf("Unknown error code (%s)", readings$error_code[unknown])
  readings$error_label <- label
  readings
}
