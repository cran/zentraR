# ============================================================================
# Pluggable local storage for readings.
#
# A "store" is a small S3 object describing where synced readings live. All
# stores implement three generics:
#   * zc_store_read(store, device_id = NULL) -> tidy readings tibble
#   * zc_store_write(store, readings)        -> append + de-duplicate
#   * zc_store_last_time(store, device_id)   -> latest stored epoch, or NA
#
# zc_sync() is written against these generics only, so new backends (e.g. a
# database) can be added without touching the sync engine. Passing
# `store = NULL` to zc_sync() means "return only" - fetch and hand back data
# without persisting it.
# ============================================================================

# Canonical column order for stored readings.
zc_reading_cols <- c(
  "device_id", "datetime", "timestamp", "port_num",
  "sensor_name", "measurement", "value", "unit", "error_code"
)

# Coerce a readings data frame to the canonical schema/order used by stores.
zc_standardize_readings <- function(readings) {
  missing_cols <- setdiff(zc_reading_cols, names(readings))
  if (length(missing_cols)) {
    cli::cli_abort(
      "Readings are missing column{?s} {.field {missing_cols}}. Expected output of {.fn zc_get_readings}.",
      class = "zentraR_invalid_request_error"
    )
  }
  readings[zc_reading_cols]
}

# Key used to de-duplicate: a reading is unique per device/port/measurement/time.
zc_dedup_readings <- function(readings) {
  if (nrow(readings) == 0) {
    return(readings)
  }
  key <- paste(
    readings$device_id, readings$port_num,
    readings$measurement, readings$timestamp,
    sep = "\r"
  )
  keep <- !duplicated(key)
  readings <- readings[keep, , drop = FALSE]
  readings[order(readings$device_id, readings$timestamp), , drop = FALSE]
}

# Turn a device id into a safe file stem.
zc_safe_stem <- function(device_id) {
  gsub("[^A-Za-z0-9._-]", "_", device_id)
}

# ---- Generics ---------------------------------------------------------------

#' Read, write, and inspect a zentraR store
#'
#' Low-level accessors for a store created by [zc_store_rds()], [zc_store_csv()],
#' or [zc_store_memory()]. Most users interact with stores only through
#' [zc_sync()], but these are exported for direct access to the stored data.
#'
#' @param store A store object.
#' @param device_id Optional device id. For `zc_store_read()`, `NULL` reads all
#'   devices. For `zc_store_last_time()` a single device id is required.
#' @param readings A tidy readings data frame (as returned by
#'   [zc_get_readings()]).
#'
#' @return `zc_store_read()` returns a tidy readings [tibble][tibble::tibble].
#'   `zc_store_write()` invisibly returns the store. `zc_store_last_time()`
#'   returns the latest stored timestamp (numeric epoch seconds) for the device,
#'   or `NA_real_` if the device has no stored data.
#' @name zc_store_access
#' @examples
#' \dontrun{
#' store <- zc_store_csv("data")
#' zc_store_read(store)
#' zc_store_last_time(store, "z6-00930")
#' }
NULL

#' @rdname zc_store_access
#' @export
zc_store_read <- function(store, device_id = NULL) UseMethod("zc_store_read")

#' @rdname zc_store_access
#' @export
zc_store_write <- function(store, readings) UseMethod("zc_store_write")

#' @rdname zc_store_access
#' @export
zc_store_last_time <- function(store, device_id) UseMethod("zc_store_last_time")

#' @export
zc_store_read.default <- function(store, device_id = NULL) {
  cli::cli_abort("{.arg store} is not a zentraR store.", class = "zentraR_invalid_request_error")
}
#' @export
zc_store_write.default <- function(store, readings) {
  cli::cli_abort("{.arg store} is not a zentraR store.", class = "zentraR_invalid_request_error")
}
#' @export
zc_store_last_time.default <- function(store, device_id) {
  cli::cli_abort("{.arg store} is not a zentraR store.", class = "zentraR_invalid_request_error")
}

# Shared: latest timestamp from a device's stored readings.
zc_last_time_from <- function(readings) {
  if (nrow(readings) == 0 || all(is.na(readings$timestamp))) {
    return(NA_real_)
  }
  max(readings$timestamp, na.rm = TRUE)
}

# ============================================================================
# RDS store: one <device>.rds per device under a directory.
# ============================================================================

#' Store readings as RDS files
#'
#' Creates a store that persists readings as native R `.rds` files (one per
#' device) inside `path`. This is the most faithful option for RStudio project
#' workflows: types (including `POSIXct` datetimes) round-trip exactly.
#'
#' @param path Directory to hold the `.rds` files. Created if it does not exist.
#'
#' @return A store object for use with [zc_sync()] and [zc_store_read()].
#' @export
#'
#' @examples
#' \dontrun{
#' store <- zc_store_rds("data/zentra")
#' zc_sync("z6-00930", store = store, start = Sys.Date() - 30)
#' }
zc_store_rds <- function(path) {
  if (!is_string(path)) {
    cli::cli_abort("{.arg path} must be a single directory path.")
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  structure(list(path = path), class = c("zc_store_rds", "zc_store"))
}

rds_file <- function(store, device_id) {
  file.path(store$path, paste0(zc_safe_stem(device_id), ".rds"))
}

#' @export
zc_store_read.zc_store_rds <- function(store, device_id = NULL) {
  if (!is.null(device_id)) {
    f <- rds_file(store, device_id)
    if (!file.exists(f)) {
      return(empty_readings())
    }
    return(tibble::as_tibble(readRDS(f)))
  }
  files <- list.files(store$path, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(empty_readings())
  }
  parts <- lapply(files, function(f) tibble::as_tibble(readRDS(f)))
  zc_dedup_readings(vctrs::vec_rbind(!!!parts))
}

#' @export
zc_store_write.zc_store_rds <- function(store, readings) {
  readings <- zc_standardize_readings(readings)
  for (dev in unique(readings$device_id)) {
    new <- readings[readings$device_id == dev, , drop = FALSE]
    existing <- zc_store_read(store, dev)
    combined <- zc_dedup_readings(vctrs::vec_rbind(existing, new))
    saveRDS(combined, rds_file(store, dev))
  }
  invisible(store)
}

#' @export
zc_store_last_time.zc_store_rds <- function(store, device_id) {
  zc_last_time_from(zc_store_read(store, device_id))
}

# ============================================================================
# CSV store: one <device>.csv per device under a directory.
# ============================================================================

#' Store readings as CSV files
#'
#' Creates a store that persists readings as plain CSV files (one per device)
#' inside `path`. The most accessible option for collaborators who open data in
#' a spreadsheet or a tool other than R. Datetimes are written as ISO 8601 UTC
#' strings and re-parsed on read.
#'
#' @param path Directory to hold the `.csv` files. Created if it does not exist.
#'
#' @return A store object for use with [zc_sync()] and [zc_store_read()].
#' @export
#'
#' @examples
#' \dontrun{
#' store <- zc_store_csv("data/zentra")
#' zc_sync("z6-00930", store = store, start = Sys.Date() - 30)
#' }
zc_store_csv <- function(path) {
  if (!is_string(path)) {
    cli::cli_abort("{.arg path} must be a single directory path.")
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  structure(list(path = path), class = c("zc_store_csv", "zc_store"))
}

csv_file <- function(store, device_id) {
  file.path(store$path, paste0(zc_safe_stem(device_id), ".csv"))
}

read_csv_file <- function(f) {
  raw <- utils::read.csv(
    f,
    colClasses = c(
      device_id = "character", datetime = "character", timestamp = "numeric",
      port_num = "integer", sensor_name = "character", measurement = "character",
      value = "numeric", unit = "character", error_code = "integer"
    ),
    stringsAsFactors = FALSE
  )
  # Regenerate datetime from the epoch timestamp for a stable POSIXct type.
  tibble::tibble(
    device_id = raw$device_id,
    datetime = zc_epoch_to_posixct(raw$timestamp),
    timestamp = raw$timestamp,
    port_num = raw$port_num,
    sensor_name = raw$sensor_name,
    measurement = raw$measurement,
    value = raw$value,
    unit = raw$unit,
    error_code = raw$error_code
  )
}

#' @export
zc_store_read.zc_store_csv <- function(store, device_id = NULL) {
  if (!is.null(device_id)) {
    f <- csv_file(store, device_id)
    if (!file.exists(f)) {
      return(empty_readings())
    }
    return(read_csv_file(f))
  }
  files <- list.files(store$path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    return(empty_readings())
  }
  parts <- lapply(files, read_csv_file)
  zc_dedup_readings(vctrs::vec_rbind(!!!parts))
}

#' @export
zc_store_write.zc_store_csv <- function(store, readings) {
  readings <- zc_standardize_readings(readings)
  for (dev in unique(readings$device_id)) {
    new <- readings[readings$device_id == dev, , drop = FALSE]
    existing <- zc_store_read(store, dev)
    combined <- zc_dedup_readings(vctrs::vec_rbind(existing, new))
    to_write <- combined
    to_write$datetime <- format(to_write$datetime, "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC")
    utils::write.csv(to_write, csv_file(store, dev), row.names = FALSE)
  }
  invisible(store)
}

#' @export
zc_store_last_time.zc_store_csv <- function(store, device_id) {
  zc_last_time_from(zc_store_read(store, device_id))
}

# ============================================================================
# Memory store: keeps readings in an R environment for the current session.
# Handy for experimentation and tests; not persisted to disk.
# ============================================================================

#' Store readings in memory (session only)
#'
#' Creates a store that keeps readings in memory for the current R session. Data
#' is not written to disk. Useful for experimentation or when another process
#' (for example a database loader) will consume the returned data.
#'
#' @return A store object for use with [zc_sync()] and [zc_store_read()].
#' @export
zc_store_memory <- function() {
  env <- new.env(parent = emptyenv())
  env$data <- empty_readings()
  structure(list(env = env), class = c("zc_store_memory", "zc_store"))
}

#' @export
zc_store_read.zc_store_memory <- function(store, device_id = NULL) {
  data <- store$env$data
  if (is.null(device_id)) {
    return(data)
  }
  data[data$device_id == device_id, , drop = FALSE]
}

#' @export
zc_store_write.zc_store_memory <- function(store, readings) {
  readings <- zc_standardize_readings(readings)
  store$env$data <- zc_dedup_readings(vctrs::vec_rbind(store$env$data, readings))
  invisible(store)
}

#' @export
zc_store_last_time.zc_store_memory <- function(store, device_id) {
  zc_last_time_from(zc_store_read(store, device_id))
}
