# ---- Time coercion ----------------------------------------------------------
#
# The v5 readings endpoint accepts a window as ISO 8601 datetimes
# (`start_datetime`/`end_datetime`) OR Unix timestamps in seconds
# (`start_timestamp`/`end_timestamp`), but not both. These helpers turn the
# flexible R inputs users provide (Date, POSIXct, numeric epoch, or an ISO
# string) into the exact strings/integers the API expects, always normalised
# to UTC so behaviour does not depend on the user's locale or session timezone.

# Convert a single value to an ISO 8601 UTC string, e.g. "2026-02-18T00:00:00+00:00".
zc_to_iso8601 <- function(x, arg = "start") {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(paste0(format(x, "%Y-%m-%d"), "T00:00:00+00:00"))
  }
  if (is.numeric(x)) {
    return(zc_to_iso8601(zc_epoch_to_posixct(x), arg = arg))
  }
  if (is.character(x)) {
    # Trust an explicit string, but reject obvious non-datetimes early.
    if (length(x) != 1 || !nzchar(trimws(x))) {
      cli::cli_abort(
        "{.arg {arg}} must be a single non-empty datetime string.",
        class = "zentraR_invalid_request_error"
      )
    }
    return(trimws(x))
  }
  cli::cli_abort(
    c(
      "Can't interpret {.arg {arg}} as a datetime.",
      "i" = "Use a {.cls Date}, {.cls POSIXct}, numeric epoch seconds, or an ISO 8601 string."
    ),
    class = "zentraR_invalid_request_error"
  )
}

# Convert epoch seconds to POSIXct (UTC).
zc_epoch_to_posixct <- function(x) {
  as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
}

# Coerce a flexible value to integer epoch seconds (UTC).
zc_to_epoch <- function(x, arg = "timestamp") {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.numeric(x)) {
    return(as.integer(round(x)))
  }
  if (inherits(x, "POSIXt")) {
    return(as.integer(round(as.numeric(x))))
  }
  if (inherits(x, "Date")) {
    return(as.integer(as.numeric(as.POSIXct(x, tz = "UTC"))))
  }
  cli::cli_abort(
    "{.arg {arg}} must be numeric epoch seconds, a {.cls Date}, or a {.cls POSIXct}.",
    class = "zentraR_invalid_request_error"
  )
}

# Build the query parameters describing the requested time window. Accepts
# datetime-style inputs (start/end) or explicit epoch timestamps, but not both.
zc_window_query <- function(start = NULL, end = NULL,
                            start_timestamp = NULL, end_timestamp = NULL) {
  has_dt <- !is.null(start) || !is.null(end)
  has_ts <- !is.null(start_timestamp) || !is.null(end_timestamp)

  if (has_dt && has_ts) {
    cli::cli_abort(
      c(
        "Provide either {.arg start}/{.arg end} or {.arg start_timestamp}/{.arg end_timestamp}, not both.",
        "i" = "The v5 API accepts datetimes or Unix timestamps for the window, but not a mix."
      ),
      class = "zentraR_invalid_request_error"
    )
  }

  if (has_ts) {
    return(drop_null(list(
      start_timestamp = zc_to_epoch(start_timestamp, "start_timestamp"),
      end_timestamp = zc_to_epoch(end_timestamp, "end_timestamp")
    )))
  }

  drop_null(list(
    start_datetime = zc_to_iso8601(start, "start"),
    end_datetime = zc_to_iso8601(end, "end")
  ))
}
