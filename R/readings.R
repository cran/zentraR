#' Download sensor readings for a device
#'
#' Fetches time-series readings for a single device and returns them in a tidy
#' "long" data frame: one row per (port, measurement, timestamp). All pages are
#' retrieved automatically, and the request is paced to respect the API rate
#' limit.
#'
#' Specify the time window with `start`/`end` (any of a [Date], [POSIXct],
#' numeric epoch seconds, or an ISO 8601 string) **or** with `start_timestamp`/
#' `end_timestamp` (epoch seconds) - but not both. All datetimes are normalised
#' to UTC. If you omit the window, the API returns its default (most recent)
#' window.
#'
#' @param device_id Device identifier (string), e.g. `"z6-00930"` or
#'   `"A4100110"`. See [zc_list_devices()].
#' @param start,end Start/end of the window. Accepts a [Date], [POSIXct],
#'   numeric epoch seconds, or an ISO 8601 string. Interpreted as UTC when no
#'   timezone is present.
#' @param start_timestamp,end_timestamp Alternative window as Unix epoch seconds
#'   (UTC). Mutually exclusive with `start`/`end`.
#' @param direction Sort order by time: `"ascending"` (default) or
#'   `"descending"`.
#' @param units `"metric"` (default) or `"imperial"`.
#' @param max_pages Maximum number of pages (calendar-month windows) to fetch.
#'   Defaults to `Inf` (all available). Useful to cap very large backfills.
#' @param progress Show a live progress indicator while pages are fetched?
#'   `NULL` (default) shows one in interactive sessions; set `TRUE`/`FALSE` to
#'   force it, or set `options(zentraR.progress = FALSE)` to turn it off
#'   globally. During a rate-limit pause the request waits and the underlying
#'   HTTP client reports the wait.
#' @param key Optional API key. Defaults to the key set with [zc_set_key()] or
#'   the `ZENTRACLOUD_API_KEY` environment variable.
#'
#' @return A [tibble][tibble::tibble] with columns `device_id`, `datetime`
#'   ([POSIXct], UTC), `timestamp` (numeric epoch seconds), `port_num`,
#'   `sensor_name`, `measurement`, `value`, `unit`, and `error_code`
#'   (`0` = valid; see [zc_error_codes()]).
#' @export
#'
#' @examples
#' \dontrun{
#' zc_set_key("my-secret-key")
#'
#' # Last 7 days:
#' zc_get_readings("z6-00930", start = Sys.Date() - 7)
#'
#' # An explicit window:
#' zc_get_readings(
#'   "z6-00930",
#'   start = as.POSIXct("2026-05-01", tz = "UTC"),
#'   end   = as.POSIXct("2026-06-01", tz = "UTC")
#' )
#' }
zc_get_readings <- function(device_id,
                            start = NULL,
                            end = NULL,
                            start_timestamp = NULL,
                            end_timestamp = NULL,
                            direction = "ascending",
                            units = "metric",
                            max_pages = Inf,
                            progress = NULL,
                            key = NULL) {
  if (!is_string(device_id) || !nzchar(trimws(device_id))) {
    cli::cli_abort(
      "{.arg device_id} must be a single non-empty string.",
      class = "zentraR_invalid_request_error"
    )
  }
  device_id <- trimws(device_id)
  direction <- zc_match_arg(direction, c("ascending", "descending"))
  units <- zc_match_arg(units, c("metric", "imperial"))

  window <- zc_window_query(start, end, start_timestamp, end_timestamp)
  zc_readings_fetch(
    device_id = device_id, window = window, direction = direction,
    units = units, max_pages = max_pages, key = key, progress = progress
  )
}

# Internal page iterator shared by zc_get_readings() and zc_sync().
#
# Walks the `next_token` pagination, building a tidy tibble for each page. If
# `on_page` is supplied it is called with each page's tibble (used by zc_sync()
# to persist progressively, so an interruption never loses committed pages) and
# the function returns invisibly. Otherwise all pages are combined and returned.
zc_readings_fetch <- function(device_id, window, direction = "ascending",
                              units = "metric", max_pages = Inf, key = NULL,
                              on_page = NULL, progress = NULL) {
  show_pb <- zc_use_progress(progress, active_default = is.null(on_page) && interactive())

  metadata <- NULL
  next_token <- NULL
  pages_fetched <- 0L
  n_readings <- 0L
  window_label <- ""
  collected <- list()

  if (show_pb) {
    cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} Fetching {.val {device_id}}: ",
        "{pages_fetched} page{?s}, {n_readings} reading{?s}{window_label}"
      ),
      format_done = paste0(
        "{cli::col_green(cli::symbol$tick)} Fetched {n_readings} reading{?s} ",
        "for {.val {device_id}} in {pages_fetched} page{?s}."
      ),
      clear = FALSE,
      .envir = environment()
    )
  }

  repeat {
    query <- c(window, drop_null(list(
      direction = direction,
      units = units,
      next_token = next_token
    )))
    body <- zc_get_json(c("devices", device_id, "data"), query = query, key = key)

    metadata <- metadata %||% body$metadata
    resolved_id <- metadata$device_id %||% device_id
    vals <- body$values %||% list()

    # Parsing a large page (~100k records) is the slow, R-bound step. Build it in
    # chunks and tick the spinner between them, so the reading count climbs live
    # and it is visibly working instead of frozen. Small pages build in one go.
    if (show_pb && length(vals) > 20000L) {
      groups <- split(seq_along(vals), ceiling(seq_along(vals) / 10000L))
      parts <- vector("list", length(groups))
      for (j in seq_along(groups)) {
        parts[[j]] <- build_readings_tibble(vals[groups[[j]]], resolved_id)
        n_readings <- n_readings + length(groups[[j]])
        cli::cli_progress_update()
      }
      page <- vctrs::vec_rbind(!!!parts)
    } else {
      page <- build_readings_tibble(vals, device_id = resolved_id)
      n_readings <- n_readings + nrow(page)
    }

    if (!is.null(on_page)) {
      on_page(page)
    } else {
      collected[[length(collected) + 1L]] <- page
    }

    pages_fetched <- pages_fetched + 1L
    win_start <- body$pagination$start_datetime
    if (!is.null(win_start) && is.character(win_start)) {
      window_label <- sprintf(" (%s)", substr(win_start, 1, 7))
    }
    if (show_pb) {
      cli::cli_progress_update()
    }

    next_token <- body$pagination$next_token
    if (is.null(next_token) || pages_fetched >= max_pages) {
      break
    }
  }

  if (show_pb) {
    cli::cli_progress_done()
  }

  if (!is.null(on_page)) {
    return(invisible(NULL))
  }
  if (length(collected) == 0) {
    return(empty_readings())
  }
  vctrs::vec_rbind(!!!collected)
}

# Empty tidy-long readings tibble with the canonical column types.
empty_readings <- function() {
  tibble::tibble(
    device_id = character(),
    datetime = .POSIXct(numeric(), tz = "UTC"),
    timestamp = numeric(),
    port_num = integer(),
    sensor_name = character(),
    measurement = character(),
    value = numeric(),
    unit = character(),
    error_code = integer()
  )
}

build_readings_tibble <- function(values, device_id) {
  if (length(values) == 0) {
    out <- empty_readings()
    return(out)
  }
  ts <- vapply(values, p_dbl, double(1), "timestamp")
  tibble::tibble(
    device_id = rep(device_id, length(values)),
    datetime = zc_epoch_to_posixct(ts),
    timestamp = ts,
    port_num = vapply(values, p_int, integer(1), "port_num"),
    sensor_name = vapply(values, p_chr, character(1), "sensor_name"),
    measurement = vapply(values, p_chr, character(1), "measurement"),
    value = vapply(values, p_dbl, double(1), "value"),
    unit = vapply(values, p_chr, character(1), "unit"),
    error_code = vapply(values, p_int, integer(1), "error_code")
  )
}
