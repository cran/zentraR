#' Incrementally fetch and store readings
#'
#' `zc_sync()` is the workhorse for keeping a local record up to date. For each
#' device it looks at what you already have, fetches only the readings newer
#' than that, and appends them to your chosen store - so you can run it on a
#' schedule (weekly, daily, or on file open) and always pull just the new data.
#'
#' The window for each device is chosen automatically:
#' * If the store already has readings for the device, fetching resumes just
#'   after the latest stored timestamp.
#' * Otherwise, if you pass `start`, that is used.
#' * Otherwise, the device's first-ever measurement is used (a full backfill).
#'
#' Pages are written to the store as they arrive, so an interruption (network
#' drop, rate limit, cancelled run) never loses the pages already committed -
#' the next run simply resumes where it left off.
#'
#' @param device_id Device id, or a character vector of device ids. `NULL`
#'   (default) discovers and syncs every device your key can access.
#' @param store Where to persist readings: a store from [zc_store_rds()],
#'   [zc_store_csv()], or [zc_store_memory()]. Pass `NULL` (default) for
#'   "return only" - readings are fetched and returned but not stored (useful
#'   when you load them into your own database).
#' @param start Optional start of the window for devices with no stored data
#'   yet (a [Date], [POSIXct], epoch seconds, or ISO 8601 string). Ignored for
#'   devices that already have stored readings.
#' @param end Optional end of the window. Defaults to now (the API's latest).
#' @param units `"metric"` (default) or `"imperial"`.
#' @param direction Fetch order: `"ascending"` (default) or `"descending"`.
#' @param max_pages Maximum pages (calendar-month windows) to fetch per device.
#'   Defaults to `Inf`.
#' @param max_active Maximum number of devices to fetch concurrently. Defaults to
#'   `1` (sequential). Because the readings rate limit is enforced *per device*,
#'   values above `1` fetch several devices at once, which can be dramatically
#'   faster for multi-device syncs. Ignored when syncing a single device.
#' @param quiet If `TRUE`, suppress progress messages. Defaults to `FALSE`.
#' @param progress Show a live per-device progress indicator while fetching?
#'   `NULL` (default) shows one in interactive sessions (unless `quiet`); set
#'   `TRUE`/`FALSE` to force it, or use `options(zentraR.progress = FALSE)`.
#' @param key Optional API key. Defaults to the key set with [zc_set_key()] or
#'   the `ZENTRACLOUD_API_KEY` environment variable.
#'
#' @return If `store` is `NULL`, a tidy readings [tibble][tibble::tibble] (the
#'   combined new readings across devices). Otherwise, a summary tibble with one
#'   row per device: `device_id`, `rows_added`, `last_time` (latest stored
#'   [POSIXct]), `status` (`"ok"` or `"error"`), and `message`.
#' @export
#'
#' @examples
#' \dontrun{
#' zc_set_key("my-secret-key")
#' store <- zc_store_csv("data/zentra")
#'
#' # First run backfills from each device's first measurement (or `start`):
#' zc_sync("z6-00930", store = store, start = Sys.Date() - 30)
#'
#' # Later runs fetch only what is new:
#' zc_sync("z6-00930", store = store)
#'
#' # Sync every accessible device:
#' zc_sync(store = store)
#'
#' # Return-only (no persistence), e.g. to load into your own database:
#' new_data <- zc_sync("z6-00930", start = Sys.Date() - 7)
#' }
zc_sync <- function(device_id = NULL,
                    store = NULL,
                    start = NULL,
                    end = NULL,
                    units = "metric",
                    direction = "ascending",
                    max_pages = Inf,
                    max_active = 1L,
                    quiet = FALSE,
                    progress = NULL,
                    key = NULL) {
  units <- zc_match_arg(units, c("metric", "imperial"))
  direction <- zc_match_arg(direction, c("ascending", "descending"))
  if (!is.null(store) && !inherits(store, "zc_store")) {
    cli::cli_abort(
      "{.arg store} must be a store (see {.fn zc_store_rds}, {.fn zc_store_csv}, {.fn zc_store_memory}) or {.code NULL}.",
      class = "zentraR_invalid_request_error"
    )
  }
  max_active <- suppressWarnings(as.integer(max_active))
  if (length(max_active) != 1 || is.na(max_active) || max_active < 1L) {
    cli::cli_abort("{.arg max_active} must be a positive whole number.", class = "zentraR_invalid_request_error")
  }
  show_progress <- zc_use_progress(progress, active_default = !quiet && interactive())

  # Device metadata is fetched lazily and only once, to support discovery and
  # the first-measurement backfill fallback.
  meta_cache <- new.env(parent = emptyenv())
  get_meta <- function() {
    if (is.null(meta_cache$df)) {
      meta_cache$df <- zc_list_devices(expand = "max_min_timestamp", key = key)
    }
    meta_cache$df
  }

  if (is.null(device_id)) {
    device_id <- get_meta()$device_id
    if (length(device_id) == 0) {
      if (!quiet) cli::cli_warn("No accessible devices found to sync.")
      return(zc_sync_summary(list()))
    }
  } else {
    device_id <- as.character(device_id)
  }

  results <- if (max_active > 1L && length(device_id) > 1L) {
    zc_sync_parallel(device_id, store, start, end, units, direction, max_pages, key, get_meta, max_active, quiet, show_progress)
  } else {
    lapply(device_id, function(dev) {
      zc_sync_one(dev, store, start, end, units, direction, max_pages, key, get_meta, quiet, show_progress)
    })
  }

  if (is.null(store)) {
    datas <- Filter(Negate(is.null), lapply(results, `[[`, "data"))
    out <- if (length(datas)) vctrs::vec_rbind(!!!datas) else empty_readings()
    if (!quiet) zc_sync_report(results)
    return(out)
  }

  summary <- zc_sync_summary(results)
  if (!quiet) zc_sync_report(results)
  summary
}

# Sync a single device. Never throws: a per-device failure is captured and
# reported in the returned status so a multi-device sync keeps going.
zc_sync_one <- function(dev, store, start, end, units, direction, max_pages, key, get_meta, quiet, show_progress = FALSE) {
  last <- if (!is.null(store)) {
    tryCatch(zc_store_last_time(store, dev), error = function(e) NA_real_)
  } else {
    NA_real_
  }
  from <- zc_sync_from(dev, last, start, get_meta)

  # When the live spinner is on it already names the device, so skip this line.
  if (!quiet && !show_progress) {
    since <- if (!is.na(last)) {
      paste0(" (new since ", format(zc_epoch_to_posixct(last), "%Y-%m-%d %H:%M UTC"), ")")
    } else {
      ""
    }
    cli::cli_inform("Syncing {.val {dev}}{since}...")
  }

  added <- 0L
  max_ts <- if (!is.na(last)) last else NA_real_
  pages_data <- list()

  on_page <- function(page) {
    if (nrow(page) == 0) {
      return(invisible())
    }
    if (!is.null(store)) {
      zc_store_write(store, page)
    } else {
      pages_data[[length(pages_data) + 1L]] <<- page
    }
    added <<- added + nrow(page)
    m <- suppressWarnings(max(page$timestamp, na.rm = TRUE))
    if (is.finite(m) && (is.na(max_ts) || m > max_ts)) {
      max_ts <<- m
    }
    invisible()
  }

  res <- tryCatch(
    {
      window <- zc_window_query(start = from, end = end)
      zc_readings_fetch(dev, window, direction, units, max_pages, key,
                        on_page = on_page, progress = show_progress)
      list(status = "ok", message = NA_character_)
    },
    error = function(e) {
      if (!quiet) cli::cli_warn(c("x" = "Failed to sync {.val {dev}}: {conditionMessage(e)}"))
      list(status = "error", message = conditionMessage(e))
    }
  )

  data <- if (is.null(store) && length(pages_data)) {
    vctrs::vec_rbind(!!!pages_data)
  } else {
    NULL
  }

  list(
    device_id = dev,
    rows_added = as.integer(added),
    last_time = if (is.na(max_ts)) NA_real_ else max_ts,
    status = res$status,
    message = res$message,
    data = data
  )
}

# Decide the fetch start for a device.
zc_sync_from <- function(dev, last, start, get_meta) {
  if (!is.na(last)) {
    return(last + 1) # resume just after the latest stored reading
  }
  if (!is.null(start)) {
    return(start)
  }
  md <- get_meta()
  if ("first_measurement" %in% names(md)) {
    row <- md[md$device_id == dev, , drop = FALSE]
    if (nrow(row) >= 1 && !is.na(row$first_measurement[[1]])) {
      return(row$first_measurement[[1]])
    }
  }
  NULL # let the API return its default window
}

zc_sync_summary <- function(results) {
  if (length(results) == 0) {
    return(tibble::tibble(
      device_id = character(),
      rows_added = integer(),
      last_time = .POSIXct(numeric(), tz = "UTC"),
      status = character(),
      message = character()
    ))
  }
  tibble::tibble(
    device_id = vapply(results, `[[`, character(1), "device_id"),
    rows_added = vapply(results, `[[`, integer(1), "rows_added"),
    last_time = zc_epoch_to_posixct(vapply(results, `[[`, double(1), "last_time")),
    status = vapply(results, `[[`, character(1), "status"),
    message = vapply(results, `[[`, character(1), "message")
  )
}

zc_sync_report <- function(results) {
  total <- sum(vapply(results, `[[`, integer(1), "rows_added"))
  n_ok <- sum(vapply(results, function(r) r$status == "ok", logical(1)))
  n_err <- length(results) - n_ok
  msg <- "Synced {n_ok} device{?s}, {total} reading{?s} fetched."
  if (n_err > 0) {
    cli::cli_warn(c("!" = paste(msg, "{n_err} device{?s} failed."), "i" = "See the {.field status} column."))
  } else {
    cli::cli_inform(c("v" = msg))
  }
  invisible()
}
