# ============================================================================
# Parallel multi-device sync.
#
# The v5 readings rate limit is enforced PER DEVICE (bucket key
# `zc_public.device_id.<id>`), so fetching several devices at once is safe —
# each device has its own independent 5-burst / 1-per-minute budget. This
# orchestrator advances every device's pagination together in rounds: each
# round issues one page-request per ready device concurrently via
# httr2::req_perform_parallel(), then advances each device's `next_token`.
#
# req_perform_parallel() does not honour retry policies, so we handle
# rate-limit backoff here: a device that returns 429 is parked until the
# server's advertised next-allowed time and retried in a later round, while the
# other devices keep going.
# ============================================================================

zc_sync_parallel <- function(device_id, store, start, end, units, direction,
                             max_pages, key, get_meta, max_active, quiet, show_progress) {
  # Per-device state.
  st <- list()
  for (dev in device_id) {
    last <- if (!is.null(store)) {
      tryCatch(zc_store_last_time(store, dev), error = function(e) NA_real_)
    } else {
      NA_real_
    }
    from <- zc_sync_from(dev, last, start, get_meta)
    st[[dev]] <- list(
      window_query = zc_window_query(start = from, end = end),
      next_token = NULL, done = FALSE, errored = FALSE, ready_at = 0,
      added = 0L, max_ts = if (is.na(last)) NA_real_ else last, pages = 0L,
      message = NA_character_, data = list(), resolved_id = NULL
    )
  }

  n_dev <- length(device_id)
  n_done <- 0L
  n_readings <- 0L
  if (show_progress) {
    cli::cli_progress_bar(
      format = "{cli::pb_spin} Syncing {n_dev} device{?s}: {n_done}/{n_dev} done, {n_readings} reading{?s}",
      clear = TRUE, .envir = environment()
    )
  }

  repeat {
    active <- device_id[vapply(device_id, function(d) !st[[d]]$done && !st[[d]]$errored, logical(1))]
    if (length(active) == 0) {
      break
    }
    now <- as.numeric(Sys.time())
    ready <- active[vapply(active, function(d) st[[d]]$ready_at <= now, logical(1))]
    if (length(ready) == 0) {
      # Every remaining device is in rate-limit backoff; wait for the soonest.
      soonest <- min(vapply(active, function(d) st[[d]]$ready_at, numeric(1)))
      Sys.sleep(max(0, min(soonest - now, 300)))
      next
    }

    reqs <- lapply(ready, function(d) {
      s <- st[[d]]
      query <- c(s$window_query, drop_null(list(
        direction = direction, units = units, next_token = s$next_token
      )))
      zc_req(c("devices", d, "data"), query = query, key = key)
    })
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue", max_active = max_active)

    for (i in seq_along(ready)) {
      dev <- ready[[i]]
      x <- resps[[i]]
      s <- st[[dev]]

      if (inherits(x, "httr2_response")) {
        body <- httr2::resp_body_json(x, simplifyVector = FALSE)
        s$resolved_id <- s$resolved_id %||% body$metadata$device_id %||% dev
        page <- build_readings_tibble(body$values %||% list(), device_id = s$resolved_id)
        if (nrow(page) > 0) {
          if (!is.null(store)) {
            zc_store_write(store, page)
          } else {
            s$data[[length(s$data) + 1L]] <- page
          }
          s$added <- s$added + nrow(page)
          n_readings <<- n_readings + nrow(page)
          m <- suppressWarnings(max(page$timestamp, na.rm = TRUE))
          if (is.finite(m) && (is.na(s$max_ts) || m > s$max_ts)) {
            s$max_ts <- m
          }
        }
        s$pages <- s$pages + 1L
        s$next_token <- body$pagination$next_token
        s$ready_at <- 0
        if (is.null(s$next_token) || s$pages >= max_pages) {
          s$done <- TRUE
          n_done <<- n_done + 1L
        }
      } else {
        resp <- x$resp
        status <- if (!is.null(resp)) httr2::resp_status(resp) else NA_integer_
        if (isTRUE(status == 429L)) {
          wait <- zc_retry_after(resp)
          if (is.na(wait)) wait <- 60
          s$ready_at <- as.numeric(Sys.time()) + wait
          if (show_progress) {
            cli::cli_alert_info("ZENTRA Cloud rate limit reached for {.val {dev}}; waiting {round(wait)}s.")
          }
        } else {
          s$errored <- TRUE
          s$message <- zc_parallel_error_message(x)
          n_done <<- n_done + 1L
          if (show_progress) {
            cli::cli_warn(c("x" = "Failed to sync {.val {dev}}: {s$message}"))
          }
        }
      }
      st[[dev]] <- s
    }

    if (show_progress) {
      cli::cli_progress_update()
    }
  }

  if (show_progress) {
    cli::cli_progress_done()
  }

  lapply(device_id, function(dev) {
    s <- st[[dev]]
    data <- if (is.null(store) && length(s$data)) vctrs::vec_rbind(!!!s$data) else NULL
    list(
      device_id = dev,
      rows_added = as.integer(s$added),
      last_time = if (is.na(s$max_ts)) NA_real_ else s$max_ts,
      status = if (s$errored) "error" else "ok",
      message = if (s$errored) s$message else NA_character_,
      data = data
    )
  })
}

# Non-throwing HTTP error summary for the parallel path (the sequential path
# raises typed errors; here we record a message and keep other devices going).
zc_parallel_error_message <- function(cnd) {
  resp <- cnd$resp
  status <- if (!is.null(resp)) httr2::resp_status(resp) else NA_integer_
  detail <- if (!is.null(resp)) zc_error_detail(resp) else NULL
  msg <- if (!is.na(status)) sprintf("HTTP %s", status) else conditionMessage(cnd)
  if (!is.null(detail)) msg <- paste0(msg, ": ", detail)
  msg
}
