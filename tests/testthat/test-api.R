test_that("zc_list_devices auto-paginates across pages", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    pn <- as.integer(req_query(req, "page_num"))
    if (pn == 1L) {
      mock_response(list(
        devices = list(mock_device("A1"), mock_device("A2")),
        pagination = list(page_num = 1L, limit = 2L, num_devices = 3L, total_pages = 2L)
      ))
    } else {
      mock_response(list(
        devices = list(mock_device("A3")),
        pagination = list(page_num = 2L, limit = 2L, num_devices = 3L, total_pages = 2L)
      ))
    }
  })
  devs <- zc_list_devices(limit = 2, key = "test-key")
  expect_equal(nrow(devs), 3)
  expect_equal(devs$device_id, c("A1", "A2", "A3"))
})

test_that("zc_list_devices returns an empty tibble when there are no devices", {
  local_test_key()
  httr2::local_mocked_responses(list(mock_response(list(
    devices = list(),
    pagination = list(page_num = 1L, limit = 1000L, num_devices = 0L, total_pages = 1L)
  ))))
  devs <- zc_list_devices(key = "test-key")
  expect_equal(nrow(devs), 0)
  expect_true("device_id" %in% names(devs))
})

test_that("zc_get_readings follows next_token pagination", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    tok <- req_query(req, "next_token")
    if (is.null(tok)) {
      mock_response(list(
        metadata = list(device_id = "d"),
        values = list(mock_reading("Air Temperature", 1, 100)),
        pagination = list(num_readings = 1L, next_token = "PAGE2")
      ))
    } else {
      mock_response(list(
        metadata = list(device_id = "d"),
        values = list(mock_reading("Air Temperature", 2, 200)),
        pagination = list(num_readings = 1L, next_token = NULL)
      ))
    }
  })
  r <- zc_get_readings("d", start = as.Date("2026-01-01"), end = as.Date("2026-03-01"), key = "test-key")
  expect_equal(nrow(r), 2)
  expect_equal(r$timestamp, c(100, 200))
})

test_that("zc_get_readings with progress = TRUE still returns correct data", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    tok <- req_query(req, "next_token")
    if (is.null(tok)) {
      mock_response(list(
        metadata = list(device_id = "d"),
        values = list(mock_reading("Air Temperature", 1, 100)),
        pagination = list(num_readings = 1L, next_token = "P2", start_datetime = "2026-02-01T00:00:00Z")
      ))
    } else {
      mock_response(list(
        metadata = list(device_id = "d"),
        values = list(mock_reading("Air Temperature", 2, 200)),
        pagination = list(num_readings = 1L, next_token = NULL, start_datetime = "2026-03-01T00:00:00Z")
      ))
    }
  })
  r <- zc_get_readings("d", start = as.Date("2026-02-01"), end = as.Date("2026-03-31"),
                       progress = TRUE, key = "test-key")
  expect_equal(nrow(r), 2)
  expect_equal(r$timestamp, c(100, 200))
})

test_that("zc_get_readings parses a large page correctly (chunked path, progress on)", {
  local_test_key()
  big <- lapply(seq_len(21000), function(i) {
    mock_reading("Air Temperature", i / 10, 1770000000 + i * 60, unit = "C")
  })
  httr2::local_mocked_responses(function(req) {
    mock_response(list(
      metadata = list(device_id = "d"),
      values = big,
      pagination = list(next_token = NULL, start_datetime = "2026-01-01T00:00:00Z")
    ))
  })
  r <- zc_get_readings("d", start = as.Date("2026-01-01"), progress = TRUE, key = "test-key")
  expect_equal(nrow(r), 21000)
  expect_equal(r$value[1], 0.1)
  expect_equal(r$value[21000], 2100)
})

test_that("zc_get_readings validates arguments", {
  expect_error(zc_get_readings("", key = "k"), class = "zentraR_invalid_request_error")
  expect_error(zc_get_readings("d", direction = "sideways", key = "k"), class = "zentraR_invalid_request_error")
  expect_error(zc_get_readings("d", units = "furlongs", key = "k"), class = "zentraR_invalid_request_error")
})

test_that("HTTP errors map to typed zentraR conditions", {
  local_test_key()
  withr::local_options(zentraR.max_tries = 1)

  httr2::local_mocked_responses(list(mock_response(list(detail = "bad key"), status = 401L)))
  expect_error(zc_list_devices(key = "k"), class = "zentraR_auth_error")

  httr2::local_mocked_responses(list(mock_response(list(detail = "bad limit"), status = 422L)))
  expect_error(zc_list_devices(key = "k"), class = "zentraR_invalid_request_error")

  httr2::local_mocked_responses(list(mock_response(list(), status = 429L)))
  expect_error(zc_list_devices(key = "k"), class = "zentraR_rate_limit_error")
})

test_that("zc_retry_after parses the server's next-allowed timestamp", {
  future <- as.numeric(Sys.time()) + 42
  resp <- mock_response(
    list(detail = sprintf("Too many requests, please try again at or after this time %f.", future)),
    status = 429L
  )
  w <- zentraR:::zc_retry_after(resp)
  expect_true(w > 0 && w <= 300)
  expect_lt(abs(w - 42), 5)

  # A past timestamp means "retry now" (0s wait).
  past <- mock_response(
    list(detail = "Too many requests, please try again at or after this time 1000.0."),
    status = 429L
  )
  expect_equal(zentraR:::zc_retry_after(past), 0)

  # Unparseable detail falls back to httr2's backoff (NA).
  unparseable <- mock_response(list(detail = "no timestamp here"), status = 429L)
  expect_true(is.na(zentraR:::zc_retry_after(unparseable)))
})

test_that("zc_sync writes to a store and de-duplicates on re-run", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    mock_response(list(
      metadata = list(device_id = "d"),
      values = list(
        mock_reading("Air Temperature", 1, 100),
        mock_reading("Air Temperature", 2, 200)
      ),
      pagination = list(num_readings = 2L, next_token = NULL)
    ))
  })
  store <- zc_store_memory()

  s1 <- zc_sync("d", store = store, start = as.Date("2026-01-01"), quiet = TRUE, key = "test-key")
  expect_equal(s1$rows_added, 2L)
  expect_equal(s1$status, "ok")
  expect_equal(nrow(zc_store_read(store, "d")), 2)

  # Second run resumes after the latest stored timestamp; overlapping data dedups.
  zc_sync("d", store = store, quiet = TRUE, key = "test-key")
  expect_equal(nrow(zc_store_read(store, "d")), 2)
})

test_that("zc_sync with progress = TRUE still writes and summarizes", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    mock_response(list(
      metadata = list(device_id = "d"),
      values = list(mock_reading("Air Temperature", 1, 100), mock_reading("Air Temperature", 2, 200)),
      pagination = list(num_readings = 2L, next_token = NULL, start_datetime = "2026-01-01T00:00:00Z")
    ))
  })
  store <- zc_store_memory()
  s <- zc_sync("d", store = store, start = as.Date("2026-01-01"), progress = TRUE, key = "test-key")
  expect_equal(s$rows_added, 2L)
  expect_equal(s$status, "ok")
  expect_equal(nrow(zc_store_read(store, "d")), 2)
})

test_that("zc_sync fetches multiple devices in parallel (max_active > 1)", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    path <- httr2::url_parse(req$url)$path
    id <- if (grepl("D1", path)) "D1" else "D2"
    mock_response(list(
      metadata = list(device_id = id),
      values = list(mock_reading("Air Temperature", 1, 100)),
      pagination = list(next_token = NULL, start_datetime = "2026-01-01T00:00:00Z")
    ))
  })
  store <- zc_store_memory()
  s <- zc_sync(c("D1", "D2"), store = store, start = as.Date("2026-01-01"),
               max_active = 2, quiet = TRUE, key = "test-key")
  expect_setequal(s$device_id, c("D1", "D2"))
  expect_true(all(s$status == "ok"))
  expect_equal(sum(s$rows_added), 2L)
  expect_equal(nrow(zc_store_read(store)), 2)
})

test_that("zc_sync parallel backs off and retries a rate-limited device", {
  local_test_key()
  calls <- new.env()
  calls$D1 <- 0L
  httr2::local_mocked_responses(function(req) {
    path <- httr2::url_parse(req$url)$path
    id <- if (grepl("D1", path)) "D1" else "D2"
    if (id == "D1") {
      calls$D1 <- calls$D1 + 1L
      if (calls$D1 == 1L) {
        # 429 whose next-allowed time is in the past -> ~0s backoff, retried next round.
        return(mock_response(
          list(detail = "Too many requests, please try again at or after this time 1000.0."),
          status = 429L
        ))
      }
    }
    mock_response(list(
      metadata = list(device_id = id),
      values = list(mock_reading("Air Temperature", 1, 100)),
      pagination = list(next_token = NULL, start_datetime = "2026-01-01T00:00:00Z")
    ))
  })
  store <- zc_store_memory()
  s <- zc_sync(c("D1", "D2"), store = store, start = as.Date("2026-01-01"),
               max_active = 2, quiet = TRUE, key = "test-key")
  expect_true(all(s$status == "ok"))
  expect_equal(nrow(zc_store_read(store, "D1")), 1)
  expect_gt(calls$D1, 1L) # D1 was rate-limited then retried
})

test_that("zc_sync return-only yields combined readings", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    mock_response(list(
      metadata = list(device_id = "d"),
      values = list(mock_reading("X", 1, 100)),
      pagination = list(next_token = NULL)
    ))
  })
  out <- zc_sync("d", store = NULL, start = as.Date("2026-01-01"), quiet = TRUE, key = "test-key")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
})

test_that("zc_sync discovers devices when device_id is NULL", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    path <- httr2::url_parse(req$url)$path
    if (grepl("/data$", path)) {
      mock_response(list(
        metadata = list(device_id = "A1"),
        values = list(mock_reading("X", 1, 100)),
        pagination = list(next_token = NULL)
      ))
    } else {
      mock_response(list(
        devices = list(mock_device("A1", first = 50, last = 100)),
        pagination = list(page_num = 1L, limit = 1000L, num_devices = 1L, total_pages = 1L)
      ))
    }
  })
  store <- zc_store_memory()
  s <- zc_sync(store = store, quiet = TRUE, key = "test-key")
  expect_equal(s$device_id, "A1")
  expect_equal(nrow(zc_store_read(store)), 1)
})

test_that("zc_sync reports per-device errors without aborting the run", {
  local_test_key()
  withr::local_options(zentraR.max_tries = 1)
  httr2::local_mocked_responses(function(req) {
    dev <- httr2::url_parse(req$url)$path
    if (grepl("BAD", dev)) {
      mock_response(list(detail = "nope"), status = 403L)
    } else {
      mock_response(list(
        metadata = list(device_id = "GOOD"),
        values = list(mock_reading("X", 1, 100)),
        pagination = list(next_token = NULL)
      ))
    }
  })
  store <- zc_store_memory()
  s <- zc_sync(c("GOOD", "BAD"), store = store, start = as.Date("2026-01-01"), quiet = TRUE, key = "test-key")
  expect_setequal(s$device_id, c("GOOD", "BAD"))
  expect_equal(s$status[s$device_id == "GOOD"], "ok")
  expect_equal(s$status[s$device_id == "BAD"], "error")
  expect_equal(nrow(zc_store_read(store, "GOOD")), 1)
})

test_that("zc_get_readings sends the relative window on every page", {
  local_test_key()
  seen <- character()
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req_query(req, "window") %||% NA_character_)
    tok <- req_query(req, "next_token")
    if (is.null(tok)) {
      mock_response(list(
        metadata = list(device_id = "d"),
        values = list(mock_reading("Air Temperature", 1, 100)),
        pagination = list(num_readings = 1L, next_token = "PAGE2")
      ))
    } else {
      mock_response(list(
        metadata = list(device_id = "d"),
        values = list(mock_reading("Air Temperature", 2, 200)),
        pagination = list(num_readings = 1L, next_token = NULL)
      ))
    }
  })
  r <- zc_get_readings("d", window = "24h", key = "test-key")
  expect_equal(nrow(r), 2)
  expect_equal(seen, c("24h", "24h"))
})

test_that("zc_get_readings rejects an out-of-range window before requesting", {
  local_test_key()
  httr2::local_mocked_responses(function(req) {
    stop("no request should be made")
  })
  expect_error(
    zc_get_readings("d", window = "400d", key = "test-key"),
    class = "zentraR_invalid_request_error"
  )
})
