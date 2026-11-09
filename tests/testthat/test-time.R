test_that("zc_to_iso8601 handles supported input types", {
  expect_equal(zentraR:::zc_to_iso8601(as.Date("2026-02-18")), "2026-02-18T00:00:00+00:00")
  expect_equal(
    zentraR:::zc_to_iso8601(as.POSIXct("2026-02-18 12:30:00", tz = "UTC")),
    "2026-02-18T12:30:00+00:00"
  )
  expect_equal(zentraR:::zc_to_iso8601(1771401600), "2026-02-18T08:00:00+00:00")
  expect_equal(zentraR:::zc_to_iso8601("2026-02-18T00:00:00Z"), "2026-02-18T00:00:00Z")
  expect_null(zentraR:::zc_to_iso8601(NULL))
})

test_that("zc_to_iso8601 normalises non-UTC POSIXct to UTC", {
  t <- as.POSIXct("2026-02-18 00:00:00", tz = "America/Los_Angeles")
  expect_equal(zentraR:::zc_to_iso8601(t), "2026-02-18T08:00:00+00:00")
})

test_that("zc_to_iso8601 rejects unsupported input", {
  expect_error(zentraR:::zc_to_iso8601(list(1)), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_to_iso8601(""), class = "zentraR_invalid_request_error")
})

test_that("zc_to_epoch coerces to integer seconds", {
  expect_equal(zentraR:::zc_to_epoch(1771401600.4), 1771401600L)
  expect_equal(
    zentraR:::zc_to_epoch(as.POSIXct("2026-02-18 08:00:00", tz = "UTC")),
    1771401600L
  )
  expect_null(zentraR:::zc_to_epoch(NULL))
})

test_that("zc_window_query builds datetime params", {
  q <- zentraR:::zc_window_query(start = as.Date("2026-01-01"), end = as.Date("2026-02-01"))
  expect_named(q, c("start_datetime", "end_datetime"))
  expect_equal(q$start_datetime, "2026-01-01T00:00:00+00:00")
})

test_that("zc_window_query builds timestamp params", {
  q <- zentraR:::zc_window_query(start_timestamp = 1000, end_timestamp = 2000)
  expect_named(q, c("start_timestamp", "end_timestamp"))
  expect_equal(q$start_timestamp, 1000L)
})

test_that("zc_window_query rejects mixing datetimes and timestamps", {
  expect_error(
    zentraR:::zc_window_query(start = as.Date("2026-01-01"), start_timestamp = 1000),
    class = "zentraR_invalid_request_error"
  )
})

test_that("zc_window_query is empty when nothing supplied", {
  expect_length(zentraR:::zc_window_query(), 0)
})

test_that("zc_validate_window accepts each supported unit", {
  expect_equal(zentraR:::zc_validate_window("30m"), "30m")
  expect_equal(zentraR:::zc_validate_window("24h"), "24h")
  expect_equal(zentraR:::zc_validate_window("7d"), "7d")
  expect_equal(zentraR:::zc_validate_window("2w"), "2w")
  expect_null(zentraR:::zc_validate_window(NULL))
})

test_that("zc_validate_window normalises case and surrounding whitespace", {
  expect_equal(zentraR:::zc_validate_window(" 24H "), "24h")
})

test_that("zc_validate_window accepts the one-year maximum for every unit", {
  expect_equal(zentraR:::zc_validate_window("525600m"), "525600m")
  expect_equal(zentraR:::zc_validate_window("8760h"), "8760h")
  expect_equal(zentraR:::zc_validate_window("365d"), "365d")
  expect_equal(zentraR:::zc_validate_window("52w"), "52w")
})

test_that("zc_validate_window rejects windows longer than one year", {
  expect_error(zentraR:::zc_validate_window("366d"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("53w"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("8761h"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("525601m"), class = "zentraR_invalid_request_error")
})

test_that("zc_validate_window rejects a zero-length window", {
  expect_error(zentraR:::zc_validate_window("0h"), class = "zentraR_invalid_request_error")
})

test_that("zc_validate_window rejects malformed windows", {
  expect_error(zentraR:::zc_validate_window("7 days"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("7"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("d"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("1.5d"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("-1d"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window("7y"), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window(""), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window(7), class = "zentraR_invalid_request_error")
  expect_error(zentraR:::zc_validate_window(c("1d", "2d")), class = "zentraR_invalid_request_error")
})

test_that("zc_window_query builds a relative window param", {
  q <- zentraR:::zc_window_query(window = "24h")
  expect_named(q, "window")
  expect_equal(q$window, "24h")
})

test_that("zc_window_query rejects mixing window with datetimes", {
  expect_error(
    zentraR:::zc_window_query(start = as.Date("2026-01-01"), window = "24h"),
    class = "zentraR_invalid_request_error"
  )
})

test_that("zc_window_query rejects mixing window with timestamps", {
  expect_error(
    zentraR:::zc_window_query(start_timestamp = 1000, window = "24h"),
    class = "zentraR_invalid_request_error"
  )
})
