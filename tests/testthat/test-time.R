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
