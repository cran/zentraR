test_that("build_readings_tibble produces tidy long output", {
  vals <- list(
    mock_reading("Gust Speed", 0.53, 1771401600, port = 1L),
    mock_reading("Air Temperature", 20.1, 1771401600, port = 1L, unit = "C")
  )
  r <- zentraR:::build_readings_tibble(vals, "A4100123")
  expect_s3_class(r, "tbl_df")
  expect_equal(nrow(r), 2)
  expect_equal(r$device_id, c("A4100123", "A4100123"))
  expect_s3_class(r$datetime, "POSIXct")
  expect_equal(r$value, c(0.53, 20.1))
  expect_equal(r$measurement, c("Gust Speed", "Air Temperature"))
})

test_that("build_readings_tibble handles empty input", {
  r <- zentraR:::build_readings_tibble(list(), "x")
  expect_equal(nrow(r), 0)
  expect_named(r, zentraR:::zc_reading_cols)
})

test_that("build_readings_tibble tolerates missing / null fields", {
  vals <- list(list(port_num = 1L, measurement = "X", timestamp = 100, error_code = 0L))
  r <- zentraR:::build_readings_tibble(vals, "d")
  expect_true(is.na(r$value))
  expect_true(is.na(r$unit))
})

test_that("zc_pivot_wider spreads single-port measurements", {
  vals <- list(
    mock_reading("Gust Speed", 0.53, 1771401600),
    mock_reading("Air Temperature", 20.1, 1771401600, unit = "C"),
    mock_reading("Gust Speed", 0.61, 1771402500),
    mock_reading("Air Temperature", 19.8, 1771402500, unit = "C")
  )
  r <- zentraR:::build_readings_tibble(vals, "d")
  w <- zc_pivot_wider(r)
  expect_equal(nrow(w), 2)
  expect_true(all(c("Gust Speed", "Air Temperature") %in% names(w)))
  expect_type(w$`Gust Speed`, "double")
})

test_that("zc_pivot_wider disambiguates measurements shared across ports", {
  vals <- list(
    mock_reading("Air Temperature", 20.1, 1771401600, port = 1L),
    mock_reading("Air Temperature", 19.0, 1771401600, port = 2L)
  )
  r <- zentraR:::build_readings_tibble(vals, "d")
  w <- zc_pivot_wider(r)
  expect_equal(nrow(w), 1)
  expect_true(all(c("Air Temperature_p1", "Air Temperature_p2") %in% names(w)))
  expect_equal(w$`Air Temperature_p1`, 20.1)
  expect_equal(w$`Air Temperature_p2`, 19.0)
})

test_that("zc_pivot_wider on empty readings returns empty frame", {
  w <- zc_pivot_wider(zentraR:::empty_readings())
  expect_equal(nrow(w), 0)
})

test_that("zc_error_codes and zc_label_errors work", {
  codes <- zc_error_codes()
  expect_true(all(c("error_code", "error_label") %in% names(codes)))
  expect_equal(codes$error_label[codes$error_code == 0], "No error (valid reading)")

  r <- zentraR:::build_readings_tibble(
    list(
      mock_reading("X", 1, 100, error_code = 0L),
      mock_reading("X", 2, 200, error_code = 143L),
      mock_reading("X", 3, 300, error_code = 9999L)
    ),
    "d"
  )
  labelled <- zc_label_errors(r)
  expect_equal(labelled$error_label[1], "No error (valid reading)")
  expect_match(labelled$error_label[2], "No data available")
  expect_match(labelled$error_label[3], "Unknown error code")
})

test_that("build_devices_tibble flattens base and expanded fields", {
  devs <- list(
    mock_device("A1", first = 1670611500, last = 1784664000),
    mock_device("A2", first = 1670611500, last = 1784664000)
  )
  dt <- zentraR:::build_devices_tibble(devs)
  expect_equal(nrow(dt), 2)
  expect_equal(dt$device_id, c("A1", "A2"))
  expect_s3_class(dt$first_measurement, "POSIXct")
})

test_that("build_devices_tibble handles empty list", {
  dt <- zentraR:::build_devices_tibble(list())
  expect_equal(nrow(dt), 0)
  expect_true(all(c("device_id", "model") %in% names(dt)))
})

test_that("zc_validate_expand rejects unknown values", {
  expect_error(zentraR:::zc_validate_expand("nope"), class = "zentraR_invalid_request_error")
  expect_equal(zentraR:::zc_validate_expand(c("settings", "hardware")), "settings,hardware")
  expect_null(zentraR:::zc_validate_expand(NULL))
})
