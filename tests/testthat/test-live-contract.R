# Contract tests against the live ZENTRA Cloud v5 API.
#
# Purpose: detect upstream API changes before users do. The rest of the suite
# runs on mocked responses and would stay green through a field rename, a type
# change or a dropped pagination field. These tests assert the real shape.
#
# Enabled by ZENTRAR_LIVE_TESTS=true plus a key; skipped everywhere else.
# See helper-live.R for the gate and the key-set contract check.

# ---- devices ----------------------------------------------------------------

test_that("the devices endpoint returns the envelope zc_list_devices() walks", {
  skip_if_not_live()
  body <- live_devices_body()

  expect_type(body$devices, "list")
  expect_gt(length(body$devices), 0)
  expect_type(body$pagination, "list")
  expect_true(is.numeric(body$pagination$total_pages))
})

test_that("device objects match zentraR's field contract", {
  skip_if_not_live()
  device <- live_devices_body()$devices[[1]]

  expect_key_set(
    device,
    c(
      # Parsed into the tibble by build_devices_tibble().
      "device_id", "name", "organization_id", "organization_name",
      "device_type", "model", "data_range",
      # Returned by v5 but deliberately not surfaced by zentraR. Listed so the
      # test still fails if they disappear or if a genuinely new field appears.
      "can_read_data"
    ),
    "A v5 device object"
  )
  expect_key_set(
    device$data_range,
    c("first_measurement", "last_measurement"),
    "A v5 device data_range"
  )

  expect_type(device$device_id, "character")
  expect_type(device$name, "character")
  expect_type(device$organization_id, "character")
  expect_type(device$organization_name, "character")
  expect_type(device$model, "character")
  expect_true(is.numeric(device$device_type))
})

# ---- readings ---------------------------------------------------------------

test_that("the readings endpoint returns the envelope the page iterator walks", {
  skip_if_not_live()
  body <- live_readings_body()

  expect_type(body$metadata, "list")
  expect_type(body$metadata$device_id, "character")
  expect_type(body$values, "list")
  expect_type(body$pagination, "list")

  # zc_readings_fetch() reads these two directly: next_token drives the loop
  # (absent on the last page), start_datetime labels the progress bar.
  expect_true(is.null(body$pagination$next_token) ||
                is.character(body$pagination$next_token))
  expect_true(is.null(body$pagination$start_datetime) ||
                is.character(body$pagination$start_datetime))

  # An empty window here is a real signal, not a skip: either the test device
  # stopped reporting or the window parameters no longer mean what we think.
  expect_gt(length(body$values), 0)
})

test_that("reading objects match zentraR's field contract", {
  skip_if_not_live()
  values <- live_readings_body()$values
  skip_if(length(values) == 0, "no readings returned; see the envelope test")
  reading <- values[[1]]

  expect_key_set(
    reading,
    c(
      # Parsed into the tibble by build_readings_tibble().
      "port_num", "measurement", "unit", "sensor_name",
      "value", "timestamp", "datetime", "error_code",
      # Returned by v5 but deliberately not surfaced by zentraR.
      "position", "reading_id"
    ),
    "A v5 reading object"
  )

  expect_type(reading$measurement, "character")
  expect_type(reading$unit, "character")
  expect_type(reading$sensor_name, "character")
  expect_true(is.numeric(reading$port_num))
  expect_true(is.numeric(reading$error_code))
  expect_true(is.null(reading$value) || is.numeric(reading$value))

  # Epoch seconds, not milliseconds and not an ISO string: a unit change here
  # would shift every datetime in the package by decades without erroring.
  expect_true(is.numeric(reading$timestamp))
  expect_gt(reading$timestamp, 946684800)                      # 2000-01-01
  expect_lt(reading$timestamp, as.numeric(Sys.time()) + 86400)
})

# ---- end-to-end parse -------------------------------------------------------

test_that("live responses parse into the documented tidy columns", {
  skip_if_not_live()

  devices <- zentraR:::build_devices_tibble(live_devices_body()$devices)
  expect_named(
    devices,
    c(
      "device_id", "name", "organization_id", "organization_name",
      "device_type", "model", "first_measurement", "last_measurement"
    )
  )
  expect_s3_class(devices$last_measurement, "POSIXct")

  body <- live_readings_body()
  skip_if(length(body$values) == 0, "no readings returned; see the envelope test")
  readings <- zentraR:::build_readings_tibble(body$values, live_device_id())
  expect_named(
    readings,
    c(
      "device_id", "datetime", "timestamp", "port_num",
      "sensor_name", "measurement", "value", "unit", "error_code"
    )
  )
  expect_s3_class(readings$datetime, "POSIXct")
  expect_false(anyNA(readings$measurement))
  expect_false(anyNA(readings$timestamp))
})
