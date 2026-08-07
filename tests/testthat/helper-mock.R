# Build a mocked httr2 response with a JSON body from an R object.
mock_response <- function(obj, status = 200L) {
  body <- jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA)
  httr2::response(
    status_code = status,
    url = "https://api.zentracloud.io/v5/mock",
    method = "GET",
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(as.character(body))
  )
}

# A single device object, optionally with a data_range (max_min_timestamp).
mock_device <- function(id, org = "org-1", first = NULL, last = NULL) {
  d <- list(
    device_id = id, name = id, organization_id = org,
    organization_name = "Test Org", device_type = 181L, model = "ATMOS 41W"
  )
  if (!is.null(first) || !is.null(last)) {
    d$data_range <- list(first_measurement = first, last_measurement = last)
  }
  d
}

# A single reading object.
mock_reading <- function(measurement, value, timestamp, port = 1L,
                         unit = "m/s", sensor = "ATMOS 41W", error_code = 0L) {
  list(
    port_num = port, measurement = measurement, unit = unit,
    sensor_name = sensor, value = value, timestamp = timestamp,
    datetime = format(zentraR:::zc_epoch_to_posixct(timestamp), "%Y-%m-%d %H:%M:%S+00:00"),
    error_code = error_code
  )
}

# Extract a query parameter from a mocked request's URL.
req_query <- function(req, name) {
  httr2::url_parse(req$url)$query[[name]]
}

# Ensure requests can be built: a dummy key for the duration of a test.
local_test_key <- function(env = parent.frame()) {
  withr::local_envvar(ZENTRACLOUD_API_KEY = "test-key", .local_envir = env)
}
