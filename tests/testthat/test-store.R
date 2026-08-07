make_readings <- function(device_id = "d", timestamps = c(100, 200)) {
  vals <- lapply(timestamps, function(ts) mock_reading("Air Temperature", ts / 10, ts, unit = "C"))
  zentraR:::build_readings_tibble(vals, device_id)
}

for (backend in c("memory", "csv", "rds")) {
  make_store <- function(backend) {
    switch(backend,
      memory = zc_store_memory(),
      csv = zc_store_csv(tempfile("zc-csv-")),
      rds = zc_store_rds(tempfile("zc-rds-"))
    )
  }

  test_that(paste0(backend, " store: write, read, last_time"), {
    store <- make_store(backend)
    r <- make_readings("d", c(100, 200))
    zc_store_write(store, r)

    got <- zc_store_read(store, "d")
    expect_equal(nrow(got), 2)
    expect_s3_class(got$datetime, "POSIXct")
    expect_equal(zc_store_last_time(store, "d"), 200)
    expect_true(is.na(zc_store_last_time(store, "missing")))
  })

  test_that(paste0(backend, " store: append is incremental and de-duplicated"), {
    store <- make_store(backend)
    zc_store_write(store, make_readings("d", c(100, 200)))
    # Re-write overlapping + new data.
    zc_store_write(store, make_readings("d", c(200, 300)))
    got <- zc_store_read(store, "d")
    expect_equal(nrow(got), 3) # 100, 200, 300 (200 not duplicated)
    expect_equal(sort(got$timestamp), c(100, 200, 300))
    expect_equal(zc_store_last_time(store, "d"), 300)
  })

  test_that(paste0(backend, " store: reads across multiple devices"), {
    store <- make_store(backend)
    zc_store_write(store, make_readings("d1", c(100)))
    zc_store_write(store, make_readings("d2", c(200)))
    all <- zc_store_read(store)
    expect_setequal(all$device_id, c("d1", "d2"))
    expect_equal(nrow(all), 2)
  })

  test_that(paste0(backend, " store: empty read returns typed empty frame"), {
    store <- make_store(backend)
    got <- zc_store_read(store, "nothing")
    expect_equal(nrow(got), 0)
    expect_named(got, zentraR:::zc_reading_cols)
  })
}

test_that("non-store objects are rejected", {
  expect_error(zc_store_read(list()), class = "zentraR_invalid_request_error")
  expect_error(zc_store_write(list(), make_readings()), class = "zentraR_invalid_request_error")
})
