# Helpers for the live API contract tests (test-live-contract.R).
#
# These are the only tests that touch the network. Everything else in the suite
# runs against mocked responses (helper-mock.R), which means the suite cannot
# notice when the upstream v5 API changes shape. The live tests close that gap
# and run on a schedule in CI; see the `api-contract` job in .gitlab-ci.yml.

# Is a live run mandatory here? The scheduled CI job sets this. Everywhere else
# a missing key is a legitimate reason to skip; in that job it is a fault, and
# skipping would report success. A green pipeline reporting "all skipped" is
# precisely the failure mode this file exists to rule out, so credentials that
# are absent, mistyped, or not reaching the runner must fail loudly instead.
live_required <- function() {
  identical(Sys.getenv("ZENTRAR_REQUIRE_LIVE"), "true")
}

live_unavailable <- function(reason) {
  if (live_required()) {
    stop("live API tests are required here, but ", reason, call. = FALSE)
  }
  testthat::skip(reason)
}

# Gate: live tests run only when explicitly enabled and credentialled, so
# R CMD check, CRAN and contributors without a key are unaffected.
skip_if_not_live <- function() {
  if (!live_required()) {
    testthat::skip_on_cran()
    if (!identical(Sys.getenv("ZENTRAR_LIVE_TESTS"), "true")) {
      testthat::skip("live API tests disabled (set ZENTRAR_LIVE_TESTS=true)")
    }
  }
  if (!nzchar(Sys.getenv("ZENTRACLOUD_API_KEY"))) {
    live_unavailable("ZENTRACLOUD_API_KEY is not set")
  }
  invisible(TRUE)
}

live_device_id <- function() {
  id <- trimws(Sys.getenv("ZENTRAR_TEST_DEVICE_SN", unset = ""))
  if (!nzchar(id)) {
    live_unavailable("ZENTRAR_TEST_DEVICE_SN is not set")
  }
  id
}

# The v5 API allows roughly a 5-request burst and then ~1 request/minute, so
# every test reuses one fetch per endpoint instead of calling again.
.live_cache <- new.env(parent = emptyenv())

live_fetch <- function(name, fn) {
  if (!exists(name, envir = .live_cache, inherits = FALSE)) {
    assign(name, fn(), envir = .live_cache)
  }
  get(name, envir = .live_cache, inherits = FALSE)
}

live_devices_body <- function() {
  live_fetch("devices", function() {
    zentraR:::zc_get_json(
      "devices",
      query = list(expand = "max_min_timestamp", page_num = 1L, limit = 1000L)
    )
  })
}

# Readings for the test device. Prefers a recent relative window; if the device
# has not reported lately, falls back to the 24 hours before its last known
# measurement so a quiet logger does not masquerade as an API change.
live_readings_body <- function() {
  live_fetch("readings", function() {
    id <- live_device_id()
    fetch <- function(query) {
      zentraR:::zc_get_json(
        c("devices", id, "data"),
        query = c(query, list(direction = "descending", units = "metric"))
      )
    }

    body <- fetch(zentraR:::zc_window_query(window = "7d"))
    if (length(body$values)) {
      return(body)
    }

    last <- live_device_last_measurement(id)
    if (is.na(last)) {
      return(body)
    }
    fetch(zentraR:::zc_window_query(
      start_timestamp = last - 86400,
      end_timestamp = last
    ))
  })
}

# Last measurement time (epoch seconds) for a device, from the devices response.
live_device_last_measurement <- function(id) {
  devices <- live_devices_body()$devices
  for (d in devices) {
    if (identical(zentraR:::p_chr(d, "device_id"), id)) {
      return(zentraR:::p_dbl(d$data_range, "last_measurement"))
    }
  }
  NA_real_
}

# The contract check. `expected` is the exact set of fields zentraR's parsers
# expect on one JSON object:
#
#   - a missing field breaks parsing now (columns silently become NA);
#   - an unknown field means the API grew something we do not handle, and is the
#     earliest warning that upstream has changed.
#
# Both directions fail the test, because a scheduled red pipeline is the alert.
expect_key_set <- function(object, expected, what) {
  actual <- names(object)
  if (is.null(actual)) {
    actual <- character()
  }
  missing <- setdiff(expected, actual)
  unknown <- setdiff(actual, expected)

  testthat::expect(
    length(missing) == 0,
    sprintf(
      "%s no longer returns field(s) zentraR depends on: %s.",
      what, paste(missing, collapse = ", ")
    )
  )
  testthat::expect(
    length(unknown) == 0,
    sprintf(
      "%s returned field(s) zentraR does not know about: %s. The v5 API has changed.",
      what, paste(unknown, collapse = ", ")
    )
  )
  invisible(object)
}
