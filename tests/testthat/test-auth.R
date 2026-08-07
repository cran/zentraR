test_that("zc_key resolves explicit argument first", {
  expect_equal(zc_key("explicit"), "explicit")
  expect_equal(zc_key("  padded  "), "padded")
})

test_that("zc_key reads the environment variable", {
  withr::local_envvar(ZENTRACLOUD_API_KEY = "env-key")
  # Ensure no session key is shadowing it.
  if (exists("api_key", envir = zentraR:::.zentraR_env)) {
    withr::defer(assign("api_key", get("api_key", envir = zentraR:::.zentraR_env), envir = zentraR:::.zentraR_env))
    rm("api_key", envir = zentraR:::.zentraR_env)
  }
  expect_equal(zc_key(), "env-key")
})

test_that("zc_key errors clearly when no key is available", {
  withr::local_envvar(ZENTRACLOUD_API_KEY = "")
  had_key <- exists("api_key", envir = zentraR:::.zentraR_env)
  if (had_key) {
    saved <- get("api_key", envir = zentraR:::.zentraR_env)
    withr::defer(assign("api_key", saved, envir = zentraR:::.zentraR_env))
    rm("api_key", envir = zentraR:::.zentraR_env)
  }
  expect_error(zc_key(), class = "zentraR_auth_error")
  expect_false(zc_has_key())
})

test_that("zc_set_key stores the key for the session", {
  withr::local_envvar(ZENTRACLOUD_API_KEY = "")
  withr::defer(if (exists("api_key", envir = zentraR:::.zentraR_env)) rm("api_key", envir = zentraR:::.zentraR_env))
  zc_set_key("session-key")
  expect_equal(zc_key(), "session-key")
  expect_true(zc_has_key())
})

test_that("zc_set_key rejects empty keys", {
  expect_error(zc_set_key(""), "non-empty")
  expect_error(zc_set_key(c("a", "b")), "single")
})
