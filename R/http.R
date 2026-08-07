# ---- Configuration helpers --------------------------------------------------

# Base URL for the v5 API. Configurable via option `zentraR.base_url` (set to a
# default in .onLoad) so tests and staging environments can point elsewhere.
zc_base_url <- function() {
  getOption("zentraR.base_url", "https://api.zentracloud.io/v5")
}

zc_user_agent <- function() {
  ver <- tryCatch(as.character(utils::packageVersion("zentraR")), error = function(e) "dev")
  paste0(
    "zentraR/", ver,
    " (https://gitlab.com/meter-group-inc/pubpackages/zentraR)"
  )
}

# ---- Request construction ---------------------------------------------------

# Build an httr2 request for the given path segments and query parameters.
# `path` is a character vector of URL path segments (each is encoded
# individually, so device ids containing special characters are handled).
zc_req <- function(path, query = list(), key = NULL) {
  req <- httr2::request(zc_base_url())
  req <- do.call(httr2::req_url_path_append, c(list(req), as.list(path)))
  req <- httr2::req_headers(
    req,
    "X-API-Key" = zc_key(key),
    "Accept" = "application/json",
    .redact = "X-API-Key"
  )
  req <- httr2::req_user_agent(req, zc_user_agent())

  query <- drop_null(query)
  if (length(query)) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  # Rate-limit / transient-failure handling. The v5 API is limited per key
  # (roughly a 5-request burst, then ~1 request/minute) and returns 429 with a
  # "next time to call" the client should wait for. httr2 honours a Retry-After
  # header when present; otherwise we fall back to a capped exponential backoff.
  httr2::req_retry(
    req,
    max_tries = getOption("zentraR.max_tries", 5L),
    is_transient = zc_is_transient,
    after = zc_retry_after,
    backoff = function(i) min(60, 2^i)
  )
}

# Internal state for retry reporting: the last transient status observed.
.zentraR_http <- new.env(parent = emptyenv())

zc_is_transient <- function(resp) {
  status <- httr2::resp_status(resp)
  transient <- status %in% c(429L, 500L, 502L, 503L, 504L)
  if (transient) {
    assign("last_status", status, envir = .zentraR_http)
  }
  transient
}

# Wait precisely as long as the server tells us to. On a 429 the v5 API returns
# the absolute epoch-second time the next request is allowed, embedded in the
# JSON `detail` (there is no Retry-After header). We parse it and return the
# seconds to wait. Returning NA tells httr2 to fall back to `backoff` (used for
# 5xx and any response we can't parse), so this is always safe.
zc_retry_after <- function(resp) {
  detail <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE)$detail,
    error = function(e) NULL
  )
  if (is.null(detail) || !is.character(detail) || length(detail) != 1) {
    return(NA_real_)
  }
  match <- regmatches(detail, regexpr("[0-9]+(\\.[0-9]+)?", detail))
  if (length(match) == 0 || !nzchar(match)) {
    return(NA_real_)
  }
  target <- suppressWarnings(as.numeric(match))
  if (is.na(target)) {
    return(NA_real_)
  }
  wait <- target - as.numeric(Sys.time())
  # Never negative; cap at the GCRA window (300s) so a bad value can't stall us.
  max(0, min(wait, 300))
}

# ---- Perform + parse --------------------------------------------------------

# Perform a request, translating httr2 conditions into typed zentraR errors.
# A calling handler on the `httr2_retry` signal emits a branded, plain-language
# message before each rate-limit / transient backoff (the HTTP client shows the
# live countdown for the wait itself).
zc_perform <- function(req) {
  withCallingHandlers(
    tryCatch(
      httr2::req_perform(req),
      httr2_http = function(cnd) zc_handle_http_error(cnd),
      httr2_failure = function(cnd) {
        cli::cli_abort(
          c(
            "Could not reach ZENTRA Cloud.",
            "i" = "Check your internet connection and try again."
          ),
          class = "zentraR_connection_error",
          parent = cnd
        )
      }
    ),
    httr2_retry = function(cnd) zc_report_retry(cnd)
  )
}

# Branded message shown just before the client waits to retry a transient
# response (most often a 429 rate limit). Honours the progress toggle so
# non-interactive/automated runs stay quiet unless progress is forced on.
zc_report_retry <- function(cnd) {
  if (!zc_use_progress(NULL, active_default = interactive())) {
    return(invisible())
  }
  status <- get0("last_status", envir = .zentraR_http, ifnotfound = NA_integer_)
  reason <- if (isTRUE(status == 429L)) "rate limit reached" else "temporary server issue"
  secs <- tryCatch(round(as.numeric(cnd$delay)), error = function(e) NA_real_)
  cli::cli_alert_info(
    "ZENTRA Cloud {reason}; waiting {secs}s before retrying (attempt {cnd$tries})."
  )
  invisible()
}

# Perform a GET and return the parsed JSON body as a nested list.
zc_get_json <- function(path, query = list(), key = NULL) {
  resp <- zc_perform(zc_req(path, query = query, key = key))
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

# ---- Error handling ---------------------------------------------------------

zc_handle_http_error <- function(cnd) {
  resp <- cnd$resp
  status <- httr2::resp_status(resp)
  detail <- zc_error_detail(resp)

  base_msg <- switch(
    as.character(status),
    "401" = c(
      "ZENTRA Cloud rejected the request: the API key is missing or invalid (401).",
      "i" = "Set a valid key with {.code zc_set_key(\"<your-key>\")}.",
      "i" = "Find your key at {.url https://app.zentracloud.io/profile/integrations}."
    ),
    "403" = c(
      "You are not authorized to access that resource (403).",
      "i" = "Your key is valid but lacks access to this organization or device."
    ),
    "422" = c(
      "ZENTRA Cloud could not process the request parameters (422)."
    ),
    "429" = c(
      "ZENTRA Cloud rate limit exceeded (429).",
      "i" = "The API allows about 5 requests immediately, then ~1 per minute.",
      "i" = "Wait a minute before retrying, or fetch smaller windows / fewer devices."
    ),
    c(sprintf("ZENTRA Cloud request failed (HTTP %s).", status))
  )

  if (!is.null(detail)) {
    base_msg <- c(base_msg, "x" = detail)
  }

  klass <- switch(
    as.character(status),
    "401" = "zentraR_auth_error",
    "403" = "zentraR_auth_error",
    "422" = "zentraR_invalid_request_error",
    "429" = "zentraR_rate_limit_error",
    "zentraR_http_error"
  )

  cli::cli_abort(
    base_msg,
    class = c(klass, "zentraR_http_error"),
    parent = cnd,
    status = status
  )
}

# Try to pull a human-readable message out of a JSON error body. The v5 API
# does not publish a fixed error schema, so we probe the common field names.
zc_error_detail <- function(resp) {
  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(body)) {
    return(NULL)
  }
  if (is.character(body)) {
    return(if (nzchar(body)) body else NULL)
  }
  if (is.list(body)) {
    for (field in c("detail", "message", "error", "errors", "description")) {
      val <- body[[field]]
      if (!is.null(val) && length(val)) {
        return(paste(as.character(unlist(val)), collapse = "; "))
      }
    }
  }
  NULL
}
