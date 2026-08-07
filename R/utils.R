# Drop NULL entries from a list (used to omit unset query parameters).
drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# Safe field extractors for JSON objects parsed with simplifyVector = FALSE.
# Each returns a length-1 typed value, substituting NA when the field is
# absent or JSON null.
p_chr <- function(x, field) {
  v <- x[[field]]
  if (is.null(v)) NA_character_ else as.character(v)[[1]]
}

p_dbl <- function(x, field) {
  v <- x[[field]]
  if (is.null(v)) NA_real_ else as.numeric(v)[[1]]
}

p_int <- function(x, field) {
  v <- x[[field]]
  if (is.null(v)) NA_integer_ else as.integer(v)[[1]]
}

p_lgl <- function(x, field) {
  v <- x[[field]]
  if (is.null(v)) NA else as.logical(v)[[1]]
}

# Decide whether to show a progress indicator. An explicit `progress` argument
# wins; otherwise fall back to the `zentraR.progress` option, defaulting to
# `active_default` (typically "interactive session and not already reporting").
zc_use_progress <- function(progress, active_default) {
  if (!is.null(progress)) {
    return(isTRUE(progress))
  }
  isTRUE(getOption("zentraR.progress", active_default))
}

# Validate a scalar value against an allowed set, with a clear error.
zc_match_arg <- function(value, choices, arg = rlang::caller_arg(value)) {
  if (!is_string(value) || !(value %in% choices)) {
    cli::cli_abort(
      "{.arg {arg}} must be one of {.val {choices}}, not {.val {value}}.",
      class = "zentraR_invalid_request_error"
    )
  }
  value
}
