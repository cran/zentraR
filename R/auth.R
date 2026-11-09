# Internal, session-scoped store for the API key.
.zentraR_env <- new.env(parent = emptyenv())

#' Set your ZENTRA Cloud API key
#'
#' Stores your ZENTRA Cloud v5 API key so that the other `zc_*()` functions can
#' authenticate. By default the key is kept for the current R session only. Set
#' `install = TRUE` to also save it to your user `.Renviron` file so it is
#' available in every future session.
#'
#' Get your key from ZENTRA Cloud: **your name (top right) -> Integrations ->
#' Show Token** (<https://app.zentracloud.io/profile/integrations>). Treat it
#' like a password. Regenerating it in ZENTRA Cloud immediately invalidates the
#' old key.
#'
#' @param key Your API key, as a string.
#' @param install If `TRUE`, write the key to your user `.Renviron` as
#'   `ZENTRACLOUD_API_KEY` so it persists across sessions. Defaults to `FALSE`.
#' @param overwrite If `TRUE`, replace an existing `ZENTRACLOUD_API_KEY` entry
#'   in `.Renviron` when `install = TRUE`. Defaults to `FALSE`.
#'
#' @return Invisibly, the key.
#' @export
#'
#' @examples
#' \dontrun{
#' # Keep the key for this session only:
#' zc_set_key("my-secret-key")
#'
#' # Persist it across sessions:
#' zc_set_key("my-secret-key", install = TRUE)
#' }
zc_set_key <- function(key, install = FALSE, overwrite = FALSE) {
  if (!is_string(key) || !nzchar(trimws(key))) {
    cli::cli_abort("{.arg key} must be a single non-empty string.")
  }
  key <- trimws(key)
  assign("api_key", key, envir = .zentraR_env)
  Sys.setenv(ZENTRACLOUD_API_KEY = key)

  if (isTRUE(install)) {
    write_key_to_renviron(key, overwrite = overwrite)
  }
  invisible(key)
}

#' Check whether an API key is available
#'
#' @return `TRUE` if a key has been set with [zc_set_key()] or is present in the
#'   `ZENTRACLOUD_API_KEY` environment variable, otherwise `FALSE`.
#' @export
zc_has_key <- function() {
  !is.null(tryCatch(zc_key(), zentraR_auth_error = function(e) NULL))
}

# Resolve the key: explicit argument -> session value -> environment variable.
zc_key <- function(key = NULL) {
  if (!is.null(key)) {
    if (!is_string(key) || !nzchar(trimws(key))) {
      cli::cli_abort("{.arg key} must be a single non-empty string.")
    }
    return(trimws(key))
  }
  session_key <- get0("api_key", envir = .zentraR_env, ifnotfound = NULL)
  if (!is.null(session_key)) {
    return(session_key)
  }
  env_key <- Sys.getenv("ZENTRACLOUD_API_KEY", unset = "")
  if (nzchar(env_key)) {
    return(env_key)
  }
  cli::cli_abort(
    c(
      "No ZENTRA Cloud API key found.",
      "i" = "Call {.code zc_set_key(\"<your-key>\")} to set one,",
      "i" = "or set the {.envvar ZENTRACLOUD_API_KEY} environment variable.",
      "i" = "Find your key at {.url https://app.zentracloud.io/profile/integrations}."
    ),
    class = "zentraR_auth_error"
  )
}

# Append (or replace) the key in the user's .Renviron.
write_key_to_renviron <- function(key, overwrite = FALSE) {
  home <- Sys.getenv("HOME", unset = path.expand("~"))
  renviron <- file.path(home, ".Renviron")

  existing <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
  has_entry <- any(grepl("^\\s*ZENTRACLOUD_API_KEY\\s*=", existing))

  if (has_entry && !overwrite) {
    cli::cli_abort(c(
      "{.envvar ZENTRACLOUD_API_KEY} is already set in {.path {renviron}}.",
      "i" = "Use {.code zc_set_key(key, install = TRUE, overwrite = TRUE)} to replace it."
    ))
  }

  line <- sprintf("ZENTRACLOUD_API_KEY=%s", key)
  if (has_entry) {
    existing[grepl("^\\s*ZENTRACLOUD_API_KEY\\s*=", existing)] <- line
    new_contents <- existing
  } else {
    new_contents <- c(existing, line)
  }
  writeLines(new_contents, renviron)
  cli::cli_inform(c(
    "v" = "Saved {.envvar ZENTRACLOUD_API_KEY} to {.path {renviron}}.",
    "i" = "Restart R or run {.code readRenviron(\"~/.Renviron\")} to use it in new sessions."
  ))
  invisible(renviron)
}
