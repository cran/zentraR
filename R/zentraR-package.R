#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||% is_string is_bool
## usethis namespace: end
NULL

# Default options, set on load unless the user has already configured them.
.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    zentraR.base_url = "https://api.zentracloud.io/v5"
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
  invisible()
}
