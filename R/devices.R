zc_expand_choices <- c(
  "max_min_timestamp", "settings", "hardware", "connectivity", "subscription"
)

#' List the devices your API key can access
#'
#' Returns one row per device the authenticated key is allowed to see. Use this
#' to discover device ids before calling [zc_get_readings()] or [zc_sync()]. All
#' pages are fetched automatically.
#'
#' @param organization_id Optional organization id (UUID string) to restrict the
#'   list to a single organization.
#' @param expand Optional character vector of extra detail to attach to each
#'   device. Any of `"max_min_timestamp"` (first/last measurement times),
#'   `"settings"`, `"hardware"`, `"connectivity"`, `"subscription"`. Adds extra
#'   columns to the result.
#' @param limit Devices requested per API page (1-1000, default 1000). Affects
#'   only the number of underlying requests, not the returned data.
#' @param max_pages Maximum number of pages to fetch. Defaults to `Inf` (all).
#' @param key Optional API key. Defaults to the key set with [zc_set_key()] or
#'   the `ZENTRACLOUD_API_KEY` environment variable.
#'
#' @return A [tibble][tibble::tibble] with one row per device. Always includes
#'   `device_id`, `name`, `organization_id`, `organization_name`,
#'   `device_type`, and `model`. Requesting `expand` adds further columns (for
#'   example `first_measurement` and `last_measurement` as [POSIXct][DateTimeClasses]
#'   times when `"max_min_timestamp"` is requested).
#' @export
#'
#' @examples
#' \dontrun{
#' zc_set_key("my-secret-key")
#' zc_list_devices()
#' zc_list_devices(expand = "max_min_timestamp")
#' }
zc_list_devices <- function(organization_id = NULL,
                            expand = NULL,
                            limit = 1000L,
                            max_pages = Inf,
                            key = NULL) {
  expand_str <- zc_validate_expand(expand)
  if (!is.numeric(limit) || length(limit) != 1 || limit < 1 || limit > 1000) {
    cli::cli_abort(
      "{.arg limit} must be a single number between 1 and 1000.",
      class = "zentraR_invalid_request_error"
    )
  }

  devices <- list()
  page_num <- 1L
  pages_fetched <- 0L

  repeat {
    query <- drop_null(list(
      organization_id = organization_id,
      expand = expand_str,
      page_num = page_num,
      limit = as.integer(limit)
    ))
    body <- zc_get_json("devices", query = query, key = key)

    devices <- c(devices, body$devices %||% list())
    pages_fetched <- pages_fetched + 1L

    total_pages <- body$pagination$total_pages %||% 1L
    if (page_num >= total_pages || pages_fetched >= max_pages) {
      break
    }
    page_num <- page_num + 1L
  }

  build_devices_tibble(devices)
}

zc_validate_expand <- function(expand) {
  if (is.null(expand)) {
    return(NULL)
  }
  expand <- unlist(strsplit(as.character(expand), ","))
  expand <- trimws(expand)
  expand <- expand[nzchar(expand)]
  bad <- setdiff(expand, zc_expand_choices)
  if (length(bad)) {
    cli::cli_abort(
      c(
        "Invalid {.arg expand} value{?s}: {.val {bad}}.",
        "i" = "Allowed values are {.val {zc_expand_choices}}."
      ),
      class = "zentraR_invalid_request_error"
    )
  }
  paste(unique(expand), collapse = ",")
}

# Flatten a list of device objects into a tibble, adding columns for whichever
# expand groups are present in the response.
build_devices_tibble <- function(devices) {
  if (length(devices) == 0) {
    return(tibble::tibble(
      device_id = character(),
      name = character(),
      organization_id = character(),
      organization_name = character(),
      device_type = integer(),
      model = character()
    ))
  }

  out <- tibble::tibble(
    device_id = vapply(devices, p_chr, character(1), "device_id"),
    name = vapply(devices, p_chr, character(1), "name"),
    organization_id = vapply(devices, p_chr, character(1), "organization_id"),
    organization_name = vapply(devices, p_chr, character(1), "organization_name"),
    device_type = vapply(devices, p_int, integer(1), "device_type"),
    model = vapply(devices, p_chr, character(1), "model")
  )

  has_group <- function(group) {
    any(vapply(devices, function(d) !is.null(d[[group]]), logical(1)))
  }
  sub <- function(group, field, fn = p_chr) {
    vapply(devices, function(d) fn(d[[group]] %||% list(), field), fn(list(), field))
  }

  if (has_group("data_range")) {
    out$first_measurement <- zc_epoch_to_posixct(sub("data_range", "first_measurement", p_dbl))
    out$last_measurement <- zc_epoch_to_posixct(sub("data_range", "last_measurement", p_dbl))
  }
  if (has_group("settings")) {
    out$logging_interval <- sub("settings", "logging_interval", p_int)
    out$upload_frequency <- sub("settings", "upload_frequency", p_int)
    out$upload_hours_enabled_count <- vapply(
      devices,
      function(d) p_int(d$settings$upload_hours %||% list(), "enabled_count"),
      integer(1)
    )
    out$upload_hours_all_hours <- vapply(
      devices,
      function(d) p_lgl(d$settings$upload_hours %||% list(), "all_hours"),
      logical(1)
    )
  }
  if (has_group("hardware")) {
    out$firmware_version <- sub("hardware", "firmware_version", p_dbl)
    out$hardware_revision <- sub("hardware", "hardware_revision", p_chr)
  }
  if (has_group("connectivity")) {
    out$connectivity_type <- sub("connectivity", "connectivity_type", p_chr)
    out$module <- sub("connectivity", "module", p_chr)
    out$module_code <- sub("connectivity", "module_code", p_chr)
    out$sim <- sub("connectivity", "sim", p_chr)
    out$host_server <- sub("connectivity", "host_server", p_chr)
    out$pending_host_server <- sub("connectivity", "pending_host_server", p_chr)
  }
  if (has_group("subscription")) {
    out$subscription_status <- sub("subscription", "status", p_chr)
    out$exempt_reason <- sub("subscription", "exempt_reason", p_chr)
    out$expiration_date <- zc_epoch_to_posixct(sub("subscription", "expiration_date", p_dbl))
    out$auto_renew <- sub("subscription", "auto_renew", p_lgl)
    out$high_data <- sub("subscription", "high_data", p_lgl)
  }

  out
}
