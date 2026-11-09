# zentraR 0.1.4

* `zc_get_readings(window = "24h")` requests a relative time range measured back
  from the moment of the request, using the readings endpoint's new `window`
  parameter. Write it as a whole number plus `m`, `h`, `d`, or `w`; it must
  resolve to one year or less (`"365d"` or `"52w"`), which is checked locally
  before the request is sent. It is an alternative to `start`/`end` and
  `start_timestamp`/`end_timestamp`, not a supplement — supplying more than one
  form is an error.

* The `zc_set_key()` help page and the README now give the real menu path to
  your API token in ZENTRA Cloud: select your name at the top right, then
  **Integrations**, then **Show Token**. They previously named a
  "User Account" menu item that does not exist.

# zentraR 0.1.3

* Documentation and metadata polish for the first CRAN submission (title
  casing, an updated external documentation link). No user-facing code changes.

# zentraR 0.1.2

* The interactive progress indicator now advances *during* the parsing of large
  pages: the running reading count climbs as records are processed, so a big
  single-device fetch is visibly working instead of appearing frozen between
  pages.

# zentraR 0.1.1

* On a rate-limit `429`, wait precisely until the server's advertised
  next-allowed time (parsed from the response) instead of using blind
  exponential backoff, so throttled fetches resume as early as possible.
* `zc_sync(max_active = N)` fetches up to N devices concurrently — safe and much
  faster because the v5 readings rate limit is enforced *per device*. A
  round-based scheduler backs off and retries any individually rate-limited
  device without blocking the others. Defaults to `1` (sequential).

# zentraR 0.1.0

* First release.
* Device discovery via `zc_list_devices()` (auto-paginated, optional `expand` metadata).
* Reading retrieval via `zc_get_readings()`: tidy long output, automatic `next_token`
  pagination, flexible UTC time windows, and rate-limit-aware retries. A live progress
  indicator (spinner with page/reading counts) is shown in interactive sessions; control
  it with the `progress` argument or `options(zentraR.progress = )`.
* `zc_pivot_wider()`, `zc_error_codes()`, and `zc_label_errors()` helpers.
* Incremental `zc_sync()` with pluggable stores: `zc_store_rds()`, `zc_store_csv()`,
  `zc_store_memory()`, or return-only (`store = NULL`). Shows a live per-device
  progress indicator in interactive sessions (`progress` argument to control it).
* Rate-limit / transient backoffs print a branded, plain-language message (e.g.
  "ZENTRA Cloud rate limit reached; waiting 60s before retrying") alongside the
  client's live countdown.
* Authentication via `zc_set_key()` / the `ZENTRACLOUD_API_KEY` environment variable.
* Vignettes: "Getting Started with zentraR", "Working with Your Data in R", and
  "Scheduling Automatic Syncs".
