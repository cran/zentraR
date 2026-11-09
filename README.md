# zentraR

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/zentraR)](https://CRAN.R-project.org/package=zentraR)
<!-- badges: end -->

`zentraR` is an R client for the [ZENTRA Cloud v5 API](https://docs.zentracloud.io).
It gets you from *"I have an API key"* to *"I have a tidy, up-to-date data frame
of my sensor readings"* in a few lines — handling authentication, pagination,
rate limits, and reshaping so you can get straight to analysis.

## Features

- **Device discovery** — `zc_list_devices()` lists every device your key can access.
- **Tidy readings** — `zc_get_readings()` returns one row per measurement, ready
  for `dplyr` / `ggplot2`, with a `zc_pivot_wider()` helper for a spreadsheet layout.
- **Automatic pagination & rate-limit handling** — large pulls just work; the
  client paces itself and retries on throttling.
- **Incremental sync with pluggable storage** — `zc_sync()` fetches only what's
  new and appends it to your store of choice: native R files (`zc_store_rds()`),
  plain CSV (`zc_store_csv()`), or nothing at all (return-only, for loading into
  your own database).
- **Schedule-friendly** — run `zc_sync()` on file open, daily, or weekly. See the
  *Scheduling automatic syncs* vignette.

## Installation

Install the released version from CRAN:

```r
install.packages("zentraR")
```

Or install the development version from the METER Group public packages group on
GitLab:

```r
# install.packages("remotes")
remotes::install_gitlab("meter-group-inc/pubpackages/zentraR")
```

## Authentication

Get your API key from ZENTRA Cloud: **your name (top right) → Integrations →
Show Token** (<https://app.zentracloud.io/profile/integrations>). Then either
set it for the session, or save it to your `.Renviron` so it's always available:

```r
library(zentraR)

zc_set_key("your-api-key")                 # this session only
zc_set_key("your-api-key", install = TRUE) # persist across sessions
```

## Quick start

```r
library(zentraR)

# 1. What devices can I see?
devices <- zc_list_devices(expand = "max_min_timestamp")
devices

# 2. Pull the last week of readings for one device (tidy long format).
readings <- zc_get_readings("z6-00930", start = Sys.Date() - 7)
readings

# 3. Reshape to one column per measurement.
zc_pivot_wider(readings)

# 4. Add human-readable quality flags.
zc_label_errors(readings)
```

## Keeping a local record up to date

`zc_sync()` remembers what you already have and fetches only newer readings, so
you can run it on a routine. Pick where the data lives:

```r
# Persist as CSV (accessible to non-R tools):
store <- zc_store_csv("data/zentra")

# First run backfills history; later runs fetch only what's new:
zc_sync("z6-00930", store = store, start = Sys.Date() - 30)
zc_sync("z6-00930", store = store)   # incremental

# Sync every device your key can access:
zc_sync(store = store)

# Read your accumulated data back:
zc_store_read(store)
```

Prefer native R objects for an RStudio project? Use `zc_store_rds("data/zentra")`.
Piping into your own database? Pass `store = NULL` and `zc_sync()` simply returns
the new readings.

## Rate limits

The v5 API is limited per key: roughly a 5-request burst, then about one request
per minute. `zentraR` respects this automatically (pacing and retrying), but very
large historical backfills will take time. Because `zc_sync()` is incremental and
resumable, routine top-ups stay well within the limit.

## Learn more

Built-in vignettes:

```r
vignette("getting-started", package = "zentraR")
vignette("working-with-data", package = "zentraR")
vignette("scheduling", package = "zentraR")
```

Online (kept current with the API) on ZENTRA Cloud:

- [Getting Started with zentraR](https://docs.zentracloud.io/l/en/article/j1ejykpvpv-getting-started-with-zentra-r)
- [Working with Your Data in R](https://docs.zentracloud.io/l/en/article/bohtqxbqrm-working-with-your-data-in-r)
- [Scheduling Automatic Syncs](https://docs.zentracloud.io/l/en/article/f2g5u9hc2r-scheduling-automatic-syncs)

## Development

The test suite runs against mocked responses, so `R CMD check` needs no network
and no API key.

One file is different: `tests/testthat/test-live-contract.R` calls the real v5
API and asserts the response shape the parsers depend on, so that an upstream
change is caught by CI rather than by users. It is skipped unless you opt in:

```sh
export ZENTRACLOUD_API_KEY=your-key
export ZENTRAR_TEST_DEVICE_SN=z6-00930
export ZENTRAR_LIVE_TESTS=true
Rscript -e 'devtools::test(filter = "live-contract")'
```

In CI these run as the scheduled `api-contract` job; see `.gitlab-ci.yml`.
That job also sets `ZENTRAR_REQUIRE_LIVE=true`, which turns a missing key into a
failure rather than a skip, so a credential problem can't masquerade as a pass.

## License

MIT © METER Group, Inc.
