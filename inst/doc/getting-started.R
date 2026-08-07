## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(zentraR)

## -----------------------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_gitlab("meter-group-inc/pubpackages/zentraR")
# 
# # ...or pin a specific version:
# remotes::install_gitlab("meter-group-inc/pubpackages/zentraR@v0.1.0")

## -----------------------------------------------------------------------------
# install.packages(c("httr2", "cli", "rlang", "tibble", "tidyr", "vctrs"))
# install.packages("zentraR_0.1.0.tar.gz", repos = NULL, type = "source")

## -----------------------------------------------------------------------------
# library(zentraR)

## -----------------------------------------------------------------------------
# # This session only:
# zc_set_key("your-api-token")
# 
# # Persist to ~/.Renviron so every future session finds it automatically:
# zc_set_key("your-api-token", install = TRUE)

## -----------------------------------------------------------------------------
# devices <- zc_list_devices()
# devices

## -----------------------------------------------------------------------------
# zc_list_devices(expand = c("max_min_timestamp", "settings"))

## -----------------------------------------------------------------------------
# readings <- zc_get_readings("z6-00930", start = Sys.Date() - 7)
# readings

## -----------------------------------------------------------------------------
# zc_get_readings(
#   "z6-00930",
#   start = as.POSIXct("2026-05-01", tz = "UTC"),
#   end   = as.POSIXct("2026-06-01", tz = "UTC")
# )

## -----------------------------------------------------------------------------
# zc_get_readings("z6-00930", start = Sys.Date() - 1, units = "imperial", direction = "descending")

## -----------------------------------------------------------------------------
# zc_label_errors(readings)
# zc_error_codes()

## -----------------------------------------------------------------------------
# library(dplyr)
# library(ggplot2)
# 
# readings |>
#   filter(measurement == "Air Temperature", error_code == 0) |>
#   ggplot(aes(datetime, value)) +
#   geom_line() +
#   labs(y = unique(readings$unit[readings$measurement == "Air Temperature"]))

## -----------------------------------------------------------------------------
# zc_pivot_wider(readings)

## -----------------------------------------------------------------------------
# store <- zc_store_csv("data/zentra")
# 
# # First run — backfill from `start`, or from each device's first measurement if omitted:
# zc_sync("z6-00930", store = store, start = Sys.Date() - 30)
# 
# # Later runs — only what's new since last time:
# zc_sync("z6-00930", store = store)
# 
# # Every device your token can access, in one call:
# zc_sync(store = store)
# 
# # Read the accumulated record back into R:
# all_data <- zc_store_read(store)

## -----------------------------------------------------------------------------
# vignette("working-with-data", package = "zentraR")

## -----------------------------------------------------------------------------
# vignette("scheduling", package = "zentraR")

