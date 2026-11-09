## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(zentraR)
# readings <- zc_get_readings("z6-00930", start = Sys.Date() - 7)

## -----------------------------------------------------------------------------
# # Prints once, then lost:
# zc_pivot_wider(readings)
# 
# # Saved as `wide` — now you can view it, filter it, plot it, or export it:
# wide <- zc_pivot_wider(readings)
# wide

## -----------------------------------------------------------------------------
# readings                 # a tibble: prints the first 10 rows and the column types
# View(readings)           # open the RStudio spreadsheet viewer (capital V)
# head(readings, 20)       # first 20 rows;  tail(readings) for the last few
# str(readings)            # structure: every column and its type
# dplyr::glimpse(readings) # a tidy, transposed overview
# summary(readings)        # quick per-column statistics
# dim(readings)            # number of rows and columns;  nrow() / ncol()
# names(readings)          # the column names

## -----------------------------------------------------------------------------
# unique(readings$measurement)   # which measurements are present
# table(readings$measurement)    # how many readings of each
# unique(readings$device_id)     # which devices
# range(readings$datetime)       # earliest and latest timestamp

## -----------------------------------------------------------------------------
# library(dplyr)
# 
# # Keep only valid air-temperature readings:
# readings |> filter(measurement == "Air Temperature", error_code == 0)
# 
# # Highest values first:
# readings |> arrange(desc(value))

## -----------------------------------------------------------------------------
# readings[readings$measurement == "Air Temperature", ]

## -----------------------------------------------------------------------------
# mean(readings$value, na.rm = TRUE)   # na.rm = TRUE ignores missing values
# 
# # Average and count per measurement:
# readings |>
#   group_by(measurement) |>
#   summarise(avg = mean(value, na.rm = TRUE), n = n())

## -----------------------------------------------------------------------------
# plot(readings$datetime, readings$value, type = "l")

## -----------------------------------------------------------------------------
# # CSV — opens in Excel / Google Sheets, easy to share:
# write.csv(readings, "readings.csv", row.names = FALSE)
# 
# # RDS — an exact copy of the R object (types preserved); reload with readRDS():
# saveRDS(readings, "readings.rds")
# readings <- readRDS("readings.rds")

## -----------------------------------------------------------------------------
# ?zc_get_readings                 # the help page for any function
# vignette(package = "zentraR")    # list this package's guides

## -----------------------------------------------------------------------------
# vignette("getting-started", package = "zentraR")
# vignette("scheduling", package = "zentraR")

