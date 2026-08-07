## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# #!/usr/bin/env Rscript
# # sync_zentra.R — fetch new ZENTRA Cloud readings into a local store.
# 
# library(zentraR)
# 
# # The token comes from the ZENTRACLOUD_API_KEY environment variable
# # (set it in ~/.Renviron, or via zc_set_key(..., install = TRUE) once).
# 
# store <- zc_store_csv("data/zentra")
# 
# summary <- zc_sync(store = store, quiet = TRUE)
# 
# message(
#   format(Sys.time()), "  synced ", nrow(summary), " device(s), ",
#   sum(summary$rows_added), " new reading(s)"
# )

## -----------------------------------------------------------------------------
# # .Rprofile
# setHook("rstudio.sessionInit", function(newSession) {
#   if (newSession && nzchar(Sys.getenv("ZENTRACLOUD_API_KEY"))) {
#     message("Refreshing ZENTRA Cloud data in the background…")
#     system2("Rscript", "sync_zentra.R", wait = FALSE)
#   }
# }, action = "append")

## -----------------------------------------------------------------------------
# if (interactive() && nzchar(Sys.getenv("ZENTRACLOUD_API_KEY"))) {
#   try(source("sync_zentra.R"))
# }

## -----------------------------------------------------------------------------
# # install.packages("cronR")
# library(cronR)
# 
# cmd <- cron_rscript(normalizePath("sync_zentra.R"))
# cron_add(cmd, frequency = "daily", at = "06:00", id = "zentra_sync",
#          description = "Daily ZENTRA Cloud sync")
# 
# # Manage it later:
# cron_ls()
# # cron_rm("zentra_sync")

## -----------------------------------------------------------------------------
# # install.packages("taskscheduleR")
# library(taskscheduleR)
# 
# taskscheduler_create(
#   taskname = "zentra_sync",
#   rscript  = normalizePath("sync_zentra.R"),
#   schedule = "DAILY",
#   starttime = "06:00"
# )
# 
# # Remove it later:
# # taskscheduler_delete("zentra_sync")

