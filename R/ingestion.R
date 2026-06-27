################################################################################
# R/ingestion.R
# Phase 2–3 — data fetching and caching layer
#
# Sourced from server.R once readabs / readrba packages are installed.
# Pattern: check local cache first; fetch and persist on miss.
################################################################################

# library(readabs)
# library(readrba)

# ── Cache helpers ─────────────────────────────────────────────────────────────

# Return a sanitised file path for a given cache key
cache_path <- function(key, dir = "data") {  file.path(dir, paste0(stringr::str_replace_all(key, "[^a-z0-9]", "_"), ".rds"))
}

# Load cached RDS or run fetch_fn() and persist the result
load_or_cache <- function(key, fetch_fn, force_refresh = FALSE, dir = "data") {
  path <- cache_path(key, dir)
  if (!force_refresh && file.exists(path)) {
    readRDS(path)
  } else {
    message("Fetching: ", key)
    dat <- fetch_fn()
    saveRDS(dat, path)
    dat
  }
}

# ── ABS ingestion (readabs) ───────────────────────────────────────────────────

# fetch_abs <- function(cat_no) {
#   load_or_cache(
#     key      = paste0("abs_", cat_no),
#     fetch_fn = function() readabs::read_abs(cat_no = cat_no)
#   )
# }

# ── RBA ingestion (readrba) ───────────────────────────────────────────────────

# fetch_rba <- function(table_no) {
#   load_or_cache(
#     key      = paste0("rba_", table_no),
#     fetch_fn = function() readrba::read_rba(table_no = table_no)
#   )
# }

# ── Dispatcher: route each config row to the correct source ──────────────────

# ingest_all <- function(cfg = config) {
#   cfg %>%
#     dplyr::distinct(api_source, api_code) %>%
#     purrr::pmap(function(api_source, api_code, ...) {
#       if (api_source == "readabs") fetch_abs(api_code)
#       else if (api_source == "readrba") fetch_rba(api_code)
#     })
# }

# ── Filtering helpers (Phase 3) ───────────────────────────────────────────────

# Keep only Trend or Seasonally Adjusted series; standardise date column name
# tidy_abs <- function(df) {
#   df %>%
#     dplyr::filter(series_type %in% c("Trend", "Seasonally Adjusted")) %>%
#     dplyr::rename(date = dplyr::any_of(c("date", "observation_date"))) %>%
#     dplyr::select(date, series, value, series_type, table_title)
# }

# tidy_rba <- function(df) {
#   df %>%
#     dplyr::rename(date = dplyr::any_of(c("date", "observation_date"))) %>%
#     dplyr::select(date, series_description, value, unit)
# }
