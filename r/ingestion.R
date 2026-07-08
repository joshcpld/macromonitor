################################################################################
# R/ingestion.R
# Macro Monitor — data fetching and CSV cache
#
# One CSV is written per country (e.g. data/australia.csv) containing every
# series for that country.  load_cache() reads these files and splits the data
# by chart_id so the server can access each chart's data as a named list.
#
# Supported api_source values:
#   readabs  — ABS catalogue data (series_id preferred; falls back to cat_no+filter)
#   readrba  — RBA statistical tables (requires readrba package)
#   fredr    — FRED via api_code = FRED series ID (requires fredr + FRED_API_KEY)
#
# FRED setup: register for a free key at https://fred.stlouisfed.org/docs/api/api_key.html
#   then run: fredr::fredr_set_key("your_key")  OR set env var FRED_API_KEY.
#
# Public functions:
#   refresh_data()       — downloads all series, writes data/<country>.csv
#   load_cache()         — reads country CSVs, returns named list by chart_id
#   cache_last_updated() — returns human-readable timestamp string
################################################################################

library(readabs)

# ─── Internal: fetch a single FRED series ────────────────────────────────────

.fetch_fred <- function(row) {
  if (!requireNamespace("fredr", quietly = TRUE)) {
    message("  fredr not installed — skipping ", row$chart_id,
            "\n  Install with: install.packages('fredr')")
    return(NULL)
  }
  key <- Sys.getenv("FRED_API_KEY")
  if (nchar(key) == 0) {
    message("  FRED_API_KEY not set — skipping ", row$api_code,
            "\n  Run: fredr::fredr_set_key('your_key')")
    return(NULL)
  }
  message("  Fetching FRED series ", row$api_code, " for ", row$chart_id, "...")
  df <- tryCatch(
    fredr::fredr(series_id = row$api_code),
    error = function(e) { message("    ERROR: ", conditionMessage(e)); NULL }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df %>%
    select(date, value) %>%
    filter(!is.na(value)) %>%
    mutate(
      series_name = row$series_name,
      series_role = row$series_role,
      chart_id    = row$chart_id,
      country     = row$country
    )
}

# ─── Internal: fetch a single fallback row (no series_id) ────────────────────

.fetch_fallback <- function(row) {
  message("  Fetching ", row$chart_id, " via catalogue ", row$api_code,
          " table ", row$table_no, "...")
  df <- tryCatch(
    read_abs(cat_no = row$api_code, tables = as.integer(row$table_no)) %>%
      filter(
        series_type == "Seasonally Adjusted",
        str_detect(series, regex(row$series_filter, ignore_case = TRUE))
      ),
    error = function(e) {
      message("    ERROR: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0) {
    message("    No match for series_filter '", row$series_filter, "'")
    return(NULL)
  }
  best_id <- df %>%
    count(series_id) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    pull(series_id)

  df %>%
    filter(series_id == best_id) %>%
    select(date, value) %>%
    mutate(
      series_name = row$series_name,
      series_role = row$series_role,
      chart_id    = row$chart_id,
      country     = row$country
    )
}

# ─── Internal: fetch a single RBA row ────────────────────────────────────────

.fetch_rba <- function(row) {
  if (!requireNamespace("readrba", quietly = TRUE)) {
    message("  readrba not installed — skipping ", row$chart_id)
    return(NULL)
  }
  message("  Fetching RBA table ", row$api_code, " for ", row$chart_id, "...")
  df <- tryCatch(
    readrba::read_rba(table_no = row$api_code),
    error = function(e) {
      message("    ERROR: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df %>%
    filter(str_detect(series_name, regex(row$series_filter, ignore_case = TRUE))) %>%
    select(date = publication_date, value) %>%
    mutate(
      series_name = row$series_name,
      series_role = row$series_role,
      chart_id    = row$chart_id,
      country     = row$country
    )
}

# ─── refresh_data() ──────────────────────────────────────────────────────────

refresh_data <- function(cfg = config, cache_dir = "data") {
  if (!dir.exists(cache_dir)) dir.create(cache_dir)

  # Remove any existing country/chart CSVs so stale files don't linger
  old_files <- list.files(cache_dir, pattern = "\\.csv$", full.names = TRUE)
  old_files <- old_files[!grepl("last_updated\\.csv$", old_files)]
  if (length(old_files) > 0) file.remove(old_files)

  rows <- cfg %>% filter(series_role != "residual")

  ##############################################################################
  # 1. readabs with confirmed series_id — batch fetch
  ##############################################################################

  abs_id_rows <- rows %>%
    filter(api_source == "readabs", !is.na(series_id), series_id != "")

  id_data <- tibble()

  if (nrow(abs_id_rows) > 0) {
    unique_ids <- unique(abs_id_rows$series_id)
    message("Batch-fetching ", length(unique_ids), " series by ID from readabs...")
    raw <- tryCatch(
      read_abs(series_id = unique_ids),
      error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL }
    )
    if (!is.null(raw)) {
      # Deduplicate: readabs can return >1 row per (series_id, date) when a series
      # appears across multiple tables/sheets (SA + Trend + Original in same file).
      id_data <- raw %>%
        select(series_id, date, value) %>%
        distinct(series_id, date, .keep_all = TRUE) %>%
        inner_join(
          abs_id_rows %>% select(series_id, series_name, series_role, chart_id, country),
          by           = "series_id",
          relationship = "many-to-many"
        )
      message("  Got ", nrow(id_data), " observations across ",
              n_distinct(id_data$chart_id), " charts.")
    }
  }

  ##############################################################################
  # 2. readabs fallback — catalogue + table + series_filter
  ##############################################################################

  abs_fallback_rows <- rows %>%
    filter(api_source == "readabs", is.na(series_id) | series_id == "",
           !is.na(api_code), api_code != "")

  fallback_data <- if (nrow(abs_fallback_rows) > 0) {
    message("Fetching ", nrow(abs_fallback_rows), " series via catalogue fallback...")
    map_dfr(split(abs_fallback_rows, seq_len(nrow(abs_fallback_rows))), .fetch_fallback)
  } else {
    tibble()
  }

  ##############################################################################
  # 3. readrba
  ##############################################################################

  rba_rows <- rows %>% filter(api_source == "readrba", !is.na(api_code))

  rba_data <- if (nrow(rba_rows) > 0) {
    map_dfr(split(rba_rows, seq_len(nrow(rba_rows))), .fetch_rba)
  } else {
    tibble()
  }

  ##############################################################################
  # 4. fredr (FRED — Federal Reserve Economic Data)
  ##############################################################################

  fred_rows <- rows %>% filter(api_source == "fredr", !is.na(api_code), api_code != "")

  fred_data <- if (nrow(fred_rows) > 0) {
    message("Fetching ", nrow(fred_rows), " series from FRED...")
    map_dfr(split(fred_rows, seq_len(nrow(fred_rows))), .fetch_fred)
  } else {
    tibble()
  }

  ##############################################################################
  # 5. Combine and write ONE CSV per country
  ##############################################################################

  all_data <- bind_rows(id_data, fallback_data, rba_data, fred_data)

  if (nrow(all_data) == 0) {
    message("No data fetched — nothing cached.")
    return(invisible(NULL))
  }

  walk(unique(all_data$country), function(ctry) {
    fname <- ctry %>% str_to_lower() %>% str_replace_all("\\s+", "_")
    path  <- file.path(cache_dir, paste0(fname, ".csv"))

    all_data %>%
      filter(country == ctry) %>%
      select(date, value, series_name, series_role, chart_id) %>%
      arrange(chart_id, date) %>%
      write_csv(path)

    n_charts <- n_distinct(filter(all_data, country == ctry)$chart_id)
    message("  Wrote ", ctry, ": ", n_charts, " charts → ", path)
  })

  write_csv(
    tibble(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    file.path(cache_dir, "last_updated.csv")
  )

  message("Refresh complete.")
  invisible(all_data)
}

# ─── load_cache() ────────────────────────────────────────────────────────────
# Reads all country CSVs from data/ and returns a named list keyed by chart_id.
# This maintains the live_data()[[chart_id]] interface in server.R.

load_cache <- function(cache_dir = "data") {
  files <- list.files(cache_dir, pattern = "\\.csv$", full.names = TRUE)
  files <- files[!grepl("last_updated\\.csv$", files)]
  if (length(files) == 0) return(list())

  all_data <- map_dfr(files, ~read_csv(.x, show_col_types = FALSE))
  if (nrow(all_data) == 0) return(list())

  split(all_data, all_data$chart_id)
}

# ─── cache_last_updated() ────────────────────────────────────────────────────

cache_last_updated <- function(cache_dir = "data") {
  path <- file.path(cache_dir, "last_updated.csv")
  if (!file.exists(path)) return("Never")
  ts <- read_csv(path, show_col_types = FALSE)$updated_at[1]
  format(as.POSIXct(ts), "%d %b %Y, %H:%M")
}
