################################################################################
# global.R
# Macro Monitor — packages, configuration, and shared utilities
################################################################################

library(shiny)
library(bslib)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(lubridate)

# Transformation definitions and Phase 3 implementation stubs
source("R/transformations.R")

################################################################################
# Configuration
#
# One CSV per country lives in config/.  Drop a new file there to add a tab —
# no code changes required.  Files are read in alphabetical order; prefix with
# numbers (01_australia.csv, 02_usa.csv …) to control tab order.
#
# CSV columns (country is injected automatically from the filename):
#   page            — LHS nav item
#   sub_section     — section header within page
#   chart_id        — unique chart ID; MULTIPLE ROWS sharing chart_id → multi-line
#   chart_title     — Plotly chart title
#   series_name     — legend label per line (= chart_title for single-line charts)
#   api_source      — "readabs" or "readrba"  (blank for residual rows)
#   api_code        — ABS catalog number or RBA table code  (blank for residual rows)
#   frequency       — "monthly" or "quarterly"
#   transform_type  — "standard" or "additive_decomp"  (see R/transformations.R)
#   series_role     — "series" | "aggregate" | "component" | "residual"
################################################################################

config <- list.files("config", pattern = "\\.csv$", full.names = TRUE) %>%
  map_dfr(function(path) {
    # Derive tab label from filename: "01_australia.csv" → "Australia"
    country_name <- basename(path) %>%
      str_remove("\\.csv$") %>%
      str_remove("^[0-9]+_?") %>%
      str_replace_all("_", " ") %>%
      str_to_title()

    read_csv(path, show_col_types = FALSE) %>%
      mutate(country = country_name)
  }) %>%
  mutate(
    default_transform = "yoy",
    chart_id = chart_id %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]", "_") %>%
      str_replace_all("_+", "_") %>%
      str_remove_all("^_|_$")
  )

# ── Utilities ─────────────────────────────────────────────────────────────────

to_chart_id <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove_all("^_|_$")
}

################################################################################
# Phase 1 — mock data and placeholder chart helpers
################################################################################

mock_series <- function(n = 120, seed = 1, drift = 0, vol = 0.4) {
  set.seed(seed)
  dates <- seq(
    floor_date(Sys.Date(), "month") %m-% months(n - 1),
    floor_date(Sys.Date(), "month"),
    by = "month"
  )
  tibble(date = dates, value = round(cumsum(rnorm(n, drift, vol)), 3))
}

# Alias — server.R calls apply_transform; canonical definition is in transformations.R
apply_transform <- apply_standard_transform

# ── Colour palette (shared across single & multi-series) ─────────────────────

series_colours <- c("#1565C0", "#FF8F00", "#43A047", "#E53935", "#8E24AA", "#00ACC1")

# ── Single-series line chart ──────────────────────────────────────────────────

placeholder_chart <- function(label, df = NULL, seed = 1) {
  if (is.null(df)) df <- mock_series(seed = seed)

  plot_ly(
    df, x = ~date, y = ~value,
    type          = "scatter",
    mode          = "lines",
    line          = list(color = series_colours[1], width = 1.8),
    hovertemplate = "%{x|%b %Y}:  %{y:.2f}<extra></extra>"
  ) %>%
    layout(
      title         = list(text = label, font = list(size = 13, color = "#212121"),
                           x = 0.02, y = 0.97),
      xaxis         = list(title = "", showgrid = FALSE, zeroline = FALSE),
      yaxis         = list(title = "", gridcolor = "#EEEEEE", zeroline = FALSE,
                           tickfont = list(size = 11)),
      plot_bgcolor  = "#FAFAFA",
      paper_bgcolor = "white",
      hovermode     = "x unified",
      margin        = list(l = 45, r = 15, t = 38, b = 30)
    ) %>%
    config(displayModeBar = FALSE)
}

# ── Multi-series line chart ───────────────────────────────────────────────────
# df must have columns: date, value, series (factor or character)

placeholder_multiseries_chart <- function(label, df) {
  n_s <- n_distinct(df$series)

  plot_ly(
    df, x = ~date, y = ~value, color = ~series,
    colors        = series_colours[seq_len(n_s)],
    type          = "scatter",
    mode          = "lines",
    line          = list(width = 1.8),
    hovertemplate = "%{x|%b %Y}:  %{y:.2f}<extra>%{fullData.name}</extra>"
  ) %>%
    layout(
      title         = list(text = label, font = list(size = 13, color = "#212121"),
                           x = 0.02, y = 0.97),
      xaxis         = list(title = "", showgrid = FALSE, zeroline = FALSE),
      yaxis         = list(title = "", gridcolor = "#EEEEEE", zeroline = FALSE,
                           tickfont = list(size = 11)),
      legend        = list(orientation = "h", y = -0.2, font = list(size = 11)),
      plot_bgcolor  = "#FAFAFA",
      paper_bgcolor = "white",
      hovermode     = "x unified",
      margin        = list(l = 45, r = 15, t = 38, b = 55)
    ) %>%
    config(displayModeBar = FALSE)
}

# ── GDP decomposition stacked-bar ─────────────────────────────────────────────

decomp_components <- c("Consumption", "Business Investment", "Dwelling Investment",
                        "Government", "Net Exports", "Inventories & Other")
decomp_palette    <- c("#1565C0", "#42A5F5", "#90CAF9", "#FF8F00", "#EF9A9A", "#BDBDBD")

# transform = "periodic" → QoQ contributions; "yoy" → rolling annual contributions
placeholder_decomp_chart <- function(label, seed = 1, transform = "yoy") {
  set.seed(seed)
  n_q   <- 40
  dates <- seq(
    floor_date(Sys.Date(), "month") %m-% months((n_q - 1) * 3),
    floor_date(Sys.Date(), "month"),
    by = "quarter"
  )
  qoq_means <- c(0.40, 0.12, 0.05, 0.18, 0.05, 0.03)
  means     <- if (transform == "yoy") qoq_means * 4 else qoq_means
  vol       <- if (transform == "yoy") 0.35 else 0.18

  df <- map2_dfr(decomp_components, means, function(comp, mu) {
    tibble(date = dates, component = comp,
           value = round(rnorm(n_q, mu, vol), 3))
  }) %>%
    mutate(component = factor(component, levels = decomp_components))

  ytitle <- if (transform == "yoy") "ppts (annual)" else "ppts (qtrly)"

  plot_ly(
    df, x = ~date, y = ~value, color = ~component,
    colors        = decomp_palette,
    type          = "bar",
    hovertemplate = "%{x|%b %Y}:  %{y:.2f} ppts<extra>%{fullData.name}</extra>"
  ) %>%
    layout(
      barmode       = "relative",
      title         = list(text = label, font = list(size = 13, color = "#212121"),
                           x = 0.02, y = 0.97),
      xaxis         = list(title = "", showgrid = FALSE, zeroline = FALSE),
      yaxis         = list(title = ytitle, gridcolor = "#EEEEEE", zeroline = TRUE,
                           zerolinecolor = "#BDBDBD", tickfont = list(size = 11)),
      plot_bgcolor  = "#FAFAFA",
      paper_bgcolor = "white",
      legend        = list(orientation = "h", y = -0.22, font = list(size = 11),
                           traceorder = "normal"),
      margin        = list(l = 55, r = 15, t = 38, b = 65),
      hovermode     = "x unified"
    ) %>%
    config(displayModeBar = FALSE)
}
