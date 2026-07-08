################################################################################
# R/charts.R
# Macro Monitor — live Plotly chart builders
#
# Functions:
#   live_chart()        — standard line chart from cached data (single or multi-series)
#   live_decomp_chart() — stacked bar contribution chart from cached data
################################################################################

# ── Shared x-axis helpers ────────────────────────────────────────────────────

.rangeselector <- list(
  buttons = list(
    list(count = 1,  label = "1Y",  step = "year", stepmode = "backward"),
    list(count = 2,  label = "2Y",  step = "year", stepmode = "backward"),
    list(count = 3,  label = "3Y",  step = "year", stepmode = "backward"),
    list(count = 4,  label = "4Y",  step = "year", stepmode = "backward"),
    list(count = 5,  label = "5Y",  step = "year", stepmode = "backward"),
    list(count = 10, label = "10Y", step = "year", stepmode = "backward"),
    list(count = 20, label = "20Y", step = "year", stepmode = "backward"),
    list(step = "all", label = "Max")
  ),
  active      = 6L,   # 0-based: highlights 20Y button on first render
  bgcolor     = "#ffffff",
  bordercolor = "#90CAF9",
  borderwidth = 1,
  font        = list(size = 10, color = "#424242"),
  activecolor = "#1565C0",
  x           = 0,
  y           = 1.04,
  xanchor     = "left",
  yanchor     = "bottom"
)

# Default x-axis view: last 20 years, or full series if shorter
.x_range <- function(dates) {
  end   <- max(dates)
  start <- max(min(dates), end - lubridate::years(20))
  list(as.character(start), as.character(end))
}

# Bar chart x-range: pad by ~50 days each side so edge bars aren't half-clipped
.x_range_bar <- function(dates) {
  end   <- max(dates)
  start <- max(min(dates), end - lubridate::years(20))
  list(as.character(as.Date(start) - 50L),
       as.character(as.Date(end)   + 50L))
}

# Build yaxis list, optionally applying a user-specified range
.yaxis <- function(..., y_range = NULL) {
  cfg <- list(...)
  if (!is.null(y_range)) cfg$range <- y_range
  cfg
}

# ─── live_chart() ────────────────────────────────────────────────────────────

live_chart <- function(df, tfm, frequency, label, y_range = NULL) {
  # uirevision keyed on the max date of the raw data: changes when new data
  # is loaded (resets zoom to 20Y default), stable across transform toggles
  ui_rev <- format(max(df$date))

  df_plot <- df %>%
    dplyr::group_by(series_name) %>%
    dplyr::group_modify(~apply_standard_transform(.x, tfm, frequency)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(value))

  n_series <- dplyr::n_distinct(df_plot$series_name)
  x_rng    <- .x_range(df_plot$date)

  base <- if (n_series == 1) {
    plotly::plot_ly(
      df_plot,
      x             = ~date,
      y             = ~value,
      type          = "scatter",
      mode          = "lines",
      line          = list(color = series_colours[1], width = 1.8),
      hovertemplate = "%{x|%b %Y}:  %{y:.2f}<extra></extra>"
    )
  } else {
    plotly::plot_ly(
      df_plot,
      x             = ~date,
      y             = ~value,
      color         = ~series_name,
      colors        = series_colours[seq_len(n_series)],
      type          = "scatter",
      mode          = "lines",
      line          = list(width = 1.8),
      hovertemplate = "%{x|%b %Y}:  %{y:.2f}<extra>%{fullData.name}</extra>"
    )
  }

  legend_cfg <- if (n_series > 1) {
    list(orientation = "h", y = -0.2, font = list(size = 11))
  } else {
    list(showlegend = FALSE)
  }

  base %>%
    plotly::layout(
      uirevision    = ui_rev,     # preserves zoom/range when transform changes
      title         = list(text = label, font = list(size = 13, color = "#212121"),
                           x = 0.02, y = 0.97),
      xaxis         = list(title = "", showgrid = FALSE, zeroline = FALSE,
                           range = x_rng, rangeselector = .rangeselector),
      yaxis         = .yaxis(title = "", gridcolor = "#EEEEEE", zeroline = FALSE,
                             tickfont = list(size = 11), fixedrange = FALSE,
                             y_range = y_range),
      legend        = legend_cfg,
      plot_bgcolor  = "#FAFAFA",
      paper_bgcolor = "white",
      hovermode     = "x unified",
      margin        = list(l = 45, r = 15, t = 52, b = if (n_series > 1) 55 else 30)
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

# ─── live_decomp_chart() ─────────────────────────────────────────────────────

live_decomp_chart <- function(df_all, tfm, label, y_range = NULL) {
  ui_rev     <- format(max(df_all$date))
  df_contrib <- apply_additive_decomp(df_all, transform = tfm)
  if (nrow(df_contrib) == 0) return(plotly::plot_ly())

  df_contrib <- df_contrib %>%
    dplyr::mutate(series_name = factor(series_name, levels = decomp_components))

  lag_n <- if (tfm == "yoy") 4L else 1L
  agg_growth <- df_all %>%
    dplyr::filter(series_role == "aggregate") %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(agg_pct = (value - dplyr::lag(value, lag_n)) / dplyr::lag(value, lag_n) * 100) %>%
    dplyr::filter(!is.na(agg_pct)) %>%
    dplyr::select(date, agg_pct)

  # Pad range ~50 days each side so edge bars aren't half-clipped
  x_rng  <- .x_range_bar(df_contrib$date)
  ytitle <- if (tfm == "yoy") "ppts (annual)" else "ppts (qtrly)"

  plotly::plot_ly() %>%
    plotly::add_bars(
      data          = df_contrib,
      x             = ~date,
      y             = ~value,
      color         = ~series_name,
      colors        = decomp_palette,
      hovertemplate = "%{x|%b %Y}:  %{y:.2f} ppts<extra>%{fullData.name}</extra>"
    ) %>%
    plotly::add_lines(
      data          = agg_growth,
      x             = ~date,
      y             = ~agg_pct,
      name          = "GDP",
      line          = list(color = "#212121", width = 2),
      hovertemplate = "%{x|%b %Y}:  %{y:.2f}%<extra>GDP</extra>"
    ) %>%
    plotly::layout(
      uirevision    = ui_rev,     # preserves zoom/range when transform changes
      barmode       = "relative",
      title         = list(text = label, font = list(size = 13, color = "#212121"),
                           x = 0.02, y = 0.97),
      xaxis         = list(title = "", showgrid = FALSE, zeroline = FALSE,
                           range = x_rng, rangeselector = .rangeselector),
      yaxis         = .yaxis(title = ytitle, gridcolor = "#EEEEEE",
                             zeroline = TRUE, zerolinecolor = "#BDBDBD",
                             tickfont = list(size = 11), fixedrange = FALSE,
                             y_range = y_range),
      plot_bgcolor  = "#FAFAFA",
      paper_bgcolor = "white",
      legend        = list(orientation = "h", y = -0.22, font = list(size = 11),
                           traceorder = "normal"),
      margin        = list(l = 55, r = 15, t = 52, b = 65),
      hovermode     = "x unified"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
