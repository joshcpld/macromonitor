################################################################################
# R/charts.R
# Phase 3 — live chart builders
#
# Replaces placeholder_chart() calls in server.R with series-aware renderers.
# Each builder accepts a pre-filtered, tidy data frame and a display label.
################################################################################

# ── Shared layout defaults ────────────────────────────────────────────────────

# chart_layout <- list(
#   xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
#   yaxis = list(title = "", gridcolor = "#EEEEEE", zeroline = FALSE,
#                tickfont = list(size = 11)),
#   plot_bgcolor  = "#FAFAFA",
#   paper_bgcolor = "white",
#   hovermode     = "x unified",
#   margin        = list(l = 45, r = 15, t = 38, b = 30)
# )

# ── Line chart (default for most macro series) ────────────────────────────────

# live_line_chart <- function(df, label,
#                             date_col  = "date",
#                             value_col = "value",
#                             color     = "#1565C0") {
#   plotly::plot_ly(
#     df,
#     x    = as.formula(paste0("~", date_col)),
#     y    = as.formula(paste0("~", value_col)),
#     type = "scatter",
#     mode = "lines",
#     line = list(color = color, width = 1.8),
#     hovertemplate = "%{x|%b %Y}:  %{y:.2f}<extra></extra>"
#   ) %>%
#     plotly::layout(
#       title = list(
#         text = label,
#         font = list(size = 13, color = "#212121"),
#         x = 0.02, y = 0.97
#       ),
#       !!!chart_layout   # splice shared defaults
#     ) %>%
#     plotly::config(displayModeBar = FALSE)
# }

# ── Multi-series overlay (e.g. Trend vs Seasonally Adjusted) ─────────────────

# live_multiseries_chart <- function(df, label,
#                                    date_col   = "date",
#                                    value_col  = "value",
#                                    series_col = "series_type") {
#   plotly::plot_ly(
#     df,
#     x      = as.formula(paste0("~", date_col)),
#     y      = as.formula(paste0("~", value_col)),
#     color  = as.formula(paste0("~", series_col)),
#     colors = c("#1565C0", "#90A4AE"),
#     type   = "scatter",
#     mode   = "lines",
#     line   = list(width = 1.8),
#     hovertemplate = "%{x|%b %Y}:  %{y:.2f}<extra></extra>"
#   ) %>%
#     plotly::layout(
#       title = list(
#         text = label,
#         font = list(size = 13, color = "#212121"),
#         x = 0.02, y = 0.97
#       ),
#       legend = list(orientation = "h", y = -0.15),
#       !!!chart_layout
#     ) %>%
#     plotly::config(displayModeBar = FALSE)
# }
