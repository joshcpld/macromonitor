################################################################################
# server.R
# Macro Monitor — reactive logic and chart rendering
################################################################################

server <- function(input, output, session) {

  ################################################################################
  # Data cache
  #
  # live_data is a reactiveVal holding a named list of data frames (one per
  # chart_id) loaded from data/<chart_id>.csv.  Updating it automatically
  # re-renders every chart that calls live_data().
  ################################################################################

  live_data <- reactiveVal(load_cache())

  # Refresh button — downloads fresh data from ABS/RBA, then reloads the cache
  observeEvent(input$refresh_data, {
    showModal(modalDialog(
      title  = "Refreshing data",
      "Downloading from ABS, RBA, and FRED — this may take a minute...",
      footer = NULL
    ))
    tryCatch({
      refresh_data()
      live_data(load_cache())
      removeModal()
      showNotification("Data updated successfully.", type = "message", duration = 5)
    }, error = function(e) {
      removeModal()
      showNotification(paste("Download failed:", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })

  # "Last updated" timestamp displayed in navbar
  output$last_updated_text <- renderText({
    live_data()   # establish reactive dependency
    ts <- cache_last_updated()
    if (ts == "Never") "No data cached" else paste("Updated:", ts)
  })

  ################################################################################
  # Chart rendering
  #
  # Each chart uses live data if a cached CSV exists, otherwise falls back to
  # the placeholder / mock-data renderer so the app remains usable before the
  # first refresh.
  ################################################################################

  # One row per unique chart; preserves CSV order for deterministic mock seeds
  chart_meta <- config %>%
    group_by(chart_id) %>%
    summarise(
      chart_title       = first(chart_title),
      transform_type    = first(transform_type),
      frequency         = first(frequency),
      default_transform = first(default_transform),
      # Exclude aggregate row — it is the denominator, not a plotted line
      n_series          = sum(series_role != "aggregate"),
      .groups           = "drop"
    ) %>%
    mutate(seed = row_number() * 13L)

  chart_meta %>%
    purrr::pwalk(function(chart_id, chart_title, transform_type, frequency,
                          default_transform, n_series, seed, ...) {
      local({
        .id          <- chart_id
        .label       <- chart_title
        .seed        <- seed
        .freq        <- frequency
        .type        <- transform_type
        .default_tfm <- default_transform
        .n           <- n_series
        .tfm         <- paste0(chart_id, "_tfm")
        # Series names for multi-series placeholder legend
        .series      <- config %>%
          filter(chart_id == .id, series_role %in% c("series", "component")) %>%
          pull(series_name)

        output[[.id]] <- renderPlotly({
          tfm  <- if (is.null(input[[.tfm]])) .default_tfm else input[[.tfm]]

          # Y-axis range — NULL when either input is blank or non-numeric
          ymin    <- suppressWarnings(as.numeric(input[[paste0(.id, "_ymin")]]))
          ymax    <- suppressWarnings(as.numeric(input[[paste0(.id, "_ymax")]]))
          y_range <- if (!is.na(ymin) && !is.na(ymax)) c(ymin, ymax) else NULL

          df_cache <- live_data()[[.id]]
          has_data <- !is.null(df_cache) && nrow(df_cache) > 0

          if (.type == "additive_decomp") {
            if (has_data) {
              live_decomp_chart(df_cache, tfm, .label, y_range)
            } else {
              placeholder_decomp_chart(.label, seed = .seed, transform = tfm,
                                       y_range = y_range)
            }

          } else if (has_data) {
            live_chart(df_cache, tfm, .freq, .label, y_range)

          } else if (.n == 1) {
            df <- mock_series(seed = .seed) %>% apply_transform(tfm, .freq)
            placeholder_chart(.label, df = df, y_range = y_range)

          } else {
            df <- map2_dfr(seq_len(.n), .series, function(i, nm) {
              mock_series(seed = .seed + i) %>%
                apply_transform(tfm, .freq) %>%
                mutate(series = nm)
            })
            placeholder_multiseries_chart(.label, df, y_range = y_range)
          }
        })
      })
    })

}
