################################################################################
# server.R
# Macro Monitor — reactive logic and chart rendering
################################################################################

server <- function(input, output, session) {

  ################################################################################
  # Phase 1 — placeholder chart rendering
  #
  # Iterates over unique chart_ids. Each chart may have 1 or more series rows.
  # The transform toggle re-renders the chart reactively on change.
  ################################################################################

  # One row per unique chart; preserves CSV order for deterministic seeds
  chart_meta <- config %>%
    group_by(chart_id) %>%
    summarise(
      chart_title       = first(chart_title),
      transform_type    = first(transform_type),
      frequency         = first(frequency),
      default_transform = first(default_transform),
      # Exclude aggregate row — it is the denominator, not a plotted line
      n_series          = sum(series_role != "aggregate"),
      .groups = "drop"
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
        # Component/series names for legend labels (exclude aggregate row)
        .series      <- config %>%
          filter(chart_id == .id, series_role %in% c("series", "component")) %>%
          pull(series_name)

        output[[.id]] <- renderPlotly({
          tfm <- if (is.null(input[[.tfm]])) .default_tfm else input[[.tfm]]

          if (.type == "additive_decomp") {
            placeholder_decomp_chart(.label, seed = .seed, transform = tfm)

          } else if (.n == 1) {
            df <- mock_series(seed = .seed) %>% apply_transform(tfm, .freq)
            placeholder_chart(.label, df = df)

          } else {
            df <- map2_dfr(seq_len(.n), .series, function(i, nm) {
              mock_series(seed = .seed + i) %>%
                apply_transform(tfm, .freq) %>%
                mutate(series = nm)
            })
            placeholder_multiseries_chart(.label, df)
          }
        })
      })
    })

  ################################################################################
  # Phase 2–3 stubs
  ################################################################################

  # source("R/ingestion.R", local = TRUE)
  # source("R/charts.R",    local = TRUE)

}
