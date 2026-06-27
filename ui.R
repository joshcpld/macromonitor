################################################################################
# ui.R
# Macro Monitor — fully config-driven layout
#
# Everything — countries, pages, sections, charts — is generated from the CSV.
# To add a new country: add rows with a new `country` value.
# To add a new page:    add rows with a new `page` value for that country.
# To add a new section: add rows with a new `sub_section` value.
# To add a new chart:   add a row with a new `chart_id`.
# To add a line to an existing chart: add a row with the SAME `chart_id`.
# Row order in the CSV = display order everywhere.
################################################################################

# ── Chart cell: toggle + plotly output ───────────────────────────────────────

chart_with_toggle <- function(id, freq, chart_type, default_tfm = "yoy",
                              height = "265px") {
  toggle <- if (chart_type == "additive_decomp") {
    div(class = "chart-controls",
        shinyWidgets::radioGroupButtons(
          inputId  = paste0(id, "_tfm"),
          label    = NULL,
          choices  = c("QoQ" = "periodic", "YoY" = "yoy"),
          selected = default_tfm,
          size     = "xs", status = "primary"
        ))
  } else {
    periodic_label <- if (freq == "quarterly") "QoQ" else "MoM"
    choices        <- setNames(c("level", "periodic", "yoy"),
                               c("Level", periodic_label, "YoY"))
    div(class = "chart-controls",
        shinyWidgets::radioGroupButtons(
          inputId  = paste0(id, "_tfm"),
          label    = NULL,
          choices  = choices,
          selected = default_tfm,
          size     = "xs", status = "primary"
        ))
  }

  div(class = "chart-wrapper",
      toggle,
      plotlyOutput(id, height = if (chart_type == "additive_decomp") "305px" else height))
}

# ── Section builder ───────────────────────────────────────────────────────────

build_section <- function(ctry, pg, sec) {
  sec_cfg   <- config %>% filter(country == ctry, page == pg, sub_section == sec)
  chart_ids <- sec_cfg %>% pull(chart_id) %>% unique()   # preserves CSV order
  n         <- length(chart_ids)

  # ≤3 charts: fit on one row (col-12 / col-6 / col-4)
  # 4+ charts: split into rows of 2
  rows <- if (n <= 3) {
    width <- if (n == 1) 12L else if (n == 2) 6L else 4L
    cells <- lapply(chart_ids, function(cid) {
      cdata <- sec_cfg %>% filter(chart_id == cid)
      column(width, chart_with_toggle(
        id          = cid,
        freq        = cdata$frequency[1],
        chart_type  = cdata$chart_type[1],
        default_tfm = cdata$default_transform[1]
      ))
    })
    list(do.call(fluidRow, cells))
  } else {
    groups <- split(chart_ids, ceiling(seq_along(chart_ids) / 2))
    lapply(groups, function(cids) {
      width <- if (length(cids) == 1L) 12L else 6L
      cells <- lapply(cids, function(cid) {
        cdata <- sec_cfg %>% filter(chart_id == cid)
        column(width, chart_with_toggle(
          id          = cid,
          freq        = cdata$frequency[1],
          chart_type  = cdata$transform_type[1],
          default_tfm = cdata$default_transform[1]
        ))
      })
      do.call(fluidRow, cells)
    })
  }

  do.call(tagList, c(
    list(div(class = "section-header", h4(sec))),
    rows,
    list(tags$hr(class = "section-divider"))
  ))
}

# ── Country tab builder ───────────────────────────────────────────────────────

build_country_tab <- function(ctry) {
  pages <- config %>% filter(country == ctry) %>% pull(page) %>% unique()

  page_panels <- lapply(pages, function(pg) {
    sections <- config %>%
      filter(country == ctry, page == pg) %>%
      pull(sub_section) %>%
      unique()

    page_content <- do.call(tagList, lapply(sections, function(sec) {
      build_section(ctry, pg, sec)
    }))

    nav_panel(pg, page_content)
  })

  nav_panel(
    ctry,
    do.call(navset_pill_list, c(
      list(id     = paste0(to_chart_id(ctry), "_page"),
           well   = FALSE,
           fluid  = TRUE,
           widths = c(2, 10)),
      page_panels
    ))
  )
}

# ── UI definition ─────────────────────────────────────────────────────────────

countries <- config %>% pull(country) %>% unique()

ui <- tagList(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

  do.call(page_navbar, c(
    list(
      title = "Macro Monitor",
      id    = "top_nav",
      theme = bs_theme(bootswatch = "flatly", primary = "#1565C0")
    ),
    lapply(countries, build_country_tab)
  ))
)
