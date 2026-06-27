# Macro Monitor

An interactive R Shiny dashboard for macroeconomic surveillance, built around a fully config-driven architecture. The entire layout — countries, pages, sections, and charts — is controlled by CSV files. No code changes are needed to extend the dashboard.

---

## Design principles

### Config-driven layout

All structure lives in `config/`. Each file corresponds to one country tab. Row order in the CSV equals display order on the page.

```
config/
├── australia.csv
└── usa.csv          ← adding this file creates a new tab automatically
```

**CSV columns:**

| Column | Purpose |
|---|---|
| `page` | Left-hand nav item (e.g. Activity, Prices) |
| `sub_section` | Section header within a page |
| `chart_id` | Unique chart identifier — multiple rows sharing one `chart_id` produce a multi-line chart |
| `chart_title` | Plotly chart title |
| `series_name` | Legend label for each line (matches `chart_title` for single-line charts) |
| `api_source` | `readabs` or `readrba` |
| `api_code` | ABS catalogue number or RBA table code |
| `frequency` | `monthly` or `quarterly` |
| `chart_type` | `line` or `decomp` |

The country tab label is derived from the filename (`australia.csv` → "Australia") — no country column needed.

### Adding content without touching code

| Task | Action |
|---|---|
| New country | Drop a new CSV in `config/` |
| New page | Add rows with a new `page` value |
| New section | Add rows with a new `sub_section` value |
| New chart | Add a row with a new `chart_id` |
| Second line on a chart | Add another row with the **same `chart_id`**, different `series_name` |
| Reorder anything | Reorder the CSV rows |

**Multi-line example** — two series on one CPI chart:
```
Prices,Inflation,cpi_measures,CPI Measures,Trimmed Mean,readabs,6401.0,quarterly,line
Prices,Inflation,cpi_measures,CPI Measures,Headline,readabs,6401.0,quarterly,line
```

---

## Project structure

```
macromonitor/
├── global.R          # packages, config loading, chart helpers
├── ui.R              # fully dynamic layout built from config
├── server.R          # reactive rendering loop
├── config/           # one CSV per country
├── R/
│   ├── ingestion.R   # Phase 2: readabs / readrba fetch + cache stubs
│   └── charts.R      # Phase 3: live chart builder stubs
├── www/
│   └── custom.css    # section headers, toggle buttons, nav styling
├── data/             # Phase 3: cached RDS files (gitignored)
├── input/
└── output/
```

---

## Running the app

```r
install.packages(c("shiny", "bslib", "plotly", "shinyWidgets", "tidyverse"))
shiny::runApp()
```

Each chart has a **Level / MoM|QoQ / YoY** transform toggle (or **QoQ / YoY** for decomposition charts). All charts default to the YoY view.

---

## Planned features

### Live data ingestion (Phase 2–3)

Replace the current mock series with live pipelines sourced via:

- [`readabs`](https://github.com/MattCowgill/readabs) — ABS catalogue data (`read_abs(cat_no = ...)`)
- [`readrba`](https://github.com/MattCowgill/readrba) — RBA statistical tables (`read_rba(table_no = ...)`)

Data will be filtered to Trend or Seasonally Adjusted series, cached locally to `data/` as RDS files, and reloaded on next startup without re-fetching.

### Contribution-to-growth (CTG) charts

Implement a robust CTG methodology for all relevant national accounts aggregates. This involves:

- Computing each component's share of the prior-period level
- Multiplying by the component's period-on-period growth rate to derive the percentage-point contribution
- Applying the same approach for YoY contributions via rolling four-quarter sums
- Rendering contributions as stacked relative bar charts, consistent with ABS and RBA publication conventions
