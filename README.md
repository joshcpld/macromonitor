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
| `table_no` | ABS table number within the catalogue (e.g. `2`) |
| `series_id` | ABS series ID (e.g. `A2304402X`) — preferred fetch path; takes priority over `series_filter` |
| `series_filter` | Fallback series description filter when no direct `series_id` is available |
| `frequency` | `monthly` or `quarterly` |
| `transform_type` | `standard` or `additive_decomp` |
| `series_role` | For `additive_decomp` charts: `aggregate`, `component`, `component_imports`, or `residual`. Leave blank for `standard` charts. |

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
Prices,Inflation,cpi_measures,CPI Measures,Trimmed Mean,readabs,6401.0,10,A3604506F,,quarterly,standard,
Prices,Inflation,cpi_measures,CPI Measures,Headline,readabs,6401.0,2,A2325846C,,quarterly,standard,
```

**Decomposition example** — GDP growth decomposed into contributions:
```
Activity,GDP,gdp_decomp,GDP Growth Decomp,Real GDP,readabs,5206.0,2,A2304402X,,quarterly,additive_decomp,aggregate
Activity,GDP,gdp_decomp,GDP Growth Decomp,Consumption,readabs,5206.0,2,A2304081W,,quarterly,additive_decomp,component
Activity,GDP,gdp_decomp,GDP Growth Decomp,Imports,readabs,5206.0,2,A2304173C,,quarterly,additive_decomp,component_imports
```

---

## Project structure

```
macromonitor/
├── run_app.R         # launch app in Chrome on port 3838
├── global.R          # packages, config loading, chart helpers
├── ui.R              # fully dynamic layout built from config
├── server.R          # reactive rendering loop
├── config/           # one CSV per country
├── R/
│   ├── ingestion.R   # readabs / readrba fetch + CSV caching
│   ├── charts.R      # live_chart() and live_decomp_chart()
│   └── transformations.R  # YoY, QoQ/MoM, and additive_decomp logic
├── www/
│   ├── custom.css         # section headers, toggle buttons, nav styling
│   └── toggle-highlight.js  # transform toggle state management
└── data/             # cached CSVs (gitignored — regenerated via Refresh button)
```

---

## Running the app

```r
install.packages(c("shiny", "bslib", "plotly", "shinyWidgets", "tidyverse", "readabs"))
source("run_app.R")
```

Or directly:
```r
shiny::runApp()
```

On first load, charts render as placeholders. Click **Refresh Data** in the navbar to download live series from the ABS. Data is cached to `data/` as CSVs and reloaded automatically on subsequent runs.

---

## Chart controls

Each chart includes:

- **Transform toggle** — Level / MoM or QoQ / YoY for standard charts; QoQ / YoY for decomposition charts. All charts default to YoY.
- **Y-axis range** — optional min/max text inputs above each chart to lock the y-axis scale.
- **X-axis range selector** — 1Y, 2Y, 3Y, 4Y, 5Y, 10Y, 20Y, Max buttons built into each Plotly chart. Default view shows 20 years of history.
