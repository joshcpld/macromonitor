################################################################################
# R/transformations.R
# Macro Monitor — canonical transformation methodology reference
#
# Two transformation types are used across the dashboard:
#
#   "standard"          — level series with a user-controlled view toggle
#   "additive_decomp"   — contribution-to-growth decomposition for aggregates
#
# This file is the single source of truth for all transform mathematics.
# Source it from global.R; do not duplicate logic elsewhere.
################################################################################

################################################################################
# 1. STANDARD TRANSFORMS
#
# Applied to level series. The chart toggle exposes three views:
#
#   Level   :  y_t                               (raw published value)
#   MoM/QoQ :  y_t − y_{t−1}                    (period-on-period change)
#   YoY     :  y_t − y_{t−n}                    n = 12 (monthly), n = 4 (quarterly)
#
# Values are in the natural units of the underlying series — e.g. index points,
# $bn, %, or percentage points depending on the published source.
#
# Phase 3 note: ABS series already published in growth-rate form (e.g. CPI
# %chg YoY) will show the published rate in "Level" view; applying the YoY
# transform again would be incorrect. Series-level metadata should flag this.
################################################################################

apply_standard_transform <- function(df, transform, frequency = "monthly") {
  lag_n <- if (frequency == "quarterly") 4L else 12L

  # Ensure chronological order before any lag operation
  df <- dplyr::arrange(df, date)

  out <- switch(transform,
    "periodic" = df %>% dplyr::mutate(value = (value / dplyr::lag(value, 1)    - 1) * 100),
    "yoy"      = df %>% dplyr::mutate(value = (value / dplyr::lag(value, lag_n) - 1) * 100),
    df   # "level": return unchanged
  )

  out %>% dplyr::filter(!is.na(value))
}

################################################################################
# 2. ADDITIVE DECOMPOSITION  (transform_type = "additive_decomp")
#
# Purpose
# -------
# Decompose the growth of an aggregate series (e.g. real GDP) into the
# percentage-point (ppt) contributions of its demand components.
# The decomposition is exact and additive: summing all component contributions
# reproduces the aggregate growth rate precisely.
#
# Inputs (from the config CSV for each chart_id with transform_type = "additive_decomp")
#   series_role = "aggregate"  — the headline series (e.g. Real GDP, chain-volume $m)
#   series_role = "component"  — explicit demand components (fetched from API)
#   series_role = "residual"   — computed last; no api_code; see formula below
#
# GDP components used in australia.csv
#   Consumption              Private final consumption expenditure
#   Business Investment      Private gross fixed capital formation (excl. dwellings)
#   Dwelling Investment      Residential gross fixed capital formation
#   Government               General government final consumption expenditure
#   Exports                  Exports of goods and services          (series_role = "component")
#   Imports                  Imports of goods and services          (series_role = "component_imports")
#   Inventories & Other      Residual — see below
#
# Note on series_role = "component_imports":
#   Imports enter the national accounts identity as a deduction (GDP = C + I + G + X - M).
#   Their contribution formula is negated relative to the standard component formula:
#
#   c_imports,t^QoQ  =  −(M_t − M_t−1) / A_t−1 × 100
#
#   The "Net Exports" bar in the decomp chart is therefore the sum of the Exports
#   contribution and the (negative) Imports contribution.
#
# Formulas
# --------
# Let  A_t  = aggregate level (chain-volume $m)
#      x_it = component i level
#
# QoQ contribution (ppts):
#
#   c_i,t^QoQ  =  (x_i,t − x_i,t−1) / A_t−1  × 100
#
# YoY contribution (ppts):
#
#   c_i,t^YoY  =  (x_i,t − x_i,t−4) / A_t−4  × 100
#
# Residual ("Inventories & Other"):
#
#   c_res^QoQ  =  [ (A_t − A_t−1) / A_t−1 × 100 ]  −  Σ_i c_i,t^QoQ
#   c_res^YoY  =  [ (A_t − A_t−4) / A_t−4 × 100 ]  −  Σ_i c_i,t^YoY
#
# The residual absorbs:
#   (a) Changes in inventories (not tracked as an explicit component)
#   (b) The chain-linking discrepancy: ABS chain-volume components do not sum
#       exactly to chain-volume GDP due to the annual-overlap method used to
#       construct constant-price estimates. The residual closes this gap.
#   (c) The ABS statistical discrepancy.
#
# Identity (holds exactly by construction):
#   Σ_i c_i,t^QoQ  =  (A_t − A_t−1) / A_t−1 × 100   ✓
#   Σ_i c_i,t^YoY  =  (A_t − A_t−4) / A_t−4 × 100   ✓
#
# Phase 3 implementation
################################################################################

# apply_additive_decomp()
#
# Inputs
# ------
#   df_all    : data frame with columns (date, value, series_name, series_role)
#               as written by ingestion.R — one row per (series, date)
#   transform : "periodic" (QoQ) or "yoy" (YoY)
#
# Returns
# -------
#   Tidy data frame with columns (date, series_name, value) where value is the
#   ppt contribution of each component to aggregate growth.
#   Exports + Imports are automatically collapsed into a single "Net Exports" bar.
#   The residual ("Inventories & Other") is appended last.

apply_additive_decomp <- function(df_all, transform = "periodic") {
  lag_n <- if (transform == "yoy") 4L else 1L

  # One row per (series, date) — guards against duplicates from readabs multi-table files
  df_agg  <- df_all %>%
    dplyr::filter(series_role == "aggregate") %>%
    dplyr::distinct(date, .keep_all = TRUE) %>%
    dplyr::arrange(date)

  df_comp <- df_all %>%
    dplyr::filter(series_role %in% c("component", "component_imports")) %>%
    dplyr::distinct(series_name, date, .keep_all = TRUE)

  if (nrow(df_agg) == 0 || nrow(df_comp) == 0) {
    return(dplyr::tibble(date = as.Date(NA), series_name = NA_character_, value = NA_real_)[-1, ])
  }

  # Lagged aggregate denominator
  denom <- df_agg %>%
    dplyr::arrange(date) %>%
    dplyr::transmute(date, agg_lag = dplyr::lag(value, lag_n))

  # Component contributions
  # component_imports rows have their sign negated (imports subtract from GDP)
  contrib <- df_comp %>%
    dplyr::group_by(series_name, series_role) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(comp_lag = dplyr::lag(value, lag_n)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(denom, by = "date") %>%
    dplyr::mutate(
      raw_contrib  = (value - comp_lag) / agg_lag * 100,
      contribution = dplyr::if_else(series_role == "component_imports",
                                    -raw_contrib, raw_contrib)
    ) %>%
    dplyr::filter(!is.na(contribution), !is.na(agg_lag))

  # Collapse Exports + Imports → "Net Exports"
  trade_names <- c("Exports", "Imports")
  if (any(contrib$series_name %in% trade_names)) {
    net_exports <- contrib %>%
      dplyr::filter(series_name %in% trade_names) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(value = sum(contribution), .groups = "drop") %>%
      dplyr::mutate(series_name = "Net Exports")

    contrib_clean <- dplyr::bind_rows(
      contrib %>%
        dplyr::filter(!series_name %in% trade_names) %>%
        dplyr::select(date, series_name, value = contribution),
      net_exports
    )
  } else {
    contrib_clean <- contrib %>% dplyr::select(date, series_name, value = contribution)
  }

  # Aggregate growth rate (for residual)
  agg_growth <- df_agg %>%
    dplyr::left_join(denom, by = "date") %>%
    dplyr::mutate(agg_pct = (value - agg_lag) / agg_lag * 100) %>%
    dplyr::filter(!is.na(agg_pct)) %>%
    dplyr::select(date, agg_pct)

  # Residual = aggregate growth − sum of explicit contributions
  residual <- contrib_clean %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(sum_contrib = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(agg_growth, by = "date") %>%
    dplyr::filter(!is.na(agg_pct)) %>%
    dplyr::transmute(date, series_name = "Inventories & Other", value = agg_pct - sum_contrib)

  dplyr::bind_rows(contrib_clean, residual) %>% dplyr::arrange(date)
}
