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

  out <- switch(transform,
    "periodic" = df %>% dplyr::mutate(value = value - dplyr::lag(value, 1)),
    "yoy"      = df %>% dplyr::mutate(value = value - dplyr::lag(value, lag_n)),
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
# Phase 3 implementation (uncomment when live data is available)
################################################################################

# apply_additive_decomp <- function(df_agg, df_comps, transform = "periodic") {
#
#   # df_agg  : data frame with columns (date, value) — the aggregate series
#   # df_comps: named list of data frames with (date, value) — one per component
#   # transform: "periodic" → QoQ contributions; "yoy" → YoY contributions
#
#   lag_n <- if (transform == "yoy") 4L else 1L
#
#   # Lagged aggregate used as denominator
#   denom <- df_agg %>%
#     dplyr::arrange(date) %>%
#     dplyr::mutate(agg_lag = dplyr::lag(value, lag_n)) %>%
#     dplyr::select(date, agg_lag)
#
#   # Explicit component contributions
#   contrib_comps <- purrr::imap_dfr(df_comps, function(df, nm) {
#     df %>%
#       dplyr::arrange(date) %>%
#       dplyr::mutate(comp_lag = dplyr::lag(value, lag_n)) %>%
#       dplyr::left_join(denom, by = "date") %>%
#       dplyr::mutate(
#         value  = (value - comp_lag) / agg_lag * 100,
#         series = nm
#       ) %>%
#       dplyr::filter(!is.na(value), !is.na(agg_lag)) %>%
#       dplyr::select(date, series, value)
#   })
#
#   # Aggregate growth rate (for residual computation)
#   agg_growth <- df_agg %>%
#     dplyr::arrange(date) %>%
#     dplyr::left_join(denom, by = "date") %>%
#     dplyr::mutate(value = (value - agg_lag) / agg_lag * 100) %>%
#     dplyr::filter(!is.na(value)) %>%
#     dplyr::select(date, value)
#
#   # Residual = aggregate growth − sum of explicit contributions
#   contrib_res <- contrib_comps %>%
#     dplyr::group_by(date) %>%
#     dplyr::summarise(sum_comps = sum(value), .groups = "drop") %>%
#     dplyr::left_join(agg_growth, by = "date") %>%
#     dplyr::mutate(
#       value  = value - sum_comps,
#       series = "Inventories & Other"
#     ) %>%
#     dplyr::select(date, series, value)
#
#   dplyr::bind_rows(contrib_comps, contrib_res)
# }
