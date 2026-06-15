# ----------------------------------------------------------
# 03_empirical_analysis.R
# Project: When Financial Markets Catch a Cold
# Purpose: Baseline regressions, diagnostics, and results table
# Output:  output/tables/table_baseline_results.txt
# ----------------------------------------------------------

message("\n--- 03_empirical_analysis.R ---")

# ----------------------------------------------------------
# 1. Baseline model (pair FE only)
# ----------------------------------------------------------

baseline_model <- feols(
  corr_ij ~ overlap_stress | pair_id,
  data    = monthly_panel,
  cluster = ~pair_id
)

# ----------------------------------------------------------
# 2. Pair and time fixed effects
# ----------------------------------------------------------

model_time_fe <- feols(
  corr_ij ~ overlap_stress | pair_id + month,
  data    = monthly_panel,
  cluster = ~pair_id
)

# ----------------------------------------------------------
# 3. Liquidity channel: volume-weighted pairs
# ----------------------------------------------------------

volume_pairs <- volume_weights %>%
  select(market, weight) %>%
  rename(market_i = market, weight_i = weight) %>%
  crossing(
    volume_weights %>%
      select(market, weight) %>%
      rename(market_j = market, weight_j = weight)
  ) %>%
  filter(market_i < market_j) %>%
  mutate(pair_volume_weight = (weight_i + weight_j) / 2) %>%
  select(market_i, market_j, pair_volume_weight)

monthly_panel <- monthly_panel %>%
  left_join(volume_pairs, by = c("market_i", "market_j")) %>%
  mutate(overlap_stress_vol = overlap_share * pair_volume_weight * high_stress)

message("✓ Volume weights merged into panel.")

# ----------------------------------------------------------
# 4. Volume-weighted model
# ----------------------------------------------------------

model_volume <- feols(
  corr_ij ~ overlap_stress_vol | pair_id,
  data    = monthly_panel,
  cluster = ~pair_id
)

# ----------------------------------------------------------
# 5. Full specification
# ----------------------------------------------------------

model_full <- feols(
  corr_ij ~ overlap_stress + overlap_stress_vol | pair_id + month,
  data    = monthly_panel,
  cluster = ~pair_id
)

message("✓ Main models estimated.")

# ----------------------------------------------------------
# 6. NBFI extension: bilateral institutional investor intensity
# ----------------------------------------------------------

nbfi_intensity <- load_raw("nbfi_intensity.csv")

nbfi_pairs <- nbfi_intensity %>%
  rename(market_i = market, nbfi_i = nbfi_share) %>%
  crossing(
    nbfi_intensity %>% rename(market_j = market, nbfi_j = nbfi_share)
  ) %>%
  filter(market_i < market_j) %>%
  mutate(nbfi_bilateral = (nbfi_i + nbfi_j) / 2) %>%
  select(market_i, market_j, nbfi_bilateral)

monthly_panel <- monthly_panel %>%
  left_join(nbfi_pairs, by = c("market_i", "market_j")) %>%
  mutate(overlap_stress_nbfi = overlap_share * high_stress * nbfi_bilateral)

model_nbfi <- feols(
  corr_ij ~ overlap_stress_nbfi | pair_id,
  data    = monthly_panel,
  cluster = ~pair_id
)

model_full_nbfi <- feols(
  corr_ij ~ overlap_stress + overlap_stress_nbfi | pair_id + month,
  data    = monthly_panel,
  cluster = ~pair_id
)

message("✓ NBFI extension models estimated.")

# ----------------------------------------------------------
# 7. Diagnostics
# ----------------------------------------------------------

stopifnot(
  "monthly_panel has no rows after corr_ij filter" =
    nrow(filter(monthly_panel, !is.na(corr_ij))) > 0,
  "overlap_stress has no variation" =
    sd(monthly_panel$overlap_stress, na.rm = TRUE) > 0
)

diag_summary <- monthly_panel %>%
  filter(!is.na(corr_ij)) %>%
  summarise(
    mean_corr      = mean(corr_ij),
    sd_corr        = sd(corr_ij),
    share_low_days = mean(n_days < MIN_TRADING_DAYS)
  )

message("  Diagnostics:")
message("    mean corr      = ", round(diag_summary$mean_corr,      3))
message("    sd corr        = ", round(diag_summary$sd_corr,        3))
message("    share low days = ", round(diag_summary$share_low_days, 3))

# ----------------------------------------------------------
# 8. Results table
# ----------------------------------------------------------

results_table <- etable(
  baseline_model,
  model_time_fe,
  model_volume,
  model_full,
  model_nbfi,
  model_full_nbfi,
  headers = c("Baseline", "Pair + Time FE", "Liquidity", "Full", "NBFI", "Full + NBFI"),
  keep    = c("%overlap_stress", "%overlap_stress_vol", "%overlap_stress_nbfi"),
  dict    = c(
    overlap_stress      = "Overlap \u00d7 Stress",
    overlap_stress_vol  = "Overlap \u00d7 Volume \u00d7 Stress",
    overlap_stress_nbfi = "Overlap \u00d7 NBFI \u00d7 Stress"
  ),
  se      = "cluster",
  fitstat = c("n", "r2", "wr2"),
  title   = "Trading-Hour Overlap and Financial Contagion"
)

save_table(
  capture.output(results_table),
  "table_baseline_results.txt"
)

message("✓ 03_empirical_analysis.R completed.")