# ----------------------------------------------------------
# 06_robustness_analysis.R
# Project: When Financial Markets Catch a Cold
# Purpose: Robustness checks — two-way clustering,
#          trading-day filter, alternative stress threshold,
#          and non-stress specification
# Output:  output/tables/table_robustness.txt
# ----------------------------------------------------------

message("\n--- 06_robustness_analysis.R ---")

# ----------------------------------------------------------
# 1. Base robustness sample
# ----------------------------------------------------------

robust_panel <- monthly_panel %>%
  filter(!is.na(corr_ij), !is.na(overlap_share), !is.na(high_stress))

# ----------------------------------------------------------
# 2. Two-way clustered models
# ----------------------------------------------------------

model_time_fe_robust <- feols(
  corr_ij ~ overlap_stress | pair_id + month,
  data    = robust_panel,
  cluster = ~pair_id + month
)

model_volume_robust <- feols(
  corr_ij ~ overlap_stress_vol | pair_id,
  data    = robust_panel,
  cluster = ~pair_id + month
)

model_full_robust <- feols(
  corr_ij ~ overlap_stress + overlap_stress_vol | pair_id + month,
  data    = robust_panel,
  cluster = ~pair_id + month
)

message("✓ Two-way clustered models estimated.")

# ----------------------------------------------------------
# 3. Minimum trading-days restriction
# ----------------------------------------------------------

model_min_days <- feols(
  corr_ij ~ overlap_stress | pair_id + month,
  data    = filter(robust_panel, n_days >= MIN_TRADING_DAYS),
  cluster = ~pair_id + month
)

message("✓ Minimum trading-days model estimated.")

# ----------------------------------------------------------
# 4. Alternative stress threshold (80th percentile)
# ----------------------------------------------------------

robust_panel_alt <- robust_panel %>%
  left_join(
    vix_monthly %>%
      mutate(
        high_stress_alt = as.integer(
          vix_mean > quantile(vix_mean, 0.80, na.rm = TRUE)
        )
      ) %>%
      select(month, high_stress_alt),
    by = "month"
  ) %>%
  mutate(overlap_stress_alt = overlap_share * high_stress_alt)

model_alt_stress <- feols(
  corr_ij ~ overlap_stress_alt | pair_id,
  data    = robust_panel_alt,
  cluster = ~pair_id + month
)

message("✓ Alternative stress threshold model estimated.")

# ----------------------------------------------------------
# 5. Non-stress specification
# ----------------------------------------------------------

model_nonstress <- feols(
  corr_ij ~ I(overlap_share * (1 - high_stress)) | pair_id + month,
  data    = robust_panel,
  cluster = ~pair_id + month
)

message("✓ Non-stress model estimated.")

# ----------------------------------------------------------
# 6. Robustness summary table
# ----------------------------------------------------------

robustness_table <- etable(
  baseline_model,
  model_time_fe_robust,
  model_volume_robust,
  model_full_robust,
  model_min_days,
  model_alt_stress,
  model_nonstress,
  keep    = c("%overlap_stress", "%overlap_stress_vol",
              "%overlap_stress_alt", "%overlap_nonstress"),
  dict    = c(
    overlap_stress     = "Overlap × Stress",
    overlap_stress_vol = "Overlap × Volume × Stress",
    overlap_stress_alt = "Overlap × Alternative Stress",
    overlap_nonstress  = "Overlap × Non-stress"
  ),
  title   = "Robustness Analysis"
)

save_table(
  capture.output(robustness_table),
  "table_robustness.txt"
)

message("✓ 06_robustness_analysis.R completed.")