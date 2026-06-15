# ----------------------------------------------------------
# 02_construct_panel.R
# Project: When Financial Markets Catch a Cold
# Purpose: Construct bilateral monthly panel of equity return
#          correlations across 45 market pairs (10 markets)
# Output:  data/clean/monthly_panel.csv
# ----------------------------------------------------------

message("\n--- 02_construct_panel.R ---")

# ----------------------------------------------------------
# 1. Prepare daily returns
# ----------------------------------------------------------

returns_panel <- equity_returns %>%
  mutate(month = floor_date(date, "month"))

# ----------------------------------------------------------
# 2. Create unordered market pairs
# ----------------------------------------------------------

market_pairs <- sort(unique(returns_panel$market)) %>%
  combn(2) %>%
  t() %>%
  as.data.frame() %>%
  setNames(c("market_i", "market_j"))

# ----------------------------------------------------------
# 3. Construct bilateral daily returns
# ----------------------------------------------------------

returns_i <- returns_panel %>%
  select(date, month, market_i = market, return_i = return)

returns_j <- returns_panel %>%
  select(date, month, market_j = market, return_j = return)

pairwise_daily <- market_pairs %>%
  inner_join(returns_i, by = "market_i",                       relationship = "many-to-many") %>%
  inner_join(returns_j, by = c("market_j", "date", "month"),   relationship = "many-to-many")

# ----------------------------------------------------------
# 4. Compute monthly bilateral correlations
# ----------------------------------------------------------
monthly_panel <- pairwise_daily %>%
  group_by(market_i, market_j, month) %>%
  summarise(
    corr_ij = if (n() >= MIN_TRADING_DAYS &&
                  sd(return_i, na.rm = TRUE) > 0 &&
                  sd(return_j, na.rm = TRUE) > 0) {
      cor(return_i, return_j, use = "pairwise.complete.obs")
    } else {
      NA_real_
    },
    n_days = n(),
    .groups = "drop"
  ) %>%
  mutate(pair_id = paste(market_i, market_j, sep = "_")) %>%
  filter(month <= SAMPLE_END)

# ----------------------------------------------------------
# 5. Merge overlap and stress indicators
# ----------------------------------------------------------

monthly_panel <- monthly_panel %>%
  left_join(overlap_matrix, by = c("market_i", "market_j")) %>%
  left_join(vix_monthly,    by = "month") %>%
  mutate(overlap_stress = overlap_share * high_stress)

# ----------------------------------------------------------
# 6. Sanity checks
# ----------------------------------------------------------

stopifnot(
  "No valid correlations computed" = any(!is.na(monthly_panel$corr_ij)),
  "overlap_share contains NAs"     = !anyNA(monthly_panel$overlap_share),
  "high_stress contains NAs"       = !anyNA(monthly_panel$high_stress)
)

summary_stats <- monthly_panel %>%
  filter(!is.na(corr_ij)) %>%
  summarise(
    mean_corr = mean(corr_ij),
    sd_corr   = sd(corr_ij),
    min_corr  = min(corr_ij),
    max_corr  = max(corr_ij)
  )

message("  Correlation summary:")
message("    mean = ", round(summary_stats$mean_corr, 3))
message("    sd   = ", round(summary_stats$sd_corr,   3))
message("    min  = ", round(summary_stats$min_corr,  3))
message("    max  = ", round(summary_stats$max_corr,  3))

# ----------------------------------------------------------
# 7. Save output
# ----------------------------------------------------------

save_clean(monthly_panel, "monthly_panel.csv")
message("✓ 02_construct_panel.R completed.")