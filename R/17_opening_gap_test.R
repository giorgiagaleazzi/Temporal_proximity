# ----------------------------------------------------------
# 17_opening_gap_test.R
# Project: When Markets Trade Together
# Purpose: Test whether the timing channel reflects
#          contemporaneous overlap or sequential
#          follow-the-sun transmission.
#          Constructs a pair-level opening-time gap measure
#          and runs a horse race against trading-hour overlap.
# Output:  output/tables/table_opening_gap.txt
# ----------------------------------------------------------

message("\n--- 16_opening_gap_test.R ---")

# ----------------------------------------------------------
# 1. Market opening times (UTC)
# ----------------------------------------------------------

opening_times <- tibble(
  market   = c("AU",   "BR",   "CA",   "DE",  "FR",  "HK",  "IN",   "JP",  "UK",  "US"),
  open_utc = c( 23.0,   13.0,   14.5,   8.0,   8.0,   1.5,   3.75,   0.0,   8.0,  14.5)
)

# ----------------------------------------------------------
# 2. Pair-level opening gap
#    Shortest arc on the 24-hour clock, so that
#    AU (23:00) vs JP (00:00) = 1 hour, not 23 hours.
# ----------------------------------------------------------

opening_gap_df <- opening_times %>%
  rename(market_i = market, open_i = open_utc) %>%
  crossing(
    opening_times %>%
      rename(market_j = market, open_j = open_utc)
  ) %>%
  filter(market_i < market_j) %>%
  mutate(
    opening_gap = abs(open_i - open_j),
    opening_gap = pmin(opening_gap, 24 - opening_gap)
  ) %>%
  select(market_i, market_j, opening_gap)

message("  Opening gaps computed for ", nrow(opening_gap_df), " pairs.")

# ----------------------------------------------------------
# 3. Merge into panel and construct interaction
# ----------------------------------------------------------

monthly_panel_gap <- monthly_panel %>%
  left_join(opening_gap_df, by = c("market_i", "market_j")) %>%
  mutate(gap_stress = opening_gap * high_stress)

stopifnot(
  "opening_gap contains NAs after merge" = !anyNA(monthly_panel_gap$opening_gap)
)

# Collinearity between the two timing measures
cor_measures <- cor(
  monthly_panel_gap$gap_stress,
  monthly_panel_gap$overlap_stress,
  use = "complete.obs"
)

message("  Correlation between gap_stress and overlap_stress: ",
        round(cor_measures, 3))

# ----------------------------------------------------------
# 4. Specifications
# ----------------------------------------------------------

# Gap only
model_gap <- feols(
  corr_ij ~ gap_stress | pair_id + month,
  data    = monthly_panel_gap,
  cluster = ~pair_id
)

# Overlap only (replicates baseline with time FE for comparability)
model_overlap <- feols(
  corr_ij ~ overlap_stress | pair_id + month,
  data    = monthly_panel_gap,
  cluster = ~pair_id
)

# Horse race: both timing measures simultaneously
model_horse_race <- feols(
  corr_ij ~ overlap_stress + gap_stress | pair_id + month,
  data    = monthly_panel_gap,
  cluster = ~pair_id
)

message("\u2713 Opening-gap models estimated.")

# ----------------------------------------------------------
# 5. Results table
# ----------------------------------------------------------

gap_table <- etable(
  model_gap,
  model_overlap,
  model_horse_race,
  headers = c("Gap only", "Overlap only", "Horse race"),
  keep    = c("%gap_stress", "%overlap_stress"),
  dict    = c(
    gap_stress     = "Opening Gap \u00d7 Stress",
    overlap_stress = "Overlap \u00d7 Stress"
  ),
  se      = "cluster",
  fitstat = c("n", "r2", "wr2"),
  title   = "Simultaneous Trading versus Follow-the-Sun Transmission"
)

save_table(
  capture.output(gap_table),
  "table_opening_gap.txt"
)

message("\u2713 17_opening_gap_test.R completed.")
