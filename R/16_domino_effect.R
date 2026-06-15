# ==========================================================
# 16_domino_effect 
# Opening-gap test
# Does sequential market timing matter?
# ==========================================================

# ----------------------------------------------------------
# Market opening times (UTC)
# ----------------------------------------------------------

opening_times <- tibble(
  market = c("JP","HK","AU","IN","UK","DE","FR","BR","US","CA"),
  open_utc = c(
    0.0,
    1.5,
    23.0,
    3.75,
    8.0,
    8.0,
    8.0,
    13.0,
    14.5,
    14.5
  )
)

# ----------------------------------------------------------
# Pair-level opening gap
# ----------------------------------------------------------

opening_gap_df <- opening_times %>%
  rename(market_i = market,
         open_i   = open_utc) %>%
  crossing(
    opening_times %>%
      rename(market_j = market,
             open_j   = open_utc)
  ) %>%
  filter(market_i < market_j) %>%
  mutate(
    
    opening_gap = abs(open_i - open_j),
    
    # shortest distance around the 24h clock
    opening_gap = pmin(opening_gap,
                       24 - opening_gap)
    
  ) %>%
  select(
    market_i,
    market_j,
    opening_gap
  )

# ----------------------------------------------------------
# Merge into panel
# ----------------------------------------------------------

monthly_panel_gap <- monthly_panel %>%
  left_join(
    opening_gap_df,
    by = c("market_i", "market_j")
  ) %>%
  mutate(
    gap_stress = opening_gap * high_stress
  )

# ----------------------------------------------------------
# Model 1
# Gap only
# ----------------------------------------------------------

model_gap <- feols(
  corr_ij ~ gap_stress |
    pair_id + month,
  data = monthly_panel_gap,
  cluster = ~ pair_id
)

# ----------------------------------------------------------
# Model 2
# Overlap only
# ----------------------------------------------------------

model_overlap <- feols(
  corr_ij ~ overlap_stress |
    pair_id + month,
  data = monthly_panel_gap,
  cluster = ~ pair_id
)

# ----------------------------------------------------------
# Model 3
# Horse race
# ----------------------------------------------------------

model_overlap_gap <- feols(
  corr_ij ~
    overlap_stress +
    gap_stress |
    pair_id + month,
  data = monthly_panel_gap,
  cluster = ~ pair_id
)

etable(
  model_gap,
  model_overlap,
  model_overlap_gap,
  headers = c(
    "Gap only",
    "Overlap only",
    "Horse race"
  )
)

monthly_panel_gap %>%
  distinct(pair_id, overlap_share, opening_gap) %>%
  summarise(correlation = cor(overlap_share, opening_gap))
