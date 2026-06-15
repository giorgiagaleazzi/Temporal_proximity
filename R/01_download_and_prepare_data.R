# ----------------------------------------------------------
# 01_download_and_prepare_data.R
# Project: When Financial Markets Catch a Cold
# Purpose: Download equity prices, compute returns, fetch
#          macro controls, build trading-hour overlap matrix
# ----------------------------------------------------------

message("\n--- 01_download_and_prepare_data.R ---")

# ----------------------------------------------------------
# 1. Market definitions ----
# ----------------------------------------------------------

# markets tibble
markets <- tibble(
  country = c(
    "Japan", "Hong Kong", "Australia",
    "United Kingdom", "Germany", "France",
    "United States", "Canada", "Brazil", "India"
  ),
  market = c("JP", "HK", "AU", "UK", "DE", "FR", "US", "CA", "BR", "IN"),
  ticker = c(
    "^N225",  "^HSI",   "^AXJO",
    "^FTSE",  "^GDAXI", "^FCHI",
    "^GSPC",  "^GSPTSE", "^BVSP", "^NSEI"
  )
)
save_raw(markets, "markets_reference.csv")

# ----------------------------------------------------------
# 2. Download daily equity prices----
# ----------------------------------------------------------

safe_tq_get <- possibly(tq_get, otherwise = NULL)

equity_prices <- markets %>%
  mutate(
    data = map(ticker, ~ safe_tq_get(.x, from = SAMPLE_START, to = SAMPLE_END))
  ) %>%
  drop_na(data) %>%
  unnest(data) %>%
  select(country, market, date, adjusted, volume)

save_raw(equity_prices, "equity_prices_raw.csv")
message("✓ Equity prices downloaded.")

# ----------------------------------------------------------
# 3. Compute daily log returns----
# ----------------------------------------------------------

equity_returns <- equity_prices %>%
  distinct(market, date, .keep_all = TRUE) %>%
  arrange(market, date) %>%
  group_by(market) %>%
  mutate(return = log(adjusted / dplyr::lag(adjusted))) %>%
  ungroup() %>%
  filter(!is.na(return))

save_clean(equity_returns, "equity_returns_daily.csv")
message("✓ Daily log returns computed.")

# ----------------------------------------------------------
# 4. VIX stress indicator----
# ----------------------------------------------------------

vix_raw <- tq_get("^VIX", from = SAMPLE_START, to = SAMPLE_END) %>%
  select(date, vix_close = adjusted)

save_raw(vix_raw, "vix_daily.csv")

vix_monthly <- vix_raw %>%
  mutate(month = floor_date(date, "month")) %>%
  summarise(vix_mean = mean(vix_close, na.rm = TRUE), .by = month) %>%
  mutate(
    high_stress = as.integer(
      vix_mean > quantile(vix_mean, probs = STRESS_VIX_PERCENTILE / 100, na.rm = TRUE)
    )
  )

save_clean(vix_monthly, "vix_monthly.csv")
message("✓ VIX stress indicator constructed.")

# ----------------------------------------------------------
# 5. US macroeconomic controls (FRED)----
# ----------------------------------------------------------
# Used as optional robustness controls only.
# Core identification relies on trading-hour overlap x stress.

fred_series <- c(INDPRO = "ip", CPIAUCSL = "cpi", TB3MS = "rate")

macro_us <- map2(
  names(fred_series),
  fred_series,
  ~ {
    result <- tryCatch(
      tq_get(.x, get = "economic.data", from = SAMPLE_START, to = SAMPLE_END),
      error = function(e) {
        message("  Warning: could not download ", .x, " — ", e$message)
        NULL
      }
    )
    if (is.null(result) || isFALSE(result) || nrow(result) == 0) {
      message("  Skipping ", .x, " — no data returned")
      return(NULL)
    }
    result %>% select(date, !!.y := price)
  }
) %>%
  compact() %>%                          # remove any NULLs
  reduce(left_join, by = "date") %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END)

save_raw(macro_us, "macro_controls_fred_us.csv")
message("✓ US macroeconomic controls downloaded.")

# ----------------------------------------------------------
# 6. Trading volume weights----
# ----------------------------------------------------------

volume_weights <- equity_prices %>%
  filter(!is.na(volume), !is.na(adjusted)) %>%
  mutate(traded_value = volume * adjusted) %>%
  summarise(avg_traded_value = mean(traded_value, na.rm = TRUE), .by = market) %>%
  mutate(weight = avg_traded_value / sum(avg_traded_value))

save_clean(volume_weights, "volume_weights.csv")
message("✓ Volume weights computed.")

# ----------------------------------------------------------
# 7. Trading-hour overlap matrix----
# ----------------------------------------------------------
# Hours in UTC. AU overnight session normalised to 24h+ scale
# (23:00-29:00 UTC) to correctly capture overlap with other markets.

exchange_hours <- tibble(
  market    = c("JP", "HK", "AU", "UK", "DE", "FR", "US", "CA", "BR", "IN"),
  open_utc  = c( 0.0,  1.5, 23.0,  8.0,  8.0,  8.0, 14.5, 14.5, 13.0,  3.75),
  close_utc = c( 6.0,  8.0, 29.0, 16.5, 16.5, 16.5, 21.0, 21.0, 21.0, 10.0)
)

save_raw(exchange_hours, "exchange_hours_utc.csv")

overlap_matrix <- exchange_hours %>%
  rename_with(~ paste0(.x, "_i")) %>%
  crossing(exchange_hours %>% rename_with(~ paste0(.x, "_j"))) %>%
  filter(market_i < market_j) %>%
  mutate(
    overlap_hours     = pmax(0, pmin(close_utc_i, close_utc_j) - pmax(open_utc_i, open_utc_j)),
    avg_trading_hours = ((close_utc_i - open_utc_i) + (close_utc_j - open_utc_j)) / 2,
    overlap_share     = overlap_hours / avg_trading_hours
  ) %>%
  select(market_i, market_j, overlap_share)

save_clean(overlap_matrix, "trading_hour_overlap_matrix.csv")
message("✓ Trading-hour overlap matrix constructed.")

# ----------------------------------------------------------
# 8. Institutional investor proxy (NBFI intensity)----
# ----------------------------------------------------------
# ETF/mutual fund AUM as share of market cap, by market.
# Source: ICI, national exchanges, or World Bank GFDD.
# Used to construct bilateral NBFI intensity weight.

nbfi_intensity <- tibble(
  market = c("JP", "HK", "AU", "UK", "DE", "FR", "US", "CA", "BR", "IN"),
  # ETF + mutual fund AUM / total market cap, approx 2015-2023 average
  # Sources: ICI World Factbook 2023, national exchange data
  nbfi_share = c(
    0.12,   # Japan: relatively low institutional ownership
    0.35,   # Hong Kong: high, major fund hub
    0.42,   # Australia: very high, superannuation system
    0.48,   # UK: high, major asset management centre
    0.25,   # Germany: moderate
    0.22,   # France: moderate
    0.65,   # US: highest globally, ETF and mutual fund dominant
    0.38,   # Canada: high, pension fund driven
    0.18,   # Brazil: lower institutional penetration, capital controls historically
    0.14    # India: low but growing — FII ownership ~20% of NSE market cap
            # but domestic institutional penetration still below developed markets
  )
)

save_raw(nbfi_intensity, "nbfi_intensity.csv")
message("✓ NBFI intensity proxy constructed.")

# ----------------------------------------------------------
# 9. Done----
# ----------------------------------------------------------

message("✓ 01_download_and_prepare_data.R completed.")
