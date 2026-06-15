# When Financial Markets Catch a Cold
### Market-Based Finance, Time Zones, and Financial Contagion

**Author:** Giorgia Galeazzi, University of Glasgow  
**Status:** Working paper — first draft complete, 10-market extended sample  
**Date:** June 2026

---

## Overview

This repository contains the full replication pipeline for the working paper *When Financial Markets Catch a Cold: Market-Based Finance, Time Zones, and Financial Contagion*.

The paper introduces **trading-hour overlap** as a novel structural channel of international financial contagion. Using daily equity return data for **ten major global financial centres** over 2008–2026, it shows that markets trading simultaneously experience significantly stronger comovement during periods of global financial stress — even after controlling for market-pair fixed effects and common global shocks.

**Key results:**

| Specification | β | SE | p |
|---|---|---|---|
| Baseline (pair FE) | 0.247 | 0.039 | < 0.001 |
| Pair + Time FE | 0.108 | 0.035 | < 0.01 |
| Volume-weighted | 1.282 | 0.275 | < 0.001 |
| NBFI-weighted | 0.633 | 0.135 | < 0.001 |

Placebo test (non-stress periods): negative and significant. Horse-race against geographic distance: overlap subsumes distance entirely (r = −0.958).

---

## Repository Structure

```
Financial Flu/
│
├── config/
│   ├── pipeline_config.yml       # All parameters, paths, and sample dates
│   └── config.R                  # Loads yml, exposes globals, I/O helpers
│
├── R/
│   ├── 01_download_and_prepare_data.R     # Download equity prices, compute returns,
│   │                                      # fetch VIX and macro controls,
│   │                                      # build overlap matrix, NBFI proxy
│   ├── 02_construct_panel.R               # Construct bilateral monthly panel
│   ├── 03_empirical_analysis.R            # Baseline regressions (6 specifications)
│   ├── 04_plot_coefficients.R             # Coefficient plot across specifications
│   ├── 05_overlap_tercile_plot.R          # Comovement by overlap tercile and stress
│   ├── 06_robustness_check.R              # 7 robustness specifications
│   ├── 07_distance_vs_timing.R            # Geography vs trading-time horse race
│   ├── 08_crisis_timeline_plot.R          # Average correlations over crisis episodes
│   ├── 09_overlap_heatmap_and_table.R     # Raw overlap heatmap
│   ├── 10_volume_weighted_overlap_heatmap.R  # Volume-weighted overlap heatmap
│   ├── 11_subperiod_analysis.R            # Sub-period: 2008-2014 vs 2015-2026
│   └── 12_export_latex_tables.R           # Export all tables to LaTeX
│
├── data/
│   ├── raw/                       # Raw downloaded data (created by pipeline)
│   └── clean/                     # Processed panel data (created by pipeline)
│
├── output/
│   ├── plots/                     # All PNG figures
│   ├── tables/                    # Plain-text regression output
│   ├── latex/                     # LaTeX table files (.tex)
│   └── logs/                      # Timestamped pipeline logs
│
└── main.R                         # Single entry point — runs full pipeline
```

---

## Replication

### Requirements

- R ≥ 4.2
- Internet connection (data downloaded from Yahoo Finance and FRED on first run)

### Packages

All packages are installed automatically on first run:

```
broom, dplyr, fixest, geosphere, ggplot2, kableExtra, knitr,
lubridate, lmtest, plm, purrr, quantmod, readr, sandwich,
stringr, tibble, tidyr, tidyquant, yaml
```

### Running the pipeline

1. Open `Financial Flu.Rproj` in RStudio — sets the working directory automatically.
2. Open `main.R` and click **Source**, or run:

```r
source("main.R")
```

Full pipeline: approximately 10–20 minutes depending on internet speed.

---

## Output Files

| Output | Location | Description |
|---|---|---|
| `coef_plot.png` | `output/plots/` | Coefficient estimates across specifications |
| `overlap_tercile_plot.png` | `output/plots/` | Comovement by overlap tercile and stress |
| `crisis_timeline_plot.png` | `output/plots/` | Average bilateral correlations 2008–2026 |
| `trading_hour_overlap_heatmap.png` | `output/plots/` | Raw overlap heatmap (10 markets) |
| `volume_weighted_overlap_heatmap.png` | `output/plots/` | Volume-weighted overlap heatmap |
| `subperiod_timeline_plot.png` | `output/plots/` | Crisis timeline with 2015 split marked |
| `subperiod_coef_plot.png` | `output/plots/` | Coefficient estimates by sub-period |
| `table_01_baseline.tex` | `output/latex/` | Baseline regression results (6 specs) |
| `table_02_robustness.tex` | `output/latex/` | Robustness checks (7 specifications) |
| `table_03_distance_vs_timing.tex` | `output/latex/` | Geography vs trading-time horse race |
| `table_04_overlap_matrix.tex` | `output/latex/` | Normalised overlap shares matrix |
| `table_05_volume_weighted_overlap.tex` | `output/latex/` | Volume-weighted overlap matrix |
| `table_06_summary_statistics.tex` | `output/latex/` | Panel summary statistics |
| `table_07_subperiod_analysis.tex` | `output/latex/` | Sub-period regression results |
| `table_08_chow_test.tex` | `output/latex/` | Chow test: structural break at 2015 |
| `log_YYYYMMDD_HHMMSS.txt` | `output/logs/` | Timestamped pipeline log |

---

## Configuration

Edit `config/pipeline_config.yml` to change any parameter:

```yaml
paths:
  raw_data:   "data/raw"
  clean_data: "data/clean"
  plots:      "output/plots"
  tables:     "output/tables"
  latex:      "output/latex"
  logs:       "output/logs"

sample:
  start_date: "2008-01-01"
  end_date:   "2026-01-31"

parameters:
  stress_vix_threshold:  25
  stress_vix_percentile: 75      # VIX > 75th percentile = high stress
  min_trading_days:      15      # Minimum trading days per month
  random_seed:           42
```

To change sample period or stress threshold: edit `pipeline_config.yml` and rerun from `02_construct_panel.R`.

---

## Sample Markets

| Market | Index | Ticker | UTC Open | UTC Close | Volume | Type |
|---|---|---|---|---|---|---|
| Japan | Nikkei 225 | ^N225 | 00:00 | 06:00 | ✓ | Developed |
| Hong Kong | Hang Seng | ^HSI | 01:30 | 08:00 | ✓ | Developed |
| Australia | ASX 200 | ^AXJO | 23:00 | 29:00* | ⚠️ | Developed |
| United Kingdom | FTSE 100 | ^FTSE | 08:00 | 16:30 | ✓ | Developed |
| Germany | DAX | ^GDAXI | 08:00 | 16:30 | ✓ | Developed |
| France | CAC 40 | ^FCHI | 08:00 | 16:30 | ✓ | Developed |
| United States | S&P 500 | ^GSPC | 14:30 | 21:00 | ✓ | Developed |
| Canada | TSX Composite | ^GSPTSE | 14:30 | 21:00 | ✓ | Developed |
| Brazil | Bovespa | ^BVSP | 13:00 | 21:00 | ✓ | Emerging |
| India | Nifty 50 | ^NSEI | 03:45 | 10:00 | ⚠️ | Emerging |

\* Australia normalised to 23:00–29:00 UTC to capture overnight session overlap.  
⚠️ Unreliable volume (AU: 53% zero-volume days; IN: 28%). Excluded from volume-weighted specs only.

---

## How to Add a New Country

Adding a new market requires changes to **one file only**: `R/01_download_and_prepare_data.R`. Everything downstream updates automatically.

### Step 1 — Find the Yahoo Finance ticker

Go to [finance.yahoo.com](https://finance.yahoo.com) and search for the country's main equity index. Common examples:

| Country | Index | Ticker |
|---|---|---|
| South Korea | KOSPI | ^KS11 |
| South Africa | JSE All Share | ^J203.JO |
| Mexico | IPC | ^MXX |
| China | SSE Composite | 000001.SS |
| Taiwan | TWSE | ^TWII |
| Singapore | STI | ^STI |

Check that historical data goes back to at least 2008 and that adjusted prices download without errors.

### Step 2 — Find the trading hours in UTC

Look up exchange opening and closing times in local time and convert to UTC. Use non-DST hours for consistency (the pipeline uses fixed UTC). A useful resource: [tradinghours.com](https://www.tradinghours.com).

**Special case — overnight sessions:** If the market closes after midnight UTC, express the close as hours > 24. Example: Australia closes at 05:00 UTC next day → express as 29:00.

### Step 3 — Add to `01_download_and_prepare_data.R`

Three tibbles need updating. Example adding South Korea (KR):

**`markets` tibble:**
```r
markets <- tibble(
  country = c(
    "Japan", "Hong Kong", "Australia",
    "United Kingdom", "Germany", "France",
    "United States", "Canada", "Brazil", "India",
    "South Korea"           # ← add here
  ),
  market = c("JP", "HK", "AU", "UK", "DE", "FR", "US", "CA", "BR", "IN",
             "KR"),         # ← add 2-letter code
  ticker = c(
    "^N225", "^HSI", "^AXJO",
    "^FTSE", "^GDAXI", "^FCHI",
    "^GSPC", "^GSPTSE", "^BVSP", "^NSEI",
    "^KS11"                 # ← add Yahoo ticker
  )
)
```

**`exchange_hours` tibble:**
```r
exchange_hours <- tibble(
  market    = c("JP", "HK", "AU", "UK", "DE", "FR", "US", "CA", "BR", "IN",
                "KR"),
  open_utc  = c( 0.0,  1.5, 23.0,  8.0,  8.0,  8.0, 14.5, 14.5, 13.0,  3.75,
                 0.0),      # ← KSE opens 09:00 KST = 00:00 UTC
  close_utc = c( 6.0,  8.0, 29.0, 16.5, 16.5, 16.5, 21.0, 21.0, 21.0, 10.0,
                 6.5)       # ← KSE closes 15:30 KST = 06:30 UTC
)
```

**`nbfi_intensity` tibble:**
```r
nbfi_intensity <- tibble(
  market     = c("JP", "HK", "AU", "UK", "DE", "FR", "US", "CA", "BR", "IN",
                 "KR"),
  nbfi_share = c(0.12, 0.35, 0.42, 0.48, 0.25, 0.22, 0.65, 0.38, 0.18, 0.14,
                 0.20)      # ← approximate ETF/fund AUM as share of market cap
                            #   Source: ICI World Factbook or FSB NBFI report
)
```

### Step 4 — Check volume data quality

Run this immediately after downloading:

```r
equity_prices %>%
  filter(market == "KR") %>%
  summarise(
    n_days       = n(),
    start        = min(date),
    end          = max(date),
    pct_zero_vol = mean(volume == 0,   na.rm = TRUE),
    pct_na_vol   = mean(is.na(volume), na.rm = TRUE),
    mean_vol     = mean(volume,        na.rm = TRUE)
  )
```

If `pct_zero_vol > 0.10`, add to the exclusion list in the volume weights section:

```r
UNRELIABLE_VOLUME <- c("AU", "IN", "KR")  # ← add new code if needed
```

The new market will still appear in all baseline, time FE, and NBFI specifications. Only the volume-weighted specification is affected.

### Step 5 — Rerun the pipeline

```r
source("main.R")
```

Or rerun from `01` only:

```r
source("R/01_download_and_prepare_data.R")
source("R/02_construct_panel.R")
# ... all subsequent scripts
```

The panel expands automatically:

| Markets | Pairs |
|---|---|
| 10 (current) | 45 |
| 11 | 55 |
| 12 | 66 |
| n | n(n-1)/2 |

### Step 6 — Verify the new pairs

```r
# Check download quality
equity_returns %>%
  filter(market == "KR") %>%
  summarise(n_days = n(), start = min(date), end = max(date),
            pct_missing = mean(is.na(return)))

# Check overlap shares are plausible
overlap_matrix %>%
  filter(market_i == "KR" | market_j == "KR") %>%
  arrange(desc(overlap_share))

# Check panel expanded correctly
monthly_panel %>%
  summarise(
    n_pairs   = n_distinct(pair_id),
    n_obs     = n(),
    new_pairs = n_distinct(pair_id[grepl("KR", pair_id)])
  )
```

---

## Known Issues

| Issue | Cause | Resolution |
|---|---|---|
| Sink stack full on startup | Previous run crashed mid-execution | Handled automatically by `while (sink.number() > 0) sink()` in `main.R` |
| VCOV warning in `07_distance_vs_timing.R` | High collinearity between log distance and overlap (r = −0.958) | `fixest` corrects numerically; SEs in horse-race columns should be interpreted with caution |
| December thin data | Holiday-reduced trading days | Sample ends January 2026 rather than December 2025 |
| AU and IN volume | Yahoo Finance reporting conventions | Excluded from volume-weighted specs; retained everywhere else |
| Distance table observation mismatch | Geographic distance only defined for 8 developed markets | Columns (2)–(4) of distance table use N = 5,995; timing-only uses full N = 9,642 |

---

## Citation

```

```

---

## Contact

Giorgia Galeazzi — Office for National Statistics - giorgia.galeazzi@ons.gov.uk galeazzi.giorgia@gmail.com
