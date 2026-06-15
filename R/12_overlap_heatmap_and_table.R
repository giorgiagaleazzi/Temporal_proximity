# ============================================================
# 12_overlap_heatmap_and_table.R
# # Project: When Financial Markets Catch a Cold
# Uses: monthly_panel data with overlap_share and pair_id
# Output:  output/plots/trading_hour_overlap_heatmap.png
# ============================================================

# ----------------------------------------------------------
# 1. Pairwise overlap
# ----------------------------------------------------------
# exchange_hours is available from 01_download_and_prepare_data.R
# AU close is represented as 29.0 (05:00 next day) to
# preserve overnight continuity in the overlap calculation.

overlap_df <- exchange_hours %>%
  rename(market_i = market, open_i = open_utc, close_i = close_utc) %>%
  crossing(
    exchange_hours %>% rename(market_j = market, open_j = open_utc, close_j = close_utc)
  ) %>%
  filter(market_i != market_j) %>%
  mutate(
    overlap_hours = pmax(0, pmin(close_i, close_j) - pmax(open_i, open_j)),
    avg_hours     = ((close_i - open_i) + (close_j - open_j)) / 2,
    overlap_share = overlap_hours / avg_hours
  )

# ------------------------------------------------------------
# 2. EXTRACT UNIQUE OVERLAP VALUES PER PAIR
# ------------------------------------------------------------
# overlap_share is time-invariant, so we take one value per pair.
# pair_id is assumed to be in the format "MARKET1_MARKET2" or
# a numeric/character identifier. Adjust the parsing below
# if your pair_id uses a different separator.

overlap_pairs <- monthly_panel %>%
  select(pair_id, overlap_share) %>%
  distinct() %>%
  # --- Parse pair_id into two market labels ---
  # If your pair_id is e.g. "US_UK" or "US-UK", adjust sep= below.
  # If it is numeric, join to a lookup table instead (see note below).
  separate(pair_id, into = c("market_i", "market_j"),
           sep = "_",       # <-- change to "-" or " " if needed
           remove = FALSE,
           extra = "merge")

# NOTE: if pair_id is numeric (e.g. 1, 2, 3 ...) you need a lookup:
#
# market_lookup <- tibble(
#   market_code = c("US", "CA", "UK", "DE", "FR", "CH", "JP", "HK", "AU"),
#   market_id   = 1:9
# )
# Then join market_i and market_j to market_lookup by id.


# ------------------------------------------------------------
# 3. DETERMINE MARKET ORDER (by average overlap, descending)
# ------------------------------------------------------------
market_order <- overlap_pairs %>%
  pivot_longer(c(market_i, market_j), values_to = "market") %>%
  group_by(market) %>%
  summarise(mean_overlap = mean(overlap_share, na.rm = TRUE)) %>%
  arrange(desc(mean_overlap)) %>%
  pull(market)


# ------------------------------------------------------------
# 4. BUILD SYMMETRIC MATRIX FOR PLOTTING
# ------------------------------------------------------------
# Add mirror rows (j→i) and diagonal (overlap = 1 with itself)
diagonal <- tibble(
  pair_id     = paste0(market_order, "_", market_order),
  market_i    = market_order,
  market_j    = market_order,
  overlap_share = 1.0
)

overlap_full <- bind_rows(
  overlap_pairs,
  overlap_pairs %>% rename(market_i = market_j, market_j = market_i),
  diagonal
) %>%
  distinct(market_i, market_j, .keep_all = TRUE) %>%
  mutate(
    market_i = factor(market_i, levels = market_order),
    market_j = factor(market_j, levels = rev(market_order))
  )


# ------------------------------------------------------------
# 5. PLOT
# ------------------------------------------------------------
# Color palette: white → steel blue → dark navy (publication-quality)
overlap_colors <- c(
  "#F7FBFF",   # ~0.0  very light blue
  "#C6DBEF",   # ~0.25
  "#6BAED6",   # ~0.50
  "#2171B5",   # ~0.75
  "#08306B"    # ~1.0  dark navy
)

p <- ggplot(overlap_full,
            aes(x = market_i, y = market_j, fill = overlap_share)) +
  
  geom_tile(color = "white", linewidth = 0.6) +
  
  geom_text(
    aes(label = ifelse(market_i == market_j, "—",
                       sprintf("%.2f", overlap_share))),
    size  = 3.2,
    color = ifelse(overlap_full$overlap_share > 0.55,
                   "white", "#1a1a2e"),
    fontface = "plain"
  ) +
  
  scale_fill_gradientn(
    colors  = overlap_colors,
    limits  = c(0, 1),
    breaks  = c(0, 0.25, 0.5, 0.75, 1),
    labels  = c("0", "0.25", "0.50", "0.75", "1"),
    name    = "Overlap\nshare",
    guide   = guide_colorbar(
      barwidth  = 0.8,
      barheight = 8,
      ticks     = TRUE,
      frame.colour = "grey40"
    )
  ) +
  
  scale_x_discrete(position = "top") +
  
  labs(
    title    = "Bilateral trading-hour overlap",
    subtitle = "Normalised simultaneous trading hours across equity market pairs",
    caption  = paste0(
      "Note: Overlap\u1d62\u2c7c = max(0, min(C\u1d62,C\u2c7c) \u2212 max(O\u1d62,O\u2c7c)) ",
      "\u00f7 0.5\u00d7[(C\u1d62\u2212O\u1d62)+(C\u2c7c\u2212O\u2c7c)]. ",
      "Darker shading indicates higher overlap. ",
      "Three structural clusters emerge \u2014 European (DE, FR, UK), Americas (BR, CA, US), ",
      "and Asian (HK, IN, JP) \u2014 with near-zero cross-cluster overlap. ",
      "Australia records zero overlap with all markets in the sample."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    # Titles
    plot.title      = element_text(face = "bold", size = 13, hjust = 0,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 10, color = "grey40", hjust = 0,
                                   margin = margin(b = 10)),
    plot.caption    = element_text(size = 8, color = "grey50", hjust = 0,
                                   margin = margin(t = 8)),
    # Axes
    axis.text.x     = element_text(size = 9, angle = 0, hjust = 0.5,
                                   face = "bold"),
    axis.text.y     = element_text(size = 9, face = "bold"),
    axis.title      = element_blank(),
    axis.ticks      = element_blank(),
    # Grid / panel
    panel.grid      = element_blank(),
    panel.background = element_blank(),
    # Legend
    legend.position  = "right",
    legend.title     = element_text(size = 9),
    legend.text      = element_text(size = 8),
    # Overall margins
    plot.margin = margin(12, 12, 12, 12)
  )


# ------------------------------------------------------------
# 6. SAVE
# ------------------------------------------------------------
save_plot(p, "overlap_heatmap.png", width = 9, height = 8)

# PDF:
# ggsave(
#   filename = "overlap_heatmap.pdf",
#   plot     = p,
#   width    = fig_size + 1.5,
#   height   = fig_size,
#   device   = cairo_pdf
# )

