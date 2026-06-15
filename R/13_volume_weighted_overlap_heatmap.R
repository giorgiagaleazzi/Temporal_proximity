# ----------------------------------------------------------
# 13_volume_weighted_overlap_heatmap.R
# Project: When Financial Markets Catch a Cold
# Purpose: Volume-weighted trading-hour overlap heatmap and LaTeX table
# Output:  output/plots/volume_weighted_overlap_heatmap.png
# ----------------------------------------------------------

message("\n--- 10_volume_weighted_overlap_heatmap.R ---")

# ----------------------------------------------------------
# 1. Construct volume-weighted overlap
# ----------------------------------------------------------
# overlap_df and volume_weights carried from 09 and 01

vw_overlap_df <- overlap_df %>%
  left_join(volume_weights %>% rename(market_i = market, weight_i = weight),
            by = "market_i") %>%
  left_join(volume_weights %>% rename(market_j = market, weight_j = weight),
            by = "market_j") %>%
  mutate(volume_weighted_overlap = overlap_share * (weight_i + weight_j) / 2)

# ----------------------------------------------------------
# 2. Heatmap
# ----------------------------------------------------------

heatmap_vw <- ggplot(vw_overlap_df, aes(x = market_j, y = market_i,
                                        fill = volume_weighted_overlap)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_viridis_c(option = "viridis", name = "Volume-weighted\noverlap") +
  coord_fixed() +
  labs(
    title    = "Volume-Weighted Trading-Hour Overlap",
    subtitle = "Weights based on average traded value",
    x        = "Market j",
    y        = "Market i"
  ) +
  theme(
    panel.grid  = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_plot(heatmap_vw, "volume_weighted_overlap_heatmap.png", width = 8, height = 6)

message("✓ 13_volume_weighted_overlap_heatmap.R completed.")