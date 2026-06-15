# ----------------------------------------------------------
# 11_crisis_timeline_plot.R
# Project: When Financial Markets Catch a Cold
# Purpose: Average bilateral correlations across major crisis episodes
# Output:  output/plots/crisis_timeline_plot.png
# ----------------------------------------------------------

message("\n--- 08_crisis_timeline_plot.R ---")

# ----------------------------------------------------------
# 1. Monthly average correlations
# ----------------------------------------------------------

avg_corr <- monthly_panel %>%
  group_by(month) %>%
  summarise(mean_corr = mean(corr_ij, na.rm = TRUE), .groups = "drop")

# ----------------------------------------------------------
# 2. Plot
# ----------------------------------------------------------

crisis_plot <- ggplot(avg_corr, aes(x = month, y = mean_corr)) +
  geom_rect(
    data        = crisis_periods,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = crisis),
    inherit.aes = FALSE,
    alpha       = 0.12
  ) +
  geom_line(linewidth = 0.8, colour = "black") +
  labs(
    title    = "Average Bilateral Equity Correlations",
    subtitle = "Shaded areas indicate major global stress episodes",
    x        = NULL,
    y        = "Average bilateral correlation",
    fill     = NULL
  ) +
  theme(legend.position = "bottom")

# ----------------------------------------------------------
# 3. Save
# ----------------------------------------------------------

save_plot(crisis_plot, "crisis_timeline_plot.png", width = 9, height = 5)
message("✓ 11_crisis_timeline_plot.R completed.")