# ----------------------------------------------------------
# 05_overlap_tercile_plot.R
# Project: When Financial Markets Catch a Cold
# Purpose: Overlap terciles by stress regime
# Output:  output/plots/overlap_tercile_plot.png
# ----------------------------------------------------------

message("\n--- 05_overlap_tercile_plot.R ---")

# ----------------------------------------------------------
# 1. Construct pair-level overlap terciles
# ----------------------------------------------------------

pair_overlap <- monthly_panel %>%
  distinct(pair_id, overlap_share) %>%
  mutate(
    overlap_tercile = ntile(overlap_share, 3) %>%
      factor(
        levels = 1:3,
        labels = c("Low overlap", "Medium overlap", "High overlap")
      )
  )

# ----------------------------------------------------------
# 2. Merge terciles and collapse to group means
# ----------------------------------------------------------

plot_df <- monthly_panel %>%
  left_join(pair_overlap, by = c("pair_id", "overlap_share")) %>%
  filter(!is.na(corr_ij), !is.na(high_stress), !is.na(overlap_tercile)) %>%
  group_by(overlap_tercile, high_stress) %>%
  summarise(
    mean_corr = mean(corr_ij, na.rm = TRUE),
    se_corr   = sd(corr_ij,   na.rm = TRUE) / sqrt(n()),
    .groups   = "drop"
  ) %>%
  mutate(stress_regime = if_else(high_stress == 1, "Stress", "Non-stress"))

# ----------------------------------------------------------
# 3. Plot
# ----------------------------------------------------------

dodge <- position_dodge(width = 0.25)

tercile_plot <- ggplot(plot_df, aes(x = overlap_tercile, y = mean_corr,
                                    group = stress_regime, colour = stress_regime)) +
  geom_line(position = dodge) +
  geom_point(size = 3, position = dodge) +
  geom_errorbar(
    aes(ymin = mean_corr - 1.96 * se_corr, ymax = mean_corr + 1.96 * se_corr),
    width    = 0.12,
    position = dodge
  ) +
  labs(
    title    = "Financial Comovement by Trading-Hour Overlap",
    subtitle = "Stress versus non-stress periods",
    x        = "Trading-hour overlap tercile",
    y        = "Average bilateral equity correlation",
    colour   = NULL
  )

# ----------------------------------------------------------
# 4. Save
# ----------------------------------------------------------

save_plot(tercile_plot, "overlap_tercile_plot.png")

message("✓ 05_overlap_tercile_plot.R completed.")