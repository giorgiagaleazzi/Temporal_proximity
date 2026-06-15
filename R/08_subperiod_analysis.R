# ----------------------------------------------------------
# 08_subperiod_analysis.R
# Project: When Financial Markets Catch a Cold
# Purpose: Sub-period analysis to test structural stability
#          of the timing channel across two regimes:
#          2008-2015 (post-GFC consolidation)
#          2015-2026 (post-crisis normalisation + COVID + tariffs)
# Output:  output/plots/subperiod_timeline_plot.png
#          output/plots/subperiod_coef_plot.png
# ----------------------------------------------------------

message("\n--- 12_subperiod_analysis.R ---")

# ----------------------------------------------------------
# 1. Crisis periods
# ----------------------------------------------------------
crisis_periods <- tibble(
  crisis = c(
    "Global Financial Crisis",
    "European Debt Crisis",
    "COVID-19",
    "Russia–Ukraine War",
    "Liberation Day Tariffs"
  ),
  start = as.Date(c("2008-01-01", "2010-05-01", "2020-03-01", "2022-02-01", "2025-04-01")),
  end   = as.Date(c("2009-06-01", "2012-12-01", "2021-06-01", "2023-12-01", "2025-06-01"))
)

# ----------------------------------------------------------
# 2. Define sub-periods
# ----------------------------------------------------------


SPLIT_DATE <- as.Date("2015-01-01")

panel_early <- monthly_panel %>%
  filter(month <  SPLIT_DATE)

panel_late  <- monthly_panel %>%
  filter(month >= SPLIT_DATE)

message("  Early period: ", min(panel_early$month), " to ", max(panel_early$month),
        " (", nrow(panel_early), " obs)")
message("  Late period:  ", min(panel_late$month),  " to ", max(panel_late$month),
        " (", nrow(panel_late),  " obs)")

# ----------------------------------------------------------
# 3. Baseline model by sub-period
# ----------------------------------------------------------

model_early_baseline <- feols(
  corr_ij ~ overlap_stress | pair_id,
  data    = panel_early,
  cluster = ~pair_id
)

model_late_baseline <- feols(
  corr_ij ~ overlap_stress | pair_id,
  data    = panel_late,
  cluster = ~pair_id
)

# ----------------------------------------------------------
# 4. Time FE model by sub-period
# ----------------------------------------------------------

model_early_time_fe <- feols(
  corr_ij ~ overlap_stress | pair_id + month,
  data    = panel_early,
  cluster = ~pair_id
)

model_late_time_fe <- feols(
  corr_ij ~ overlap_stress | pair_id + month,
  data    = panel_late,
  cluster = ~pair_id
)

# ----------------------------------------------------------
# 5. Volume-weighted model by sub-period
# ----------------------------------------------------------

model_early_volume <- feols(
  corr_ij ~ overlap_stress_vol | pair_id,
  data    = panel_early,
  cluster = ~pair_id
)

model_late_volume <- feols(
  corr_ij ~ overlap_stress_vol | pair_id,
  data    = panel_late,
  cluster = ~pair_id
)

# ----------------------------------------------------------
# 6. NBFI model by sub-period
# ----------------------------------------------------------

model_early_nbfi <- feols(
  corr_ij ~ overlap_stress_nbfi | pair_id,
  data    = panel_early,
  cluster = ~pair_id
)

model_late_nbfi <- feols(
  corr_ij ~ overlap_stress_nbfi | pair_id,
  data    = panel_late,
  cluster = ~pair_id
)

message("✓ All sub-period models estimated.")

# ----------------------------------------------------------
# 7. Formal Chow test: structural break at 2015
# ----------------------------------------------------------

# Interact all regressors with a post-2015 dummy
monthly_panel_chow <- monthly_panel %>%
  mutate(
    post2015            = as.integer(month >= SPLIT_DATE),
    overlap_stress_post = overlap_stress      * post2015,
    overlap_vol_post    = overlap_stress_vol  * post2015,
    overlap_nbfi_post   = overlap_stress_nbfi * post2015
  )

model_chow <- feols(
  corr_ij ~ overlap_stress + overlap_stress_post | pair_id,
  data    = monthly_panel_chow,
  cluster = ~pair_id
)

model_chow_time_fe <- feols(
  corr_ij ~ overlap_stress + overlap_stress_post | pair_id + month,
  data    = monthly_panel_chow,
  cluster = ~pair_id
)

message("✓ Chow test models estimated.")

# ----------------------------------------------------------
# 8. Diagnostic summary
# ----------------------------------------------------------

coef_early    <- coef(model_early_baseline)["overlap_stress"]
coef_late     <- coef(model_late_baseline) ["overlap_stress"]
coef_chow_pre <- coef(model_chow)          ["overlap_stress"]
coef_chow_int <- coef(model_chow)          ["overlap_stress_post"]

message("  Sub-period coefficients (Overlap x Stress):")
message("    Early (2008-2014): ", round(coef_early, 3))
message("    Late  (2015-2026): ", round(coef_late,  3))
message("    Chow interaction (late - early): ", round(coef_chow_int, 3))

if (abs(coef_early - coef_late) / coef_early < 0.30) {
  message("  \u2713 Coefficients stable across sub-periods (<30% change)")
} else {
  message("  \u26a0  Coefficients differ by >30% across sub-periods -- discuss in paper")
}

# ----------------------------------------------------------
# 9. Sub-period comparison plot
# ----------------------------------------------------------

avg_corr_subperiod <- monthly_panel %>%
  filter(!is.na(corr_ij)) %>%
  group_by(month) %>%
  summarise(mean_corr = mean(corr_ij, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = if_else(month < SPLIT_DATE, "2008-2014", "2015-2026"))

# Mean correlation by period (horizontal reference lines)
period_means <- avg_corr_subperiod %>%
  group_by(period) %>%
  summarise(mean_corr = mean(mean_corr), .groups = "drop")

subperiod_plot <- ggplot(avg_corr_subperiod, aes(x = month, y = mean_corr)) +
  geom_rect(
    data = crisis_periods,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = crisis),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  geom_vline(
    xintercept = as.numeric(SPLIT_DATE),
    linetype   = "dashed",
    colour     = "black",
    linewidth  = 0.7
  ) +
  geom_hline(
    data     = period_means,
    aes(yintercept = mean_corr, colour = period),
    linetype = "dotted",
    linewidth = 0.6
  ) +
  geom_line(linewidth = 0.7, colour = "black") +
  annotate("text", x = as.Date("2010-06-01"), y = 0.05,
           label = "Early period\n2008-2014", size = 3, colour = "grey40") +
  annotate("text", x = as.Date("2021-06-01"), y = 0.05,
           label = "Late period\n2015-2026", size = 3, colour = "grey40") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_manual(values = c("2008-2014" = "#2E74B5", "2015-2026" = "#C0392B")) +
  labs(
    title    = "Average Bilateral Equity Correlations by Sub-Period",
    subtitle = "Dashed line = 2015 split; dotted lines = period means",
    x        = NULL,
    y        = "Average bilateral correlation",
    fill     = NULL,
    colour   = "Period mean"
  ) +
  theme(legend.position = "bottom")

save_plot(subperiod_plot, "subperiod_timeline_plot.png", width = 10, height = 5)
message("✓ Sub-period timeline plot saved.")

# ----------------------------------------------------------
# 10. Coefficient comparison plot
# ----------------------------------------------------------

coef_subperiod <- bind_rows(
  broom::tidy(model_early_baseline) %>%
    filter(term == "overlap_stress") %>%
    mutate(period = "2008-2014", spec = "Baseline"),
  broom::tidy(model_late_baseline) %>%
    filter(term == "overlap_stress") %>%
    mutate(period = "2015-2026", spec = "Baseline"),
  broom::tidy(model_early_time_fe) %>%
    filter(term == "overlap_stress") %>%
    mutate(period = "2008-2014", spec = "Time FE"),
  broom::tidy(model_late_time_fe) %>%
    filter(term == "overlap_stress") %>%
    mutate(period = "2015-2026", spec = "Time FE"),
  broom::tidy(model_early_nbfi) %>%
    filter(term == "overlap_stress_nbfi") %>%
    mutate(period = "2008-2014", spec = "NBFI"),
  broom::tidy(model_late_nbfi) %>%
    filter(term == "overlap_stress_nbfi") %>%
    mutate(period = "2015-2026", spec = "NBFI")
) %>%
  mutate(
    conf_low  = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error,
    spec      = factor(spec, levels = c("Baseline", "Time FE", "NBFI"))
  )

subperiod_coef_plot <- ggplot(
  coef_subperiod,
  aes(x = estimate, y = spec, colour = period, shape = period)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(
    aes(xmin = conf_low, xmax = conf_high),
    size     = 0.6,
    position = position_dodge(width = 0.4)
  ) +
  scale_colour_manual(values = c("2008-2014" = "#2E74B5", "2015-2026" = "#C0392B")) +
  labs(
    title    = "Timing Channel by Sub-Period",
    subtitle = "Point estimates with 95% confidence intervals",
    x        = "Estimated effect on bilateral equity correlations",
    y        = NULL,
    colour   = NULL,
    shape    = NULL
  ) +
  theme(legend.position = "bottom")

save_plot(subperiod_coef_plot, "subperiod_coef_plot.png", width = 8, height = 4)
message("✓ Sub-period coefficient plot saved.")
message("✓ 08_subperiod_analysis.R completed.")
