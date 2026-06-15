# ----------------------------------------------------------
# 04_plot_coefficients.R
# Project: When Financial Markets Catch a Cold
# Purpose: Coefficient plot for all model specifications
# Output:  output/plots/coef_plot.png
# ----------------------------------------------------------

message("\n--- 04_plot_coefficients.R ---")

# ----------------------------------------------------------
# 1. Collect coefficients
# ----------------------------------------------------------

coef_df <- bind_rows(
  broom::tidy(baseline_model) %>%
    filter(term == "overlap_stress") %>%
    mutate(model = "Baseline"),
  
  broom::tidy(model_time_fe) %>%
    filter(term == "overlap_stress") %>%
    mutate(model = "Baseline + Time FE"),
  
  broom::tidy(model_volume) %>%
    filter(term == "overlap_stress_vol") %>%
    mutate(model = "Liquidity channel"),
  
  broom::tidy(model_full) %>%
    filter(term %in% c("overlap_stress", "overlap_stress_vol")) %>%
    mutate(model = case_when(
      term == "overlap_stress"     ~ "Timing effect (full model)",
      term == "overlap_stress_vol" ~ "Liquidity effect (full model)"
    ))
) %>%
  mutate(
    conf_low  = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error,
    model     = factor(model, levels = c(
      "Baseline",
      "Baseline + Time FE",
      "Liquidity channel",
      "Timing effect (full model)",
      "Liquidity effect (full model)"
    ))
  )

# ----------------------------------------------------------
# 2. Plot
# ----------------------------------------------------------

coef_plot <- ggplot(coef_df, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high), size = 0.5) +
  labs(
    title    = "Trading-Hour Overlap and Financial Contagion",
    subtitle = "Point estimates with 95% confidence intervals",
    x        = "Estimated effect on bilateral equity correlations",
    y        = NULL
  )

# ----------------------------------------------------------
# 3. Save
# ----------------------------------------------------------

save_plot(coef_plot, "coef_plot.png")

message("✓ 04_plot_coefficients.R completed.")