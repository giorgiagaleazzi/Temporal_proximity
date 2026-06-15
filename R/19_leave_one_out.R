# ----------------------------------------------------------
# 19_leave_one_out.R
# Project: When Financial Markets Catch a Cold
# Purpose: Leave-one-market-out robustness check.
#          Drops each market in turn and re-estimates the
#          baseline timing channel. Confirms the headline
#          result does not hinge on any single market
#          (especially Brazil or India, which are the
#          'decisive cases' in the identification figures).
# Output:  output/plots/leave_one_out_plot.png
#          output/tables/table_leave_one_out.txt
# ----------------------------------------------------------

message("\n--- 16_leave_one_out.R ---")

# ----------------------------------------------------------
# 1. All markets in sample
# ----------------------------------------------------------

all_markets <- union(
  unique(monthly_panel$market_i),
  unique(monthly_panel$market_j)
)

message("  Markets: ", paste(sort(all_markets), collapse = ", "))

# ----------------------------------------------------------
# 2. Full-sample baseline (for reference line)
# ----------------------------------------------------------

m_full <- feols(
  corr_ij ~ overlap_stress | pair_id,
  data    = monthly_panel %>% filter(!is.na(corr_ij)),
  cluster = ~pair_id
)

full_coef <- broom::tidy(m_full) %>%
  filter(term == "overlap_stress") %>%
  transmute(
    dropped   = "Full sample",
    estimate  = estimate,
    std_error = std.error,
    conf_low  = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error,
    n_obs     = nrow(filter(monthly_panel, !is.na(corr_ij)))
  )

# ----------------------------------------------------------
# 3. Leave-one-market-out loop
# ----------------------------------------------------------

loo_one <- function(drop_market) {
  panel_loo <- monthly_panel %>%
    filter(!is.na(corr_ij),
           market_i != drop_market,
           market_j != drop_market)

  m <- feols(corr_ij ~ overlap_stress | pair_id,
             data    = panel_loo,
             cluster = ~pair_id)

  broom::tidy(m) %>%
    filter(term == "overlap_stress") %>%
    transmute(
      dropped   = drop_market,
      estimate  = estimate,
      std_error = std.error,
      conf_low  = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error,
      n_obs     = nrow(panel_loo)
    )
}

loo_results <- map_dfr(sort(all_markets), loo_one)

all_results <- bind_rows(full_coef, loo_results) %>%
  mutate(
    dropped  = factor(dropped,
                      levels = c("Full sample", sort(all_markets))),
    is_full  = dropped == "Full sample"
  )

message("  Leave-one-out estimates:")
print(all_results %>% select(dropped, estimate, std_error, n_obs))

# ----------------------------------------------------------
# 4. Plot
# ----------------------------------------------------------
loo_plot <- ggplot(all_results,
                   aes(x = estimate, y = factor(dropped, levels = rev(levels(dropped))),
                       colour = is_full, shape = is_full)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = full_coef$estimate,
             linetype = "dotted", colour = "#C0392B", linewidth = 0.6) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high), size = 0.5) +
  scale_colour_manual(values = c("TRUE" = "#C0392B", "FALSE" = "#2E74B5"),
                      labels = c("TRUE" = "Full sample", "FALSE" = "LOO estimate"),
                      name   = NULL) +
  scale_shape_manual(values = c("TRUE" = 18, "FALSE" = 16),
                     labels = c("TRUE" = "Full sample", "FALSE" = "LOO estimate"),
                     name   = NULL) +
  labs(
    title    = "Leave-One-Market-Out Robustness",
    subtitle = "Baseline Overlap \u00d7 Stress coefficient; pair FE; 95% CI\nDotted red line = full-sample estimate",
    x        = "Estimated effect on bilateral equity correlations",
    y        = "Market dropped"
  ) +
  theme(legend.position = "bottom")

save_plot(loo_plot, "leave_one_out_plot.png", width = 8, height = 5)
message("\u2713 Leave-one-out plot saved.")

# ----------------------------------------------------------
# 5. Export table
# ----------------------------------------------------------

loo_export <- all_results %>%
  transmute(
    `Market dropped` = as.character(dropped),
    Estimate         = round(estimate,  3),
    Std.Error        = round(std_error, 3),
    CI.Low           = round(conf_low,  3),
    CI.High          = round(conf_high, 3),
    N                = n_obs
  )

save_table(
  capture.output(print(loo_export, n = Inf)),
  "table_leave_one_out.txt"
)

message("\u2713 19_leave_one_out.R completed.")
