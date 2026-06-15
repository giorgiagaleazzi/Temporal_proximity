# ----------------------------------------------------------
# 18_rolling_window_analysis.R
# Project: When Financial Markets Catch a Cold
# Purpose: Rolling-window estimation of the timing channel
#          (Overlap x Stress) to visualise how the effect
#          evolves continuously over time, complementing the
#          single 2015 Chow-test split.
# Output:  output/plots/rolling_window_coef_plot.png
#          output/tables/table_rolling_window.txt
# ----------------------------------------------------------

message("\n--- 15_rolling_window_analysis.R ---")

library(lubridate)

# ----------------------------------------------------------
# 1. Settings
# ----------------------------------------------------------

WINDOW_YEARS  <- 5
WINDOW_MONTHS <- WINDOW_YEARS * 12
STEP_MONTHS   <- 6

all_months <- monthly_panel %>%
  filter(!is.na(corr_ij)) %>%
  distinct(month) %>%
  arrange(month) %>%
  pull(month)

window_ends <- seq(
  from = min(all_months) %m+% months(WINDOW_MONTHS - 1),
  to   = max(all_months),
  by   = paste(STEP_MONTHS, "months")
)

message("  ", length(window_ends), " rolling windows of ", WINDOW_YEARS,
        " years, stepped every ", STEP_MONTHS, " months")

# ----------------------------------------------------------
# 2. Estimate baseline timing-channel coefficient per window
# ----------------------------------------------------------

estimate_window <- function(window_end) {
  window_start <- window_end %m-% months(WINDOW_MONTHS - 1)
  panel_w      <- monthly_panel %>%
    filter(month >= window_start, month <= window_end, !is.na(corr_ij))

  if (nrow(panel_w) < 100 || sd(panel_w$overlap_stress, na.rm = TRUE) == 0)
    return(tibble(window_end = window_end, window_start = window_start,
                  estimate = NA_real_, std_error = NA_real_, n_obs = nrow(panel_w)))

  m <- feols(corr_ij ~ overlap_stress | pair_id,
             data = panel_w, cluster = ~pair_id)

  tidy_m <- broom::tidy(m) %>% filter(term == "overlap_stress")
  tibble(window_end = window_end, window_start = window_start,
         estimate = tidy_m$estimate, std_error = tidy_m$std.error,
         n_obs = nrow(panel_w))
}

rolling_results <- map_dfr(window_ends, estimate_window) %>%
  mutate(conf_low  = estimate - 1.96 * std_error,
         conf_high = estimate + 1.96 * std_error)

message("  Valid windows: ", sum(!is.na(rolling_results$estimate)),
        " / ", nrow(rolling_results))

# ----------------------------------------------------------
# 3. Crisis periods (consistent with rest of pipeline)
# ----------------------------------------------------------

crisis_periods <- tibble(
  crisis = c("Global Financial Crisis", "European Debt Crisis",
             "COVID-19", "Russia\u2013Ukraine War", "Liberation Day Tariffs"),
  start  = as.Date(c("2008-01-01","2010-05-01","2020-03-01","2022-02-01","2025-04-01")),
  end    = as.Date(c("2009-06-01","2012-12-01","2021-06-01","2023-12-01","2025-06-01"))
)

# ----------------------------------------------------------
# 4. Plot
# ----------------------------------------------------------

rolling_plot <- ggplot(rolling_results, aes(x = window_end, y = estimate)) +
  geom_rect(
    data = crisis_periods,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = crisis),
    inherit.aes = FALSE, alpha = 0.12
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              fill = "#2E74B5", alpha = 0.20, na.rm = TRUE) +
  geom_line(colour = "#2E74B5", linewidth = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = as.numeric(as.Date("2015-01-01")),
             linetype = "dotted", colour = "black", linewidth = 0.6) +
  annotate("text", x = as.Date("2015-06-01"), y = Inf,
           label = "2015 split", vjust = 1.5, hjust = 0,
           size = 3, colour = "grey40") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Rolling-Window Estimates of the Timing Channel",
    subtitle = paste0(WINDOW_YEARS, "-year rolling windows (stepped every ",
                      STEP_MONTHS, " months); pair FE; shaded band = 95% CI"),
    x        = "Window end date",
    y        = "Overlap \u00d7 Stress coefficient",
    fill     = NULL
  ) +
  theme(legend.position = "bottom")

save_plot(rolling_plot, "rolling_window_coef_plot.png", width = 10, height = 5)
message("\u2713 Rolling-window plot saved.")

# ----------------------------------------------------------
# 5. Export table
# ----------------------------------------------------------

rolling_export <- rolling_results %>%
  filter(!is.na(estimate)) %>%
  transmute(
    Window    = paste0(format(window_start, "%Y-%m"), " \u2013 ",
                       format(window_end,   "%Y-%m")),
    Estimate  = round(estimate,  3),
    Std.Error = round(std_error, 3),
    CI.Low    = round(conf_low,  3),
    CI.High   = round(conf_high, 3),
    N         = n_obs
  )

save_table(capture.output(print(rolling_export, n = Inf)),
           "table_rolling_window.txt")

message("\u2713 18_rolling_window_analysis.R completed.")
