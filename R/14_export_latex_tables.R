# ----------------------------------------------------------
# 14_export_latex_tables.R
# Project: When Financial Markets Catch a Cold
# Purpose: Export all regression and descriptive tables to LaTeX
# Output:  output/latex/table_01_baseline.tex
#          output/latex/table_02_robustness.tex
#          output/latex/table_03_distance_vs_timing.tex
#          output/latex/table_04_overlap_matrix.tex
#          output/latex/table_05_volume_weighted_overlap.tex
#          output/latex/table_06_summary_statistics.tex
#          output/latex/table_07_subperiod_analysis.tex
#          output/latex/table_08_chow_test.tex
#          output/latex/table_09_overlap_shares_table.tex
# ----------------------------------------------------------

message("\n--- 11_export_latex_tables.R ---")

# ----------------------------------------------------------
# 1. Table 1 — Baseline regression results
# ----------------------------------------------------------

t1 <- etable(
  baseline_model,
  model_time_fe,
  model_volume,
  model_full,
  model_nbfi,
  headers   = c("Baseline", "Pair + Time FE", "Liquidity", "Full", "NBFI"),
  keep      = c("%overlap_stress$", "%overlap_stress_vol", "%overlap_stress_nbfi"),
  dict      = c(
    overlap_stress      = "Overlap $\\times$ Stress",
    overlap_stress_vol  = "Overlap $\\times$ Volume $\\times$ Stress",
    overlap_stress_nbfi = "Overlap $\\times$ NBFI $\\times$ Stress"
  ),
  se        = "cluster",                    # clustered SE
  cluster   = ~pair_id,                     # cluster variable
  digits    = 3,                            # 3 decimal places
  digits.stats = 3,
  signif.code = c(                          # significance stars
    "***" = 0.001,
    "**"  = 0.01,
    "*"   = 0.05,
    "."   = 0.10
  ),
  fitstat   = c("n", "r2", "wr2"),
  notes     = "Dependent variable: monthly bilateral equity return correlation. All specifications include market-pair fixed effects. Standard errors clustered by pair in parentheses. Observations = 9,642 (10 markets, 45 pairs, 2008--2026). AU and IN excluded from volume weights due to unreliable Yahoo Finance volume data.",
  title     = "Trading-Hour Overlap and Financial Contagion",
  label     = "tab:baseline",
  tex       = TRUE
)

save_latex(t1, "table_01_baseline.tex")
message("  saved -> Table 1: Baseline results")

# ----------------------------------------------------------
# 2. Table 2 — Robustness checks
# ----------------------------------------------------------

t2 <- etable(
  baseline_model,
  model_time_fe_robust,
  model_volume_robust,
  model_full_robust,
  model_min_days,
  model_alt_stress,
  model_nonstress,
  headers   = c("Baseline", "Two-way", "Liq.(2w)", "Full(2w)", "MinDays", "AltStress", "Placebo"),
  keep      = c("%overlap_stress$", "%overlap_stress_vol",
                "%overlap_stress_alt", "%overlap_nonstress"),
  dict      = c(
    overlap_stress      = "Overlap $\\times$ Stress",
    overlap_stress_vol  = "Overlap $\\times$ Volume $\\times$ Stress",
    overlap_stress_alt  = "Overlap $\\times$ Alt. Stress",
    overlap_nonstress   = "Overlap $\\times$ Non-stress"
  ),
  digits    = 3,
  digits.stats = 3,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
  fitstat   = c("n", "r2", "wr2"),
  notes     = "Col (1) reproduces baseline with pair-clustered SE. Cols (2)--(4) use two-way clustering by pair and month. Col (5) restricts to months with $\\geq$15 trading days. Col (6) uses 80th percentile VIX threshold. Col (7) placebo: non-stress periods.",
  title     = "Robustness Analysis",
  label     = "tab:robustness",
  tex       = TRUE
)

save_latex(t2, "table_02_robustness.tex")
message("  saved -> Table 2: Robustness checks")

# ----------------------------------------------------------
# 3. Table 3 — Geography versus trading-time
# ----------------------------------------------------------
t3 <- etable(
  model_timing,
  model_distance,
  model_horse_race,
  model_full_dist,
  headers   = c("Timing only", "Distance only", "Horse race", "Full"),
  keep      = c("%dist_stress", "%overlap_stress$", "%overlap_stress_vol"),
  dict      = c(
    dist_stress         = "Distance $\\times$ Stress",
    overlap_stress      = "Overlap $\\times$ Stress",
    overlap_stress_vol  = "Overlap $\\times$ Volume $\\times$ Stress"
  ),
  digits    = 3,
  digits.stats = 3,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
  fitstat   = c("n", "r2", "wr2"),
  notes     = "Distance $\\times$ Stress: interaction between log Haversine distance and high-VIX dummy. All specifications include pair fixed effects. SE two-way clustered by pair and month. Cols (2)--(4) use the 8-market sub-sample (N = 5,995) as geographic coordinates are defined only for original markets. Note: VCOV matrix not positive definite in Cols (3)--(4) due to high collinearity between log distance and overlap share ($r = -0.958$); SE corrected numerically by \\texttt{fixest}.",
  title     = "Financial Contagion: Geography versus Trading-Time",
  label     = "tab:distance_timing",
  tex       = TRUE
)

save_latex(t3, "table_03_distance_vs_timing.tex")
message("  saved -> Table 3: Geography vs timing")

# ----------------------------------------------------------
# 4. Table 4 — Trading-hour overlap matrix
# ----------------------------------------------------------

t4 <- overlap_matrix %>%
  pivot_wider(names_from = market_j, values_from = overlap_share) %>%
  arrange(market_i) %>%
  rename(Market = market_i) %>%
  mutate(across(where(is.numeric), ~sprintf("%.2f", .))) %>%
  mutate(across(everything(), ~ifelse(. == "NA", "\\textemdash", .))) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = "Normalised Trading-Hour Overlap Shares",
    label    = "tab:overlap_matrix"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

save_latex(t4, "table_04_overlap_matrix.tex")
message("  saved -> Table 4: Overlap matrix")

# ----------------------------------------------------------
# 5. Table 5 — Volume-weighted overlap matrix
# ----------------------------------------------------------

t5 <- vw_overlap_df %>%
  select(market_i, market_j, volume_weighted_overlap) %>%
  pivot_wider(names_from = market_j, values_from = volume_weighted_overlap) %>%
  arrange(market_i) %>%
  rename(Market = market_i) %>%
  select(Market, AU, BR, CA, DE, FR, HK, IN, JP, UK, US) %>%
  mutate(across(where(is.numeric), ~sprintf("%.3f", .))) %>%
  mutate(across(everything(), ~ifelse(. == "NA", "\\textemdash", .))) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = "Volume-Weighted Trading-Hour Overlap",
    label    = "tab:vw_overlap"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

save_latex(t5, "table_05_volume_weighted_overlap.tex")
message("  saved -> Table 5: Volume-weighted overlap matrix")

# ----------------------------------------------------------
# 6. Table 6 — Panel summary statistics
# ----------------------------------------------------------

t6 <- monthly_panel %>%
  filter(!is.na(corr_ij)) %>%
  summarise(
    across(
      c(corr_ij, overlap_share, overlap_stress, overlap_stress_vol,
        high_stress, n_days, vix_mean),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x,   na.rm = TRUE),
        min  = ~ min(.x,  na.rm = TRUE),
        max  = ~ max(.x,  na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to      = c("variable", "stat"),
    names_pattern = "(.*)_(mean|sd|min|max)"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(variable = recode(variable,
                           corr_ij            = "Bilateral correlation",
                           overlap_share      = "Trading-hour overlap",
                           overlap_stress     = "Overlap $\\times$ Stress",
                           overlap_stress_vol = "Overlap $\\times$ Volume $\\times$ Stress",
                           high_stress        = "Stress indicator",
                           n_days             = "Trading days per month",
                           vix_mean           = "VIX (monthly mean)"
  )) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    digits   = 3,
    col.names = c("Variable", "Mean", "SD", "Min", "Max"),
    caption  = "Summary Statistics",
    label    = "tab:summary_stats",
    escape   = FALSE
  ) %>%
  kable_styling(latex_options = c("hold_position"))

save_latex(t6, "table_06_summary_statistics.tex")
message("  saved -> Table 6: Summary statistics")

# ----------------------------------------------------------
# 7. Table 7 — Sub-period analysis
# ----------------------------------------------------------

t7 <- etable(
  model_early_baseline,
  model_late_baseline,
  model_early_time_fe,
  model_late_time_fe,
  model_early_volume,
  model_late_volume,
  model_early_nbfi,
  model_late_nbfi,
  headers = c(
    "Early base", "Late base",
    "Early FE",   "Late FE",
    "Early vol",  "Late vol",
    "Early NBFI", "Late NBFI"
  ),
  keep    = c("%overlap_stress$", "%overlap_stress_vol", "%overlap_stress_nbfi"),
  dict    = c(
    overlap_stress      = "Overlap $\\times$ Stress",
    overlap_stress_vol  = "Overlap $\\times$ Volume $\\times$ Stress",
    overlap_stress_nbfi = "Overlap $\\times$ NBFI $\\times$ Stress"
  ),
  digits      = 3,
  digits.stats = 3,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
  fitstat = c("n", "r2", "wr2"),
  notes   = "Early period: 2008--2014. Late period: 2015--2026. All specifications include pair fixed effects. Standard errors clustered by pair in parentheses.",
  title   = "Sub-Period Analysis: Structural Stability of the Timing Channel",
  label   = "tab:subperiod",
  tex     = TRUE
)

save_latex(t7, "table_07_subperiod_analysis.tex")
message("  saved -> Table 7: Sub-period analysis")


# ----------------------------------------------------------
# 8. Table 8 — Chow test
# ----------------------------------------------------------

# NOTE: the Chow interaction is NEGATIVE, meaning the incremental
# post-2015 effect is smaller once common shocks are absorbed.
# The correct interpretation is structural stability, not intensification.
# The notes argument above has been corrected below — replace t8 with:

t8 <- etable(
  model_chow,
  model_chow_time_fe,
  headers = c("Pair FE", "Pair + Time FE"),
  keep    = c("%overlap_stress$", "%overlap_stress_post"),
  dict    = c(
    overlap_stress      = "Overlap $\\times$ Stress (pre-2015)",
    overlap_stress_post = "Overlap $\\times$ Stress $\\times$ Post-2015"
  ),
  digits       = 3,
  digits.stats = 3,
  signif.code  = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
  fitstat = c("n", "r2", "wr2"),
  notes   = "The interaction term captures the \\textit{change} in the timing channel after 2015 relative to the pre-2015 baseline. A negative coefficient indicates the incremental post-2015 effect is smaller once common shocks are absorbed by time fixed effects; the correct interpretation is structural stability rather than intensification. All specifications include pair fixed effects. SE clustered by pair.",
  title   = "Chow Test: Structural Break at 2015",
  label   = "tab:chow",
  tex     = TRUE
)

save_latex(t8, "table_08_chow_test.tex")
message("  saved -> Table 8: Chow test (corrected note)")

# ----------------------------------------------------------
# 9. Overlab Shares Tables 
# ----------------------------------------------------------
# Build symmetric matrix including diagonal (self-overlap = 1)
markets_ordered <- sort(unique(c(overlap_df$market_i, overlap_df$market_j)))

diagonal_df <- tibble(
  market_i     = markets_ordered,
  market_j     = markets_ordered,
  overlap_share = NA_real_           # shown as "—" below
)

overlap_table <- bind_rows(overlap_df, diagonal_df) %>%
  distinct(market_i, market_j, .keep_all = TRUE) %>%
  mutate(
    overlap_share = ifelse(market_i == market_j, NA_real_, overlap_share),
    market_i = factor(market_i, levels = markets_ordered),
    market_j = factor(market_j, levels = markets_ordered)
  ) %>%
  pivot_wider(names_from = market_j, values_from = overlap_share) %>%
  arrange(market_i) %>%
  rename(Market = market_i)

t9 <- overlap_table %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    digits   = 2,
    na       = "---",
    caption  = "Normalised Trading-Hour Overlap Shares",
    label    = "tab:overlap_shares"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

save_latex(t9, "table_09_overlap_shares_table.tex")
message("\u2713 09_overlap_heatmap_and_table.R completed.")


# ----------------------------------------------------------
# 10. Table 10 — Leave-one-market-out
# ----------------------------------------------------------

stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "^{***}",
    p < 0.01  ~ "^{**}",
    p < 0.05  ~ "^{*}",
    p < 0.1   ~ "^{.}",
    TRUE      ~ ""
  )
}

t10 <- all_results %>%
  arrange(dropped) %>%
  transmute(
    `Market dropped` = as.character(dropped),
    Estimate         = sprintf("$%.3f%s$", estimate,  stars(2 * pnorm(-abs(estimate / std_error)))),
    `Std. Error`     = sprintf("$(%.3f)$", std_error),
    `95\\% CI`       = sprintf("$[%.3f,\\; %.3f]$", conf_low, conf_high),
    `N obs`          = formatC(n_obs, format = "d", big.mark = ",")
  ) %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    caption   = "Leave-One-Market-Out Robustness\\label{tab:loo}",
    align     = c("l", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(1, bold = TRUE) %>%
  add_header_above(
    c(" " = 1, "Overlap $\\\\times$ Stress coefficient" = 4),
    escape = FALSE
  ) %>%
  footnote(
    general = paste0(
      "Dependent variable: monthly bilateral equity return correlation. ",
      "Each row drops all pairs involving the named market and re-estimates ",
      "the baseline specification (pair FE, SE clustered by pair). ",
      "The full-sample estimate (first row, bold) is the Table~1 baseline. ",
      "Signif.\\ codes: ***: 0.001, **: 0.01, *: 0.05, .: 0.1."
    ),
    general_title  = "",
    escape         = FALSE,
    threeparttable = TRUE
  )

save_latex(t10, "table_10_leave_one_out.tex")
message("  saved -> Table 10: Leave-one-market-out")

# ----------------------------------------------------------
# 11. Table 11 — Opening gap: simultaneity vs follow-the-sun
# ----------------------------------------------------------

stars_val <- function(model, var) {
  p <- pvalue(model)[var]
  stars(p)
}

t11 <- data.frame(
  Variable      = c(
    "Opening Gap $\\times$ Stress", "",
    "Overlap $\\times$ Stress",     "",
    "pair\\_id", "month",
    "Observations", "R$^2$", "Within R$^2$"
  ),
  `Gap only` = c(
    sprintf("$%.4f%s$", coef(model_gap)["gap_stress"],
            stars_val(model_gap, "gap_stress")),
    sprintf("$(%.4f)$", se(model_gap)["gap_stress"]),
    "", "",
    "Yes", "Yes",
    formatC(nobs(model_gap),  format = "d", big.mark = ","),
    sprintf("%.3f", r2(model_gap,  type = "r2")),
    sprintf("%.3f", r2(model_gap,  type = "wr2"))
  ),
  `Overlap only` = c(
    "", "",
    sprintf("$%.4f%s$", coef(model_overlap)["overlap_stress"],
            stars_val(model_overlap, "overlap_stress")),
    sprintf("$(%.4f)$", se(model_overlap)["overlap_stress"]),
    "Yes", "Yes",
    formatC(nobs(model_overlap), format = "d", big.mark = ","),
    sprintf("%.3f", r2(model_overlap, type = "r2")),
    sprintf("%.3f", r2(model_overlap, type = "wr2"))
  ),
  `Horse race` = c(
    sprintf("$%.4f%s$", coef(model_horse_race)["gap_stress"],
            stars_val(model_horse_race, "gap_stress")),
    sprintf("$(%.4f)$", se(model_horse_race)["gap_stress"]),
    sprintf("$%.4f%s$", coef(model_horse_race)["overlap_stress"],
            stars_val(model_horse_race, "overlap_stress")),
    sprintf("$(%.4f)$", se(model_horse_race)["overlap_stress"]),
    "Yes", "Yes",
    formatC(nobs(model_horse_race), format = "d", big.mark = ","),
    sprintf("%.3f", r2(model_horse_race, type = "r2")),
    sprintf("%.3f", r2(model_horse_race, type = "wr2"))
  ),
  check.names = FALSE
) %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("", "Gap only", "Overlap only", "Horse race"),
    caption   = "Simultaneous Trading versus Follow-the-Sun Transmission\\label{tab:opening_gap}",
    align     = c("l", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  pack_rows("Variables",      1, 4) %>%
  pack_rows("Fixed-effects",  5, 6) %>%
  pack_rows("Fit statistics", 7, 9) %>%
  footnote(
    general = paste0(
      "Dependent variable: monthly bilateral equity return correlation. ",
      "Opening Gap $= \\\\min(|O_i - O_j|,\\\\; 24 - |O_i - O_j|)$, the shortest arc ",
      "between opening times on the 24-hour clock, so that AU (23:00 UTC) and ",
      "JP (00:00 UTC) register a gap of 1 hour rather than 23 hours. ",
      "All specifications include pair and month fixed effects. SE clustered by pair. ",
      "Correlation between raw measures across pairs: $-0.816$. ",
      "Signif.\\ codes: ***: 0.001, **: 0.01, *: 0.05, .: 0.1."
    ),
    general_title  = "",
    escape         = FALSE,
    threeparttable = TRUE
  )

save_latex(t11, "table_11_opening_gap.tex")
message("  saved -> Table 11: Opening gap")

# ----------------------------------------------------------
# Done
# ----------------------------------------------------------

message("\u2713 14_export_latex_tables.R completed.")
message("  All tables saved to: ", LATEX_DIR)
