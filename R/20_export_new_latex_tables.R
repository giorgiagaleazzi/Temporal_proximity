# ----------------------------------------------------------
# 17_export_new_latex_tables.R
# Project: When Financial Markets Catch a Cold
# Purpose: Export leave-one-out results as LaTeX table.
#          (Rolling-window results are better shown as a
#           figure; the LOO table is the key new addition.)
# Output:  output/latex/table_10_leave_one_out.tex
# ----------------------------------------------------------

message("\n--- 17_export_new_latex_tables.R ---")

library(knitr)
library(kableExtra)

# ----------------------------------------------------------
# 1. Leave-one-out LaTeX table
# ----------------------------------------------------------

loo_latex <- all_results %>%
  arrange(dropped) %>%
  transmute(
    `Market dropped` = as.character(dropped),
    Estimate         = sprintf("%.3f", estimate),
    `Std. Error`     = sprintf("(%.3f)", std_error),
    `95\\% CI`       = sprintf("[%.3f, %.3f]", conf_low, conf_high),
    `N obs`          = formatC(n_obs, format = "d", big.mark = ",")
  ) %>%
  mutate(
    Estimate     = if_else(`Market dropped` == "Full sample",
                           paste0(Estimate, "$^{***}$"), Estimate)
  )

loo_kable <- kable(
  loo_latex,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  caption   = "Leave-One-Market-Out Robustness\\label{tab:loo}",
  align     = c("l", "r", "r", "r", "r")
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(1, bold = TRUE) %>%   # highlight Full sample row
  add_header_above(c(" " = 1,
                     "Overlap $\\\\times$ Stress coefficient" = 4)) %>%
  footnote(
    general = paste0(
      "Dependent variable: monthly bilateral equity return correlation. ",
      "Each row drops all pairs involving the named market and re-estimates ",
      "the baseline specification (pair FE, SE clustered by pair). ",
      "The full-sample estimate (first row) is the Table~1 baseline. ",
      "Signif. codes: *** p<0.001, ** p<0.01, * p<0.05."
    ),
    general_title = "",
    escape        = FALSE,
    threeparttable = TRUE
  )

latex_path <- file.path(LATEX_DIR, "table_10_leave_one_out.tex")
writeLines(as.character(loo_kable), latex_path)
message("  saved -> ", latex_path)

message("\u2713 20_export_new_latex_tables.R completed.")
