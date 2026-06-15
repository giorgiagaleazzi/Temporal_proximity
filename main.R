# ========================================================== 
# main.R ----
# Project: When Financial Markets Catch a Cold
# Purpose: Master replication pipeline
# ==========================================================

rm(list = ls())

# Reset sink stack in case of previous incomplete run
while (sink.number() > 0) sink()

# ------------------------------------------------------------------------------ 
# 1. SETUP ----
# ------------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("  WHEN FINANCIAL MARKETS CATCH A COLD\n")
cat("  Replication Pipeline\n")
cat(strrep("=", 60), "\n\n")

start_time <- proc.time()

# Working directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  script_dir <- getSrcDirectory(function(x) { x })
  if (nchar(script_dir) > 0) setwd(script_dir)
}
cat("Working directory:", getwd(), "\n")

# ------------------------------------------------------------------------------
# 2. PACKAGES ----
# ------------------------------------------------------------------------------

cat("\nStep 1: Loading packages...\n")

required_packages <- c(
  "broom",
  "dplyr",
  "fixest",
  "geosphere",
  "ggplot2",
  "kableExtra",
  "knitr",
  "lubridate",
  "lmtest",
  "plm",
  "purrr",
  "quantmod",
  "readr",
  "sandwich",
  "stringr",
  "tibble",
  "tidyr",
  "tidyquant",
  "yaml"
)

new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  message("  Installing missing packages: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs)
}

invisible(lapply(required_packages, library, character.only = TRUE))
cat("  Packages loaded.\n")

# ------------------------------------------------------------------------------
# 3. CONFIGURATION ----
# ------------------------------------------------------------------------------

cat("\nStep 2: Loading configuration...\n")

config_file <- "config/pipeline_config.yml"
if (!file.exists(config_file)) {
  stop("Configuration file not found: ", config_file)
}
cfg <- yaml::read_yaml(config_file)
source("config/config.R")

# ------------------------------------------------------------------------------
# 4. LOGGING ----
# ------------------------------------------------------------------------------

cat("\nStep 3: Starting log...\n")

log_file <- file.path(cfg$paths$logs,
                      paste0("log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
sink(log_file, split = TRUE)
cat("Log file:", log_file, "\n")

# ------------------------------------------------------------------------------
# 5. DATA ----
# ------------------------------------------------------------------------------

cat("\nStep 4: Downloading and preparing data...\n")
source("R/01_download_and_prepare_data.R")

# ------------------------------------------------------------------------------
# 6. PANEL CONSTRUCTION ----
# ------------------------------------------------------------------------------

cat("\nStep 5: Constructing bilateral monthly panel...\n")
source("R/02_construct_panel.R")

# ------------------------------------------------------------------------------
# 7. EMPIRICAL ANALYSIS ----
# ------------------------------------------------------------------------------

cat("\nStep 6: Running empirical analysis...\n")
source("R/03_empirical_analysis.R")

# ------------------------------------------------------------------------------
# 8. ROBUSTNESS AND EXTENSIONS ----
# ------------------------------------------------------------------------------

cat("\nStep 7: Running robustness checks and extensions...\n")

source("R/06_robustness_check.R")
source("R/07_distance_vs_timing.R")
source("R/08_subperiod_analysis.R")
source("R/09_brazil_identification_test.R")
source("R/10_india_identification_test.R")
source("R/17_opening_gap_test.R")
source("R/18_rolling_window_analysis.R")
source("R/19_leave_one_out.R")
source("R/16_domino_effect.R")   # exploratory: sequential transmission


# ------------------------------------------------------------------------------
# 9. FIGURES ----
# ------------------------------------------------------------------------------

cat("\nStep 8: Producing figures...\n")

source("R/04_plot_coefficients.R")
source("R/05_overlap_tercile_plot.R")
source("R/11_crisis_timeline_plot.R")
source("R/12_overlap_heatmap_and_table.R")
source("R/13_volume_weighted_overlap_heatmap.R")

# ------------------------------------------------------------------------------
# 10. EXPORT TABLES ----
# ------------------------------------------------------------------------------

cat("\nStep 9: Exporting LaTeX tables...\n")

source("R/14_export_latex_tables.R")

# ------------------------------------------------------------------------------
# 11. INFOGRAPHIC ----
# ------------------------------------------------------------------------------

cat("\nStep 10: Generating infographic...\n")

source("R/15_infographic.R")

# ------------------------------------------------------------------------------
# 12. DONE ----
# ------------------------------------------------------------------------------

sink()

elapsed <- proc.time() - start_time
cat("\n", strrep("=", 60), "\n")
cat("\u2605 Pipeline completed successfully.\n",
    sprintf(" Total runtime: %.1f minutes\n", elapsed["elapsed"] / 60),
    " Outputs saved to:\n",
    "    Plots  :", PLOTS_DIR,  "\n",
    "    Tables :", TABLES_DIR, "\n",
    "    LaTeX  :", LATEX_DIR,  "\n",
    "    Logs   :", cfg$paths$logs, "\n")
#cat(strrep("=", 60), "\n")
