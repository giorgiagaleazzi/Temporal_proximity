# ----------------------------------------------------------
# helpers.R
# Project: When Financial Markets Catch a Cold
# Purpose: Shared I/O helper functions.
#          Source after config.yml is loaded:
#            cfg <- yaml::read_yaml("config.yml")
#            source("helpers.R")
# ----------------------------------------------------------

# ----------------------------------------------------------
# 1. I/O helpers (paths resolved from config)
# ----------------------------------------------------------

save_raw <- function(data, file) {
  path <- file.path(cfg$paths$raw_data, file)
  readr::write_csv(data, path)
  message("  saved -> ", path)
  invisible(path)
}

save_clean <- function(data, file) {
  path <- file.path(cfg$paths$clean_data, file)
  readr::write_csv(data, path)
  message("  saved -> ", path)
  invisible(path)
}

load_raw <- function(file, ...) {
  readr::read_csv(file.path(cfg$paths$raw_data, file), ...)
}

load_clean <- function(file, ...) {
  readr::read_csv(file.path(cfg$paths$clean_data, file), ...)
}

save_plot <- function(plot, file, width = 8, height = 5, dpi = 300) {
  path <- file.path(cfg$paths$plots, file)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  message("  saved -> ", path)
  invisible(path)
}

save_table <- function(text, file) {
  path <- file.path(cfg$paths$tables, file)
  writeLines(text, path)
  message("  saved -> ", path)
  invisible(path)
}

# ----------------------------------------------------------
# 2. Directory initialisation
# ----------------------------------------------------------

init_dirs <- function() {
  dirs <- c(
    cfg$paths$raw_data,
    cfg$paths$clean_data,
    cfg$paths$plots,
    cfg$paths$tables
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  message("✓ Output directories ready.")
}

# ----------------------------------------------------------
# 3. Startup
# ----------------------------------------------------------

init_dirs()
message("✓ helpers.R loaded.")