# ============================================================
# Install all required packages for the Quant Research Platform
# Run this once before launching the dashboard.
# ============================================================

pkgs <- c(
  # Dashboard
  "shiny", "bslib", "bsicons", "plotly", "DT", "shinyjs",

  # Financial data ingestion
  "quantmod", "fredr", "WDI",

  # Time series & econometrics
  "forecast", "rugarch", "tseries",

  # Data wrangling
  "dplyr", "tidyr", "purrr", "readr", "lubridate", "tibble", "zoo",

  # Utilities
  "glue", "yaml", "logger", "here",

  # Visualisation
  "ggplot2", "scales", "plotly",

  # Reporting
  "rmarkdown", "kableExtra", "gridExtra",

  # Optimisation
  "Matrix", "quadprog"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing) == 0) {
  message("All packages already installed. Ready to launch.")
} else {
  message(sprintf(
    "Installing %d missing package(s): %s",
    length(missing),
    paste(missing, collapse = ", ")
  ))
  install.packages(missing, repos = "https://cloud.r-project.org")
  still_missing <- missing[!vapply(missing, requireNamespace,
                                    quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(still_missing) > 0) {
    warning(sprintf("Failed to install: %s", paste(still_missing, collapse = ", ")))
  } else {
    message("All packages installed successfully.")
  }
}
