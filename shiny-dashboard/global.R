# =============================================================================
# Shiny Dashboard — Global Setup
# Loaded once at startup before ui.R and server.R.
# Sets up the environment, validates configuration, and pre-loads
# lightweight assets so the dashboard is ready immediately.
# =============================================================================

library(here)

# Ensure we load from project root regardless of working directory
setwd(here::here())

# ---- Package check ----------------------------------------------------------
required_pkgs <- c(
  "shiny", "bslib", "bsicons", "plotly", "DT", "shinyjs",
  "quantmod", "fredr", "WDI", "forecast", "rugarch", "tseries",
  "dplyr", "tidyr", "purrr", "readr", "lubridate", "glue",
  "ggplot2", "scales", "rmarkdown", "yaml", "logger", "here",
  "tibble", "zoo", "Matrix", "quadprog"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing_pkgs) > 0) {
  message(sprintf(
    "[startup] Missing packages: %s\nInstall with: install.packages(c('%s'))",
    paste(missing_pkgs, collapse = ", "),
    paste(missing_pkgs, collapse = "', '")
  ))
}

# ---- Load platform config ---------------------------------------------------
source(here::here("R", "utilities", "config.R"))
source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "visualization", "theme.R"))

cfg <- get_config()
init_logger(cfg)
set_quant_theme()

message(sprintf("[startup] Platform: %s v%s [%s]",
  cfg$platform$name,
  cfg$platform$version,
  cfg$platform$environment))
