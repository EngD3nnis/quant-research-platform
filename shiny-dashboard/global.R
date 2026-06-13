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
  "tibble", "zoo", "Matrix", "quadprog", "kableExtra", "gridExtra"
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

# ---- Load all data ingestion modules ----------------------------------------
# world_bank.R is sourced here so G20_COUNTRIES, WB_INDICATORS, G20_COUNTRY_NAMES,
# WB_INDICATOR_LABELS are available in the global environment for both ui.R and server.R.
source(here::here("R", "ingestion", "world_bank.R"))

# ---- Startup diagnostics ----------------------------------------------------
# Log the state of each external data source so operators can diagnose issues
# without having to dig through config files.

.check_fred_key <- function() {
  key <- cfg$data_sources$fred$api_key
  if (!is.null(key) && nchar(trimws(key)) > 0L) {
    log_info("[startup] FRED API key: CONFIGURED (Macro Explorer should work)")
  } else {
    log_warn(paste(
      "[startup] FRED API key: NOT SET.",
      "The Macro Explorer tab will be rate-limited.",
      "Get a free key: https://fred.stlouisfed.org/docs/api/api_key.html"
    ))
  }
}

.check_data_dirs <- function() {
  dirs <- unlist(cfg$paths)
  missing_dirs <- dirs[!dir.exists(dirs)]
  if (length(missing_dirs) > 0L) {
    for (d in missing_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    log_info("[startup] Created {length(missing_dirs)} missing data directories")
  } else {
    log_info("[startup] All data directories present")
  }
}

.check_fred_key()
.check_data_dirs()

log_info("[startup] Global environment ready — sourced world_bank.R, all modules available")
