# =============================================================================
# Configuration Management
# Loads and validates platform-wide settings from config/settings.yml
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(here)
})

#' Load platform configuration
#'
#' Reads settings.yml and resolves all paths relative to the project root.
#' Environment variables override YAML values where applicable.
#'
#' @param config_path Path to settings.yml (default: config/settings.yml)
#' @return Named list of configuration parameters
#' @export
load_config <- function(config_path = here::here("config", "settings.yml")) {
  if (!file.exists(config_path)) {
    stop(sprintf("[config] Settings file not found: %s", config_path))
  }

  cfg <- yaml::read_yaml(config_path)

  # Environment variable overrides
  if (nchar(Sys.getenv("FRED_API_KEY")) > 0) {
    cfg$data_sources$fred$api_key <- Sys.getenv("FRED_API_KEY")
  }
  if (nchar(Sys.getenv("PLATFORM_ENV")) > 0) {
    cfg$platform$environment <- Sys.getenv("PLATFORM_ENV")
  }

  # Resolve all paths relative to project root
  root <- here::here()
  cfg$paths <- lapply(cfg$paths, function(p) file.path(root, p))

  cfg
}

#' Ensure all required directories exist
#'
#' @param cfg Configuration list from load_config()
#' @return Invisibly returns cfg
#' @export
ensure_directories <- function(cfg) {
  dirs <- unlist(cfg$paths)
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(cfg)
}

#' Get a single configuration value by dot-path
#'
#' @param cfg Configuration list
#' @param path Dot-separated key path e.g. "portfolio.risk_free_rate"
#' @return The configuration value
#' @export
cfg_get <- function(cfg, path) {
  keys  <- strsplit(path, "\\.")[[1]]
  value <- cfg
  for (k in keys) {
    if (!is.list(value) || is.null(value[[k]])) {
      stop(sprintf("[config] Key not found: %s", path))
    }
    value <- value[[k]]
  }
  value
}

# Module-level singleton — loaded once per session
.config_env <- new.env(parent = emptyenv())

#' Get or initialise the global config singleton
#' @export
get_config <- function() {
  if (is.null(.config_env$cfg)) {
    .config_env$cfg <- load_config()
    ensure_directories(.config_env$cfg)
  }
  .config_env$cfg
}
