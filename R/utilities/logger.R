# =============================================================================
# Structured Logging Subsystem
# Thin wrapper over {logger} providing levelled, timestamped output
# to both console and rotating log file.
# =============================================================================

suppressPackageStartupMessages(library(logger))

#' Initialise the platform logger
#'
#' Reads log level and file path from config, sets up dual appenders
#' (console + file).  Safe to call multiple times — idempotent.
#'
#' @param cfg Configuration list from get_config()
#' @export
init_logger <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- get_config()

  level_map <- c(
    "DEBUG" = logger::DEBUG,
    "INFO"  = logger::INFO,
    "WARN"  = logger::WARN,
    "ERROR" = logger::ERROR
  )

  lvl      <- level_map[[toupper(cfg$logging$level)]]
  log_file <- cfg$logging$file
  log_dir  <- dirname(log_file)

  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

  logger::log_threshold(lvl)
  logger::log_appender(logger::appender_file(log_file))
  if (isTRUE(cfg$logging$console)) {
    logger::log_appender(logger::appender_tee(log_file))
  }

  logger::log_formatter(logger::formatter_glue_or_sprintf)
  log_info("[logger] Initialised at level {cfg$logging$level} → {log_file}")
  invisible(NULL)
}

# Convenience wrappers that prefix the calling module name
#' @export
log_debug <- function(...) logger::log_debug(...)
#' @export
log_info  <- function(...) logger::log_info(...)
#' @export
log_warn  <- function(...) logger::log_warn(...)
#' @export
log_error <- function(...) logger::log_error(...)

#' Time a block of code and log its duration
#'
#' @param label Human-readable label for the operation
#' @param expr  Expression to time
#' @export
log_timed <- function(label, expr) {
  t0  <- proc.time()["elapsed"]
  res <- force(expr)
  dt  <- round(proc.time()["elapsed"] - t0, 3)
  log_info("[timer] {label} completed in {dt}s")
  invisible(res)
}
