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

  level_map <- list(
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
  logger::log_info("[logger] Initialised at level {cfg$logging$level} -> {log_file}",
                   .topenv = environment())
  invisible(NULL)
}

# Convenience wrappers that preserve the calling environment for glue evaluation.
# Without forwarding .topenv, logger::log_* evaluates glue strings inside the
# wrapper's own empty frame — {var} references in the caller never resolve.
#' @export
log_debug <- function(..., .topenv = parent.frame()) logger::log_debug(..., .topenv = .topenv)
#' @export
log_info  <- function(..., .topenv = parent.frame()) logger::log_info(...,  .topenv = .topenv)
#' @export
log_warn  <- function(..., .topenv = parent.frame()) logger::log_warn(...,  .topenv = .topenv)
#' @export
log_error <- function(..., .topenv = parent.frame()) logger::log_error(..., .topenv = .topenv)

#' Time a block of code and log its duration
#'
#' @param label Human-readable label for the operation
#' @param expr  Expression to time
#' @export
log_timed <- function(label, expr) {
  t0  <- proc.time()["elapsed"]
  res <- force(expr)
  dt  <- round(proc.time()["elapsed"] - t0, 3)
  log_info("[timer] {label} completed in {dt}s", .topenv = environment())
  invisible(res)
}
