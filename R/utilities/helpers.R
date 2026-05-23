# =============================================================================
# General-Purpose Helper Functions
# Pure, side-effect-free utilities used across all modules.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(glue)
})

# ---- Date Utilities ----------------------------------------------------------

#' Convert a vector of dates to the nearest trading day
#' @param dates Date vector
#' @param direction "forward" or "backward"
#' @export
to_trading_day <- function(dates, direction = "forward") {
  dates <- as.Date(dates)
  wday  <- lubridate::wday(dates, week_start = 1)
  delta <- switch(direction,
    forward  = ifelse(wday == 6, 2L, ifelse(wday == 7, 1L, 0L)),
    backward = ifelse(wday == 6, -1L, ifelse(wday == 7, -2L, 0L))
  )
  dates + delta
}

#' Generate a sequence of business days between two dates
#' @param from Start date
#' @param to   End date
#' @return Date vector of business days
#' @export
business_days <- function(from, to) {
  all_days  <- seq(as.Date(from), as.Date(to), by = "day")
  wday_idx  <- lubridate::wday(all_days, week_start = 1)
  all_days[wday_idx <= 5]
}

#' Annualisation factor for a given return frequency
#' @param frequency "daily" | "weekly" | "monthly" | "quarterly"
#' @return Numeric scalar (252, 52, 12, or 4)
#' @export
annualisation_factor <- function(frequency = "daily") {
  switch(frequency,
    daily     = 252L,
    weekly    = 52L,
    monthly   = 12L,
    quarterly = 4L,
    stop(glue("Unknown frequency: {frequency}"))
  )
}

# ---- Financial Math ----------------------------------------------------------

#' Compute log returns from a price series
#' @param prices Numeric vector of prices
#' @return Numeric vector of log returns (length = length(prices) - 1)
#' @export
log_returns <- function(prices) {
  diff(log(prices))
}

#' Compute simple returns from a price series
#' @param prices Numeric vector of prices
#' @export
simple_returns <- function(prices) {
  (prices[-1] - prices[-length(prices)]) / prices[-length(prices)]
}

#' Cumulative return from a return series
#' @param r Numeric vector of period returns
#' @export
cum_return <- function(r) {
  prod(1 + r) - 1
}

#' Annualise a period return
#' @param r        Period return (scalar)
#' @param n_periods Number of periods held
#' @param ann_factor Annualisation factor (e.g. 252 for daily)
#' @export
annualise_return <- function(r, n_periods, ann_factor = 252) {
  (1 + r)^(ann_factor / n_periods) - 1
}

#' Drawdown series from a cumulative return index
#'
#' Drawdown at time t = (peak up to t - value at t) / peak up to t
#'
#' @param prices Numeric vector (price or NAV)
#' @return Numeric vector of drawdowns (negative values)
#' @export
drawdown_series <- function(prices) {
  peak <- cummax(prices)
  (prices - peak) / peak
}

#' Maximum drawdown scalar
#' @param prices Numeric vector (price or NAV)
#' @export
max_drawdown <- function(prices) {
  min(drawdown_series(prices))
}

#' Calmar ratio: annualised return / |max drawdown|
#' @param prices   Numeric price vector
#' @param ann_factor Annualisation factor
#' @export
calmar_ratio <- function(prices, ann_factor = 252) {
  r   <- log_returns(prices)
  ann <- mean(r) * ann_factor
  mdd <- abs(max_drawdown(prices))
  if (mdd == 0) return(Inf)
  ann / mdd
}

# ---- Statistical Utilities ---------------------------------------------------

#' Winsorise a numeric vector at given quantile tails
#' @param x   Numeric vector
#' @param low Lower quantile (default 0.01)
#' @param high Upper quantile (default 0.99)
#' @export
winsorise <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, c(low, high), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

#' Check if a numeric vector is approximately stationary via ADF
#' Returns TRUE if stationary at given significance level.
#' @param x   Numeric vector
#' @param sig Significance level (default 0.05)
#' @export
is_stationary <- function(x, sig = 0.05) {
  requireNamespace("tseries", quietly = TRUE)
  tryCatch({
    p <- tseries::adf.test(na.omit(x))$p.value
    p < sig
  }, error = function(e) NA)
}

#' Rolling function over a numeric vector using base R (no zoo dependency)
#' @param x      Numeric vector
#' @param width  Window width
#' @param FUN    Function to apply
#' @param ...    Additional arguments to FUN
#' @export
roll_apply <- function(x, width, FUN, ...) {
  n   <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq(width, n)) {
    out[i] <- FUN(x[(i - width + 1):i], ...)
  }
  out
}

# ---- IO Utilities ------------------------------------------------------------

#' Safely read a CSV file with informative error reporting
#' @param path    File path
#' @param ...     Arguments forwarded to readr::read_csv
#' @export
safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) {
    stop(glue("[io] File not found: {path}"))
  }
  readr::read_csv(path, show_col_types = FALSE, ...)
}

#' Persist a data frame with a timestamped filename
#' @param df      Data frame
#' @param dir     Output directory
#' @param prefix  Filename prefix
#' @param format  "csv" or "rds"
#' @export
save_artifact <- function(df, dir, prefix, format = "csv") {
  ts   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  fname <- file.path(dir, glue("{prefix}_{ts}.{format}"))
  if (format == "csv") {
    readr::write_csv(df, fname)
  } else {
    saveRDS(df, fname)
  }
  invisible(fname)
}
