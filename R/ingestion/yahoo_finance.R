# =============================================================================
# Yahoo Finance Ingestion Module
# Downloads OHLCV data for equity, ETF, and crypto tickers via {quantmod}.
# All raw downloads are cached to data/raw/ with metadata sidecar files.
# =============================================================================

suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(glue)
  library(purrr)
})

source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "helpers.R"))
source(here::here("R", "utilities", "config.R"))

# ---- Core Fetcher ------------------------------------------------------------

#' Fetch OHLCV data for a single ticker from Yahoo Finance
#'
#' @param ticker     Ticker symbol (e.g. "SPY", "BTC-USD")
#' @param from       Start date (Date or character "YYYY-MM-DD")
#' @param to         End date   (Date or character "YYYY-MM-DD"); default today
#' @param interval   "daily" | "weekly" | "monthly"
#' @param use_cache  If TRUE, return cached data when available
#' @param cfg        Configuration list
#' @return tibble with columns: date, open, high, low, close, volume, ticker
#' @export
fetch_yahoo <- function(ticker,
                        from      = Sys.Date() - lubridate::years(5),
                        to        = Sys.Date(),
                        interval  = "daily",
                        use_cache = TRUE,
                        cfg       = get_config()) {

  from <- as.Date(from)
  to   <- as.Date(to)

  cache_path <- file.path(
    cfg$paths$data_raw,
    glue("{ticker}_{from}_{to}_{interval}.rds")
  )

  if (use_cache && file.exists(cache_path)) {
    log_info("[yahoo] Cache hit for {ticker}")
    return(readRDS(cache_path))
  }

  log_info("[yahoo] Fetching {ticker} ({from} → {to}, {interval})")

  prd <- switch(interval,
    daily   = "days",
    weekly  = "weeks",
    monthly = "months",
    stop(glue("[yahoo] Unknown interval: {interval}"))
  )

  raw <- tryCatch(
    quantmod::getSymbols(
      ticker,
      src        = "yahoo",
      from       = from,
      to         = to,
      periodicity = prd,
      auto.assign = FALSE
    ),
    error = function(e) {
      log_error("[yahoo] Failed to fetch {ticker}: {conditionMessage(e)}")
      return(NULL)
    }
  )

  if (is.null(raw)) return(NULL)

  df <- xts_to_tibble(raw, ticker)

  saveRDS(df, cache_path)
  log_info("[yahoo] Saved {nrow(df)} rows → {cache_path}")
  df
}

#' Fetch multiple tickers and return as a combined long-format tibble
#'
#' @param tickers   Character vector of ticker symbols
#' @param from      Start date
#' @param to        End date
#' @param interval  "daily" | "weekly" | "monthly"
#' @param cfg       Configuration list
#' @return Long-format tibble with columns: date, ticker, open, high, low, close, volume
#' @export
fetch_yahoo_multi <- function(tickers,
                               from     = Sys.Date() - lubridate::years(5),
                               to       = Sys.Date(),
                               interval = "daily",
                               cfg      = get_config()) {
  log_info("[yahoo] Fetching {length(tickers)} tickers")

  results <- purrr::map(tickers, function(t) {
    df <- tryCatch(
      fetch_yahoo(t, from, to, interval, cfg = cfg),
      error = function(e) {
        log_warn("[yahoo] Skipping {t}: {conditionMessage(e)}")
        NULL
      }
    )
    df
  })

  combined <- dplyr::bind_rows(purrr::compact(results))
  log_info("[yahoo] Combined dataset: {nrow(combined)} rows, {dplyr::n_distinct(combined$ticker)} tickers")
  combined
}

#' Fetch adjusted closing prices only (wide format — one column per ticker)
#'
#' Adjusted prices account for dividends and splits — the correct input
#' for return calculations.
#'
#' @param tickers   Character vector
#' @param from      Start date
#' @param to        End date
#' @param cfg       Configuration list
#' @return Wide tibble with date column and one column per ticker
#' @export
fetch_adjusted_prices <- function(tickers,
                                   from = Sys.Date() - lubridate::years(5),
                                   to   = Sys.Date(),
                                   cfg  = get_config()) {
  long <- fetch_yahoo_multi(tickers, from, to, "daily", cfg)

  long |>
    dplyr::select(date, ticker, adjusted) |>
    tidyr::pivot_wider(names_from = ticker, values_from = adjusted) |>
    dplyr::arrange(date)
}

# ---- Internal Helpers --------------------------------------------------------

#' Convert an xts object from quantmod to a clean tibble
#' @param xts_obj xts object returned by quantmod::getSymbols
#' @param ticker  Ticker string (used for column naming)
#' @return tibble
xts_to_tibble <- function(xts_obj, ticker) {
  df        <- as.data.frame(xts_obj)
  df$date   <- zoo::index(xts_obj)

  # Standardise column names regardless of ticker prefix
  base_names <- c("open", "high", "low", "close", "volume", "adjusted")
  colnames(df)[seq_along(base_names)] <- base_names

  df |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ticker = ticker,
      across(c(open, high, low, close, adjusted), as.numeric),
      volume = as.numeric(volume)
    ) |>
    dplyr::select(date, ticker, everything())
}
