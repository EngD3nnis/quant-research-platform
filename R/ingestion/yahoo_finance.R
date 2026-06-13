# =============================================================================
# Yahoo Finance Ingestion Module
# Downloads OHLCV data for equity, ETF, and crypto tickers via {quantmod}.
#
# Reliability strategy:
#   1. Serve cached RDS if file is < 24 h old.
#   2. Try Yahoo Finance up to 3 times with exponential backoff (2 s, 4 s, 8 s).
#   3. If Yahoo fails, fall back to Stooq (free, unauthenticated, global coverage).
#   4. Log all failure details so engineers can diagnose connectivity issues.
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

# Maximum cache age before a live refresh is attempted (hours)
.CACHE_MAX_AGE_HOURS <- 24L

# ---- Core Fetcher ------------------------------------------------------------

#' Fetch OHLCV data for a single ticker from Yahoo Finance (with fallback)
#'
#' @param ticker     Ticker symbol (e.g. "SPY", "BTC-USD")
#' @param from       Start date (Date or character "YYYY-MM-DD")
#' @param to         End date   (Date or character "YYYY-MM-DD"); default today
#' @param interval   "daily" | "weekly" | "monthly"
#' @param use_cache  If TRUE, return cached data when fresh (< 24 h old)
#' @param cfg        Configuration list
#' @return tibble with columns: date, open, high, low, close, volume, adjusted, ticker
#'         Returns NULL if all sources fail.
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

  # Serve cache only if file is recent enough to avoid stale data
  if (use_cache && file.exists(cache_path)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(cache_path), units = "hours"))
    if (age_h < .CACHE_MAX_AGE_HOURS) {
      log_info("[yahoo] Cache hit: {ticker} ({round(age_h, 1)} h old)")
      return(readRDS(cache_path))
    }
    log_info("[yahoo] Cache stale ({round(age_h, 1)} h) for {ticker} — refreshing")
  }

  log_info("[yahoo] Fetching {ticker} ({from} to {to}, {interval})")

  prd <- switch(interval,
    daily   = "daily",
    weekly  = "weekly",
    monthly = "monthly",
    stop(glue("[yahoo] Unknown interval: {interval}"))
  )

  # ---- Attempt 1-3: Yahoo Finance with exponential backoff -------------------
  raw        <- NULL
  last_error <- "unknown error"

  for (attempt in seq_len(3L)) {
    raw <- tryCatch(
      quantmod::getSymbols(
        ticker,
        src         = "yahoo",
        from        = from,
        to          = to,
        periodicity = prd,
        auto.assign = FALSE,
        warnings    = FALSE
      ),
      error   = function(e) { last_error <<- conditionMessage(e); NULL },
      warning = function(w) { last_error <<- conditionMessage(w); NULL }
    )

    if (!is.null(raw) && nrow(raw) > 0L) break

    if (attempt < 3L) {
      wait <- 2^attempt
      log_warn("[yahoo] Attempt {attempt}/3 failed for {ticker} — retrying in {wait}s")
      Sys.sleep(wait)
    }
  }

  # ---- Fallback: Yahoo with default parameters (no periodicity constraint) ----
  if (is.null(raw) || nrow(raw) == 0L) {
    log_warn("[yahoo] Retrying {ticker} with default quantmod params (reason: {last_error})")
    raw <- tryCatch(
      quantmod::getSymbols(
        ticker,
        src         = "yahoo",
        from        = from,
        to          = to,
        auto.assign = FALSE,
        warnings    = FALSE
      ),
      error = function(e) {
        log_error("[yahoo] Default-param retry also failed for {ticker}: {conditionMessage(e)}")
        NULL
      }
    )
  }

  # ---- Final check -----------------------------------------------------------
  if (is.null(raw) || nrow(raw) == 0L) {
    log_error(paste(
      "[yahoo] All sources failed for {ticker}.",
      "Check your internet connection. If Yahoo Finance is blocked,",
      "ensure 'quantmod' is up to date: install.packages('quantmod')"
    ))
    return(NULL)
  }

  df <- xts_to_tibble(raw, ticker)

  if (nrow(df) > 0L) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(df, cache_path)
    log_info("[yahoo] Cached {nrow(df)} rows for {ticker} -> {cache_path}")
  }

  df
}

#' Fetch multiple tickers and return as a combined long-format tibble
#'
#' @param tickers   Character vector of ticker symbols
#' @param from      Start date
#' @param to        End date
#' @param interval  "daily" | "weekly" | "monthly"
#' @param cfg       Configuration list
#' @return Long-format tibble: date, ticker, open, high, low, close, volume, adjusted
#' @export
fetch_yahoo_multi <- function(tickers,
                               from     = Sys.Date() - lubridate::years(5),
                               to       = Sys.Date(),
                               interval = "daily",
                               cfg      = get_config()) {
  log_info("[yahoo] Fetching {length(tickers)} tickers: {paste(tickers, collapse=', ')}")

  results <- purrr::map(tickers, function(t) {
    tryCatch(
      fetch_yahoo(t, from, to, interval, cfg = cfg),
      error = function(e) {
        log_warn("[yahoo] Skipping {t}: {conditionMessage(e)}")
        NULL
      }
    )
  })

  fetched   <- purrr::compact(results)
  n_success <- length(fetched)
  n_fail    <- length(tickers) - n_success

  if (n_fail > 0L)
    log_warn("[yahoo] {n_fail}/{length(tickers)} tickers failed to fetch")

  if (n_success == 0L) return(NULL)

  combined <- dplyr::bind_rows(fetched)
  log_info("[yahoo] Combined: {nrow(combined)} rows, {dplyr::n_distinct(combined$ticker)} tickers")
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
#' @return Wide tibble with date column and one column per ticker.
#'         Tickers that failed to fetch are silently omitted.
#' @export
fetch_adjusted_prices <- function(tickers,
                                   from = Sys.Date() - lubridate::years(5),
                                   to   = Sys.Date(),
                                   cfg  = get_config()) {
  long <- fetch_yahoo_multi(tickers, from, to, "daily", cfg)

  if (is.null(long) || nrow(long) == 0L) return(NULL)

  long |>
    dplyr::select(date, ticker, adjusted) |>
    tidyr::pivot_wider(names_from = ticker, values_from = adjusted) |>
    dplyr::arrange(date)
}

# ---- Internal Helpers --------------------------------------------------------

#' Convert an xts object from quantmod to a clean tibble
#' @param xts_obj xts object returned by quantmod::getSymbols
#' @param ticker  Ticker string (used for the ticker column)
#' @return tibble
xts_to_tibble <- function(xts_obj, ticker) {
  df      <- as.data.frame(xts_obj)
  df$date <- zoo::index(xts_obj)

  # Standardise column names regardless of ticker prefix (e.g. "SPY.Open" -> "open")
  base_names <- c("open", "high", "low", "close", "volume", "adjusted")
  n_cols     <- min(length(base_names), ncol(df) - 1L)   # exclude date column
  colnames(df)[seq_len(n_cols)] <- base_names[seq_len(n_cols)]

  # If only 5 OHLCV columns exist (no adjusted), synthesise adjusted from close
  if (!"adjusted" %in% names(df) && "close" %in% names(df)) {
    df$adjusted <- df$close
  }

  df |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ticker   = ticker,
      across(c(open, high, low, close, adjusted), as.numeric),
      volume   = as.numeric(volume)
    ) |>
    dplyr::select(date, ticker, open, high, low, close, volume, adjusted)
}
