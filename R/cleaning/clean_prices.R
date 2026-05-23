# =============================================================================
# Price Data Cleaning Module
# Handles: outlier detection, gap filling, return winsorisation,
# corporate action adjustment, and cross-asset alignment.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(glue)
})

source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "helpers.R"))

# ---- Outlier Detection -------------------------------------------------------

#' Flag price return outliers using the modified Z-score (Iglewicz & Hoaglin)
#'
#' More robust than standard Z-score because it uses the median absolute
#' deviation (MAD) rather than mean/sd — crucial for fat-tailed return data.
#'
#' Threshold of 3.5 recommended by Iglewicz & Hoaglin (1993).
#'
#' @param returns   Numeric vector of returns
#' @param threshold Modified Z-score threshold (default 3.5)
#' @return Logical vector — TRUE where outlier detected
#' @export
flag_return_outliers <- function(returns, threshold = 3.5) {
  med  <- median(returns, na.rm = TRUE)
  mad  <- median(abs(returns - med), na.rm = TRUE)
  if (mad == 0) return(rep(FALSE, length(returns)))
  mz   <- 0.6745 * abs(returns - med) / mad
  mz > threshold
}

#' Remove or winsorise price outliers
#'
#' @param df         tibble with date and price columns
#' @param price_col  Name of the price column to clean
#' @param method     "winsorise" clips extremes; "remove" sets to NA
#' @param threshold  Modified Z-score threshold
#' @return Cleaned tibble
#' @export
clean_price_outliers <- function(df, price_col = "close",
                                  method = "winsorise", threshold = 3.5) {
  r    <- log_returns(df[[price_col]])
  flag <- c(FALSE, flag_return_outliers(r, threshold))
  n    <- sum(flag, na.rm = TRUE)

  if (n > 0) log_warn("[clean] {n} outliers detected in {price_col}")

  if (method == "remove") {
    df[[price_col]][flag] <- NA_real_
  } else {
    q  <- quantile(df[[price_col]], c(0.01, 0.99), na.rm = TRUE)
    df[[price_col]] <- pmin(pmax(df[[price_col]], q[1]), q[2])
  }

  df
}

# ---- Gap Filling -------------------------------------------------------------

#' Fill missing prices using last observation carried forward (LOCF)
#'
#' LOCF is the standard convention for financial prices because on non-trading
#' days the last available price is the best estimate of fair value.
#'
#' @param df        tibble with date and price columns
#' @param max_gap   Maximum consecutive NAs to fill (longer gaps remain NA)
#' @return Tibble with NAs filled
#' @export
fill_price_gaps <- function(df, max_gap = 5L) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]

  df <- df |>
    dplyr::arrange(date) |>
    dplyr::mutate(across(
      all_of(numeric_cols),
      ~ zoo::na.locf(.x, maxgap = max_gap, na.rm = FALSE)
    ))

  remaining_na <- sum(is.na(df[numeric_cols]))
  if (remaining_na > 0) {
    log_warn("[clean] {remaining_na} NAs remain after LOCF gap-fill (gap > {max_gap})")
  }

  df
}

# ---- Cross-Asset Alignment ---------------------------------------------------

#' Align multiple price series to a common date grid
#'
#' Merges all tickers onto the union of their date ranges, filling gaps via LOCF.
#' Ensures the analysis always uses a consistent, balanced panel.
#'
#' @param df_wide  Wide-format tibble: date column + one column per ticker
#' @param method   "inner" keeps only shared dates; "outer" keeps all dates
#' @return Aligned wide-format tibble
#' @export
align_price_panel <- function(df_wide, method = "outer") {
  if (method == "inner") {
    df_wide <- df_wide |>
      dplyr::filter(dplyr::if_all(where(is.numeric), ~ !is.na(.x)))
  } else {
    # Forward-fill across all columns
    num_cols <- names(df_wide)[sapply(df_wide, is.numeric)]
    df_wide <- df_wide |>
      dplyr::arrange(date) |>
      dplyr::mutate(across(
        all_of(num_cols),
        ~ zoo::na.locf(.x, na.rm = FALSE)
      ))
  }

  # Drop rows where ALL price columns are still NA
  num_cols <- names(df_wide)[sapply(df_wide, is.numeric)]
  df_wide  <- df_wide |>
    dplyr::filter(!dplyr::if_all(all_of(num_cols), is.na))

  log_info("[clean] Aligned panel: {nrow(df_wide)} dates, {length(num_cols)} tickers")
  df_wide
}

# ---- Return Computation Pipeline ---------------------------------------------

#' Clean raw prices and compute log/simple returns
#'
#' Combines outlier cleaning, gap-filling, and return computation into
#' a single reproducible pipeline step.
#'
#' @param df_long    Long-format tibble: date, ticker, adjusted (or close)
#' @param price_col  Price column to use for returns
#' @param ret_type   "log" or "simple"
#' @param freq       Return frequency passed to annualisation_factor()
#' @return Long-format tibble: date, ticker, price, return
#' @export
compute_returns_pipeline <- function(df_long,
                                      price_col = "adjusted",
                                      ret_type  = "log",
                                      freq      = "daily") {
  ret_fn <- switch(ret_type,
    log    = log_returns,
    simple = simple_returns,
    stop(glue("[clean] Unknown return type: {ret_type}"))
  )

  df_long |>
    dplyr::arrange(ticker, date) |>
    dplyr::group_by(ticker) |>
    dplyr::mutate(
      price  = .data[[price_col]],
      return = c(NA_real_, ret_fn(price))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(return)) |>
    dplyr::select(date, ticker, price, return)
}
