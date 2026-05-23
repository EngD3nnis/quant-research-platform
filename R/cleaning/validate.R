# =============================================================================
# Data Validation & Quality Assurance Module
# Implements a schema-based validation framework for financial time series.
# Every dataset entering the analytical pipeline must pass these checks.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(glue)
  library(purrr)
})

source(here::here("R", "utilities", "logger.R"))

# ---- Validation Result Object ------------------------------------------------

#' Construct a validation result object
#' @param passed  Logical scalar
#' @param checks  Named list of individual check results
#' @param summary Character string summarising outcome
#' @return S3 object of class "ValidationResult"
new_validation_result <- function(passed, checks, summary) {
  structure(
    list(passed = passed, checks = checks, summary = summary),
    class = "ValidationResult"
  )
}

#' @export
print.ValidationResult <- function(x, ...) {
  status <- if (x$passed) "PASSED" else "FAILED"
  cat(glue("Validation [{status}]: {x$summary}\n\n"))
  for (nm in names(x$checks)) {
    icon <- if (isTRUE(x$checks[[nm]]$ok)) "[OK]" else "[FAIL]"
    cat(glue("  {icon} {nm}: {x$checks[[nm]]$message}\n"))
  }
  invisible(x)
}

# ---- Individual Check Functions ----------------------------------------------

#' Check that required columns are present
check_required_cols <- function(df, required_cols) {
  missing <- setdiff(required_cols, names(df))
  list(
    ok      = length(missing) == 0,
    message = if (length(missing) == 0)
                glue("All {length(required_cols)} required columns present")
              else
                glue("Missing columns: {paste(missing, collapse = ', ')}")
  )
}

#' Check for excessive NA values
check_na_rate <- function(df, max_na_pct = 0.05) {
  na_rates <- colMeans(is.na(df))
  bad      <- na_rates[na_rates > max_na_pct]
  list(
    ok      = length(bad) == 0,
    message = if (length(bad) == 0)
                glue("NA rate < {max_na_pct * 100}% in all columns")
              else
                glue("High NA rate in: {paste(names(bad), round(bad * 100, 1), sep = '=', collapse = ', ')}%")
  )
}

#' Check date column is valid and monotonically increasing
check_date_integrity <- function(df, date_col = "date") {
  if (!date_col %in% names(df)) {
    return(list(ok = FALSE, message = glue("Date column '{date_col}' not found")))
  }
  dates   <- df[[date_col]]
  is_date <- inherits(dates, "Date") || inherits(dates, "POSIXct")
  sorted  <- !is.unsorted(dates, na.rm = TRUE)
  dups    <- sum(duplicated(dates))

  if (!is_date) return(list(ok = FALSE, message = "Date column is not Date/POSIXct"))
  if (!sorted)  return(list(ok = FALSE, message = "Dates are not monotonically increasing"))
  if (dups > 0) return(list(ok = FALSE, message = glue("{dups} duplicate dates detected")))

  list(ok = TRUE, message = glue("Dates valid: {min(dates)} → {max(dates)}, {nrow(df)} rows"))
}

#' Check numeric columns are within physically plausible bounds
check_numeric_bounds <- function(df, numeric_cols, lower = -Inf, upper = Inf) {
  issues <- character(0)
  for (col in intersect(numeric_cols, names(df))) {
    vals <- df[[col]]
    n_low  <- sum(vals < lower,  na.rm = TRUE)
    n_high <- sum(vals > upper,  na.rm = TRUE)
    if (n_low + n_high > 0) {
      issues <- c(issues, glue("{col}: {n_low} below {lower}, {n_high} above {upper}"))
    }
  }
  list(
    ok      = length(issues) == 0,
    message = if (length(issues) == 0)
                "All numeric values within bounds"
              else
                paste(issues, collapse = "; ")
  )
}

#' Check price columns satisfy OHLC logic: High >= max(Open, Close), Low <= min(Open, Close)
check_ohlc_logic <- function(df) {
  required <- c("open", "high", "low", "close")
  if (!all(required %in% names(df))) {
    return(list(ok = NA, message = "OHLC columns not all present — skipped"))
  }
  bad_high <- sum(df$high < pmax(df$open, df$close), na.rm = TRUE)
  bad_low  <- sum(df$low  > pmin(df$open, df$close), na.rm = TRUE)
  list(
    ok      = (bad_high + bad_low) == 0,
    message = if ((bad_high + bad_low) == 0)
                "OHLC logic valid"
              else
                glue("{bad_high} High violations, {bad_low} Low violations")
  )
}

#' Check that the time series has sufficient length
check_min_length <- function(df, min_rows = 30) {
  list(
    ok      = nrow(df) >= min_rows,
    message = glue("{nrow(df)} rows (minimum required: {min_rows})")
  )
}

# ---- Composite Validator -----------------------------------------------------

#' Run the full validation suite on a financial time series data frame
#'
#' @param df            Data frame to validate
#' @param required_cols Required column names
#' @param date_col      Name of date column
#' @param numeric_cols  Numeric columns to range-check
#' @param max_na_pct    Maximum allowed NA proportion per column
#' @param min_rows      Minimum acceptable row count
#' @param price_lower   Lower bound for price columns (default 0)
#' @return ValidationResult S3 object
#' @export
validate_dataset <- function(df,
                              required_cols = c("date", "close"),
                              date_col      = "date",
                              numeric_cols  = c("open", "high", "low", "close", "volume"),
                              max_na_pct    = 0.05,
                              min_rows      = 30L,
                              price_lower   = 0) {
  checks <- list(
    required_columns = check_required_cols(df, required_cols),
    date_integrity   = check_date_integrity(df, date_col),
    na_rate          = check_na_rate(df, max_na_pct),
    numeric_bounds   = check_numeric_bounds(df, numeric_cols, lower = price_lower),
    ohlc_logic       = check_ohlc_logic(df),
    min_length       = check_min_length(df, min_rows)
  )

  passed  <- all(purrr::map_lgl(checks, ~ isTRUE(.x$ok) || is.na(.x$ok)))
  n_fail  <- sum(purrr::map_lgl(checks, ~ isFALSE(.x$ok)))
  summary <- glue("{n_fail}/{length(checks)} checks failed")

  result  <- new_validation_result(passed, checks, summary)

  if (!passed) {
    log_warn("[validate] Dataset failed validation: {summary}")
  } else {
    log_info("[validate] Dataset passed all checks")
  }

  result
}

#' Validate and abort if any checks failed (strict mode)
#'
#' @param df   Data frame
#' @param ...  Arguments forwarded to validate_dataset()
#' @return df invisibly (pass-through on success)
#' @export
assert_valid <- function(df, ...) {
  result <- validate_dataset(df, ...)
  if (!result$passed) {
    print(result)
    stop("[validate] Strict validation failed — pipeline halted")
  }
  invisible(df)
}
