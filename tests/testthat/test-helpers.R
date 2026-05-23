# =============================================================================
# Unit Tests — R/utilities/helpers.R
# Validates core financial math functions from first principles.
# =============================================================================

library(testthat)
library(here)

source(here::here("R", "utilities", "helpers.R"))

# ---- log_returns() ----------------------------------------------------------

test_that("log_returns computes correct values", {
  prices <- c(100, 110, 121, 133.1)
  r      <- log_returns(prices)

  expect_length(r, 3)
  expect_equal(r[1], log(110 / 100), tolerance = 1e-10)
  expect_equal(r[2], log(121 / 110), tolerance = 1e-10)
  expect_equal(sum(r), log(133.1 / 100), tolerance = 1e-10)
})

test_that("log_returns is time-additive", {
  prices <- c(50, 75, 60, 90)
  r      <- log_returns(prices)
  expect_equal(sum(r), log(90 / 50), tolerance = 1e-10)
})

# ---- simple_returns() -------------------------------------------------------

test_that("simple_returns computes correct values", {
  prices <- c(100, 110, 99)
  r      <- simple_returns(prices)

  expect_length(r, 2)
  expect_equal(r[1],  0.10,  tolerance = 1e-10)
  expect_equal(r[2], -0.10, tolerance = 1e-10)
})

# ---- drawdown_series() ------------------------------------------------------

test_that("drawdown_series is non-positive", {
  prices <- c(100, 110, 105, 120, 90, 115)
  dd     <- drawdown_series(prices)
  expect_true(all(dd <= 1e-10))
})

test_that("drawdown_series is zero at peak", {
  prices <- c(100, 110, 120, 130)
  dd     <- drawdown_series(prices)
  # Monotonically increasing series — drawdown always 0
  expect_equal(max(abs(dd)), 0, tolerance = 1e-10)
})

test_that("max_drawdown returns correct value", {
  prices <- c(100, 80, 60, 70)   # max drawdown = (100-60)/100 = -0.40
  mdd    <- max_drawdown(prices)
  expect_equal(mdd, -0.40, tolerance = 1e-10)
})

# ---- annualisation_factor() -------------------------------------------------

test_that("annualisation_factor returns correct values", {
  expect_equal(annualisation_factor("daily"),     252L)
  expect_equal(annualisation_factor("weekly"),    52L)
  expect_equal(annualisation_factor("monthly"),   12L)
  expect_equal(annualisation_factor("quarterly"), 4L)
  expect_error(annualisation_factor("hourly"))
})

# ---- winsorise() ------------------------------------------------------------

test_that("winsorise clips extremes correctly", {
  set.seed(42)
  x   <- c(-100, rnorm(98), 100)
  w   <- winsorise(x, 0.01, 0.99)
  expect_true(min(w) >= quantile(x, 0.01))
  expect_true(max(w) <= quantile(x, 0.99))
  expect_length(w, length(x))
})

# ---- roll_apply() -----------------------------------------------------------

test_that("roll_apply computes rolling mean correctly", {
  x    <- c(1, 2, 3, 4, 5)
  rm   <- roll_apply(x, width = 3, FUN = mean)
  expect_equal(rm[3], 2.0)
  expect_equal(rm[5], 4.0)
  expect_true(is.na(rm[1]))
  expect_true(is.na(rm[2]))
})

test_that("roll_apply NA prefix has correct length", {
  x  <- 1:10
  rm <- roll_apply(x, width = 4, FUN = mean)
  expect_equal(sum(is.na(rm)), 3)   # first width-1 values are NA
})

# ---- cum_return() -----------------------------------------------------------

test_that("cum_return is zero for flat returns", {
  expect_equal(cum_return(rep(0, 100)), 0, tolerance = 1e-10)
})

test_that("cum_return matches compounding formula", {
  r   <- c(0.05, -0.03, 0.08)
  cr  <- cum_return(r)
  exp <- prod(1 + r) - 1
  expect_equal(cr, exp, tolerance = 1e-10)
})
