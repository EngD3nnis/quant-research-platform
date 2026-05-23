# =============================================================================
# Unit Tests — R/econometrics/time_series.R & regression.R
# =============================================================================

library(testthat)
library(here)

source(here::here("R", "utilities", "helpers.R"))
source(here::here("R", "econometrics", "time_series.R"))
source(here::here("R", "econometrics", "regression.R"))

# ---- ADF Test ---------------------------------------------------------------

test_that("adf_test identifies stationary white noise as stationary", {
  set.seed(42)
  x   <- rnorm(500)
  res <- adf_test(x)
  expect_true(res$stationary)
  expect_lt(res$p_value, 0.05)
})

test_that("adf_test identifies random walk as non-stationary", {
  set.seed(7)
  x   <- cumsum(rnorm(500))
  res <- adf_test(x)
  expect_false(res$stationary)
  expect_gt(res$p_value, 0.05)
})

# ---- OLS Manual -------------------------------------------------------------

test_that("ols_manual recovers known coefficients", {
  set.seed(42)
  n    <- 500
  x1   <- rnorm(n)
  x2   <- rnorm(n)
  y    <- 3 + 1.5 * x1 - 0.8 * x2 + rnorm(n, 0, 0.5)

  res  <- ols_manual(y, cbind(x1, x2), var_names = c("x1","x2"))
  coefs <- setNames(res$coefficients$estimate, res$coefficients$term)

  expect_equal(coefs["(Intercept)"], 3,    tolerance = 0.1)
  expect_equal(coefs["x1"],          1.5,  tolerance = 0.1)
  expect_equal(coefs["x2"],         -0.8,  tolerance = 0.1)
})

test_that("ols_manual R-squared is in [0, 1]", {
  set.seed(1)
  y   <- rnorm(200)
  x   <- matrix(rnorm(200 * 3), 200, 3)
  res <- ols_manual(y, x)
  expect_gte(res$r_squared, 0)
  expect_lte(res$r_squared, 1)
})

test_that("ols_manual R-squared matches lm() for same data", {
  set.seed(5)
  y      <- rnorm(300)
  x1     <- rnorm(300)
  x2     <- rnorm(300)
  manual <- ols_manual(y, cbind(x1, x2))
  lm_fit <- lm(y ~ x1 + x2)

  expect_equal(manual$r_squared, summary(lm_fit)$r.squared, tolerance = 1e-6)
})

test_that("ols_manual t-statistics match lm() coefficient table", {
  set.seed(9)
  y      <- rnorm(200)
  x1     <- rnorm(200)
  manual <- ols_manual(y, matrix(x1, ncol = 1), var_names = "x1")
  lm_fit <- lm(y ~ x1)

  lm_t   <- coef(summary(lm_fit))[, "t value"]
  my_t   <- setNames(manual$coefficients$t_stat, manual$coefficients$term)

  expect_equal(my_t["(Intercept)"], lm_t["(Intercept)"], tolerance = 1e-4)
  expect_equal(my_t["x1"],          lm_t["x1"],          tolerance = 1e-4)
})

# ---- ARIMA ------------------------------------------------------------------

test_that("fit_arima returns valid structure", {
  set.seed(42)
  x   <- arima.sim(n = 300, list(ar = 0.7, ma = 0.3))
  res <- fit_arima(as.numeric(x), max_p = 3, max_q = 3)

  expect_false(is.null(res))
  expect_true(is.list(res))
  expect_true("model" %in% names(res))
  expect_true("order" %in% names(res))
})

test_that("forecast_arima returns correct horizon length", {
  set.seed(1)
  x   <- arima.sim(n = 200, list(ar = 0.5))
  fit <- fit_arima(as.numeric(x))
  fc  <- forecast_arima(fit, horizon = 20)

  expect_equal(nrow(fc), 20)
  expect_equal(fc$h, 1:20)
})

test_that("forecast_arima upper_95 > lower_95", {
  set.seed(2)
  x   <- arima.sim(n = 200, list(ar = 0.5))
  fit <- fit_arima(as.numeric(x))
  fc  <- forecast_arima(fit, horizon = 10)

  expect_true(all(fc$hi_95 > fc$lo_95))
})

# ---- Forecast Accuracy ------------------------------------------------------

test_that("forecast_accuracy RMSE >= MAE (by Cauchy-Schwarz)", {
  set.seed(42)
  actual    <- rnorm(100)
  predicted <- actual + rnorm(100, 0, 0.5)
  acc       <- forecast_accuracy(actual, predicted)
  expect_gte(acc["RMSE"], acc["MAE"])
})

test_that("forecast_accuracy returns zero for perfect forecast", {
  x   <- rnorm(100)
  acc <- forecast_accuracy(x, x)
  expect_equal(acc["MAE"],  0, tolerance = 1e-10)
  expect_equal(acc["RMSE"], 0, tolerance = 1e-10)
})
