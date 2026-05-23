# =============================================================================
# Unit Tests — R/transformations/portfolio_analytics.R
# Tests portfolio math, CAPM, and optimisation correctness.
# =============================================================================

library(testthat)
library(here)

source(here::here("R", "utilities", "helpers.R"))
source(here::here("R", "utilities", "config.R"))
source(here::here("R", "transformations", "portfolio_analytics.R"))

# ---- Fixtures ---------------------------------------------------------------

set.seed(42)
n     <- 500L
dates <- seq(as.Date("2020-01-02"), length.out = n, by = "day")

# Simulated returns: two correlated assets + benchmark
Sigma_true <- matrix(c(0.0004, 0.0002, 0.0002,
                        0.0002, 0.0006, 0.0003,
                        0.0002, 0.0003, 0.0003), 3, 3)
R_raw <- MASS::mvrnorm(n, mu = rep(0.0003, 3), Sigma = Sigma_true)

returns_wide <- tibble::tibble(
  date  = dates,
  SPY   = R_raw[, 1],
  AAPL  = R_raw[, 2],
  GLD   = R_raw[, 3]
)

prices_wide <- tibble::tibble(
  date = dates,
  SPY  = 100 * cumprod(1 + R_raw[, 1]),
  AAPL = 100 * cumprod(1 + R_raw[, 2]),
  GLD  = 100 * cumprod(1 + R_raw[, 3])
)

# ---- return_statistics() ----------------------------------------------------

test_that("return_statistics returns named vector with correct keys", {
  r   <- returns_wide$SPY
  px  <- prices_wide$SPY
  s   <- return_statistics(r, px)

  expected_keys <- c("n_obs","total_return","ann_return","ann_volatility",
                      "skewness","excess_kurtosis","sharpe_ratio","sortino_ratio",
                      "max_drawdown","calmar_ratio",
                      "var_95_daily","var_99_daily","es_95_daily","es_99_daily")
  expect_true(all(expected_keys %in% names(s)))
})

test_that("return_statistics annualised vol uses sqrt-of-time rule", {
  r      <- returns_wide$SPY
  sigma_d <- sd(r)
  s      <- return_statistics(r, ann_factor = 252)
  expect_equal(s["ann_volatility"], sigma_d * sqrt(252), tolerance = 1e-8)
})

test_that("VaR 99% > VaR 95% (larger loss at higher confidence)", {
  s <- return_statistics(returns_wide$SPY)
  expect_gt(s["var_99_daily"], s["var_95_daily"])
})

test_that("ES > VaR at same confidence level", {
  s <- return_statistics(returns_wide$SPY)
  expect_gt(s["es_95_daily"], s["var_95_daily"])
})

# ---- portfolio_statistics() -------------------------------------------------

test_that("portfolio_statistics returns correct structure", {
  weights <- c(SPY = 0.5, AAPL = 0.3, GLD = 0.2)
  result  <- portfolio_statistics(returns_wide, weights)

  expect_true(is.list(result))
  expect_named(result, c("returns","nav","dates","weights","cov_matrix","port_vol","stats"))
  expect_length(result$returns, n)
})

test_that("equal-weight portfolio weights sum to 1", {
  w <- setNames(rep(1/3, 3), c("SPY","AAPL","GLD"))
  expect_equal(sum(w), 1, tolerance = 1e-10)
})

test_that("portfolio_statistics NAV starts at 100", {
  w   <- c(SPY = 0.5, AAPL = 0.3, GLD = 0.2)
  res <- portfolio_statistics(returns_wide, w)
  expect_equal(res$nav[1], 100, tolerance = 0.001)
})

# ---- capm_regression() ------------------------------------------------------

test_that("capm_regression recovers known beta", {
  # Construct synthetic data with known beta = 1.5
  set.seed(99)
  n_obs       <- 1000
  mkt         <- rnorm(n_obs, 0, 0.01)
  asset       <- 1.5 * mkt + rnorm(n_obs, 0, 0.005)

  res         <- capm_regression(asset, mkt)

  expect_equal(res$beta, 1.5, tolerance = 0.05)
})

test_that("capm_regression alpha is approximately zero for pure beta", {
  set.seed(7)
  n_obs  <- 2000
  mkt    <- rnorm(n_obs, 0.0003, 0.01)
  asset  <- 1.0 * mkt + rnorm(n_obs, 0, 0.001)

  res    <- capm_regression(asset, mkt)
  # Alpha should not be significantly different from zero
  expect_lt(abs(res$t_alpha), 3.0)
})

test_that("capm_regression R-squared is in [0, 1]", {
  res <- capm_regression(returns_wide$AAPL, returns_wide$SPY)
  expect_gte(res$r_squared, 0)
  expect_lte(res$r_squared, 1)
})

# ---- min_variance_portfolio() -----------------------------------------------

test_that("min_variance_portfolio weights sum to 1", {
  Sigma <- cov(as.matrix(returns_wide[, c("SPY","AAPL","GLD")])) * 252
  w     <- min_variance_portfolio(Sigma)
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("min_variance_portfolio weights are non-negative (no short)", {
  Sigma <- cov(as.matrix(returns_wide[, c("SPY","AAPL","GLD")])) * 252
  w     <- min_variance_portfolio(Sigma, allow_short = FALSE)
  expect_true(all(w >= -1e-8))
})

# ---- efficient_frontier() ---------------------------------------------------

test_that("efficient_frontier returns increasing risk for higher return targets", {
  mu    <- colMeans(returns_wide[, c("SPY","AAPL","GLD")]) * 252
  Sigma <- cov(as.matrix(returns_wide[, c("SPY","AAPL","GLD")])) * 252
  ef    <- efficient_frontier(mu, Sigma, n_points = 30)

  # Remove failed QP solves
  ef <- ef[!is.na(ef$portfolio_vol), ]

  # Risk should be monotonically non-decreasing with target return
  vols_sorted <- sort(ef$portfolio_vol)
  expect_equal(ef$portfolio_vol, vols_sorted, tolerance = 1e-4)
})
