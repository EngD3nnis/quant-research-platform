# =============================================================================
# Unit Tests — R/simulations/monte_carlo.R
# Validates GBM simulation properties via statistical tests.
# =============================================================================

library(testthat)
library(here)

source(here::here("R", "utilities", "helpers.R"))
source(here::here("R", "simulations", "monte_carlo.R"))

# ---- simulate_gbm() ---------------------------------------------------------

test_that("simulate_gbm returns correctly dimensioned matrix", {
  paths <- simulate_gbm(S0 = 100, mu = 0.08, sigma = 0.20,
                         T_days = 252, n_paths = 1000, seed = 1)
  expect_equal(nrow(paths), 253)    # T+1 rows
  expect_equal(ncol(paths), 1000)
})

test_that("simulate_gbm starts at S0", {
  S0    <- 150
  paths <- simulate_gbm(S0, mu = 0.1, sigma = 0.2, T_days = 100, n_paths = 500)
  expect_true(all(paths[1, ] == S0))
})

test_that("simulate_gbm prices are strictly positive", {
  paths <- simulate_gbm(S0 = 100, mu = 0.05, sigma = 0.30,
                         T_days = 252, n_paths = 5000, seed = 42)
  expect_true(all(paths > 0))
})

test_that("simulate_gbm mean terminal price approximately matches theoretical", {
  # E[S_T] = S_0 × exp(μ × T)  under the risk-neutral measure
  S0    <- 100
  mu    <- 0.10
  sigma <- 0.25
  T     <- 252
  dt    <- 1 / 252

  paths    <- simulate_gbm(S0, mu, sigma, T_days = T, n_paths = 50000, seed = 7)
  terminal <- paths[T + 1, ]

  expected <- S0 * exp(mu * T * dt)   # theoretical E[S_T]
  empirical <- mean(terminal)

  # Allow 2% tolerance for Monte Carlo error
  expect_equal(empirical, expected, tolerance = expected * 0.02)
})

test_that("simulate_gbm is reproducible with same seed", {
  p1 <- simulate_gbm(100, 0.08, 0.20, 50, 100, seed = 123)
  p2 <- simulate_gbm(100, 0.08, 0.20, 50, 100, seed = 123)
  expect_equal(p1, p2)
})

test_that("simulate_gbm gives different results with different seeds", {
  p1 <- simulate_gbm(100, 0.08, 0.20, 50, 100, seed = 1)
  p2 <- simulate_gbm(100, 0.08, 0.20, 50, 100, seed = 2)
  expect_false(identical(p1, p2))
})

# ---- summarise_mc_paths() ---------------------------------------------------

test_that("summarise_mc_paths VaR is positive", {
  paths <- simulate_gbm(100, 0.08, 0.20, 252, 5000, seed = 42)
  s     <- summarise_mc_paths(paths, S0 = 100)
  expect_gt(s$var_95, 0)
  expect_gt(s$var_99, 0)
})

test_that("summarise_mc_paths ES > VaR", {
  paths <- simulate_gbm(100, 0.08, 0.20, 252, 10000, seed = 42)
  s     <- summarise_mc_paths(paths, S0 = 100)
  expect_gt(s$es_95, s$var_95)
  expect_gt(s$es_99, s$var_99)
})

test_that("summarise_mc_paths prob_loss is in [0, 1]", {
  paths <- simulate_gbm(100, 0.08, 0.20, 252, 5000, seed = 42)
  s     <- summarise_mc_paths(paths, S0 = 100)
  expect_gte(s$prob_loss, 0)
  expect_lte(s$prob_loss, 1)
})

# ---- simulate_historical_bootstrap() ----------------------------------------

test_that("historical bootstrap preserves initial price", {
  set.seed(42)
  returns <- rnorm(500, 0.0003, 0.01)
  paths   <- simulate_historical_bootstrap(returns, S0 = 100, T_days = 100,
                                            n_paths = 500)
  expect_true(all(paths[1, ] == 100))
})

test_that("historical bootstrap prices are positive", {
  set.seed(1)
  returns <- rnorm(500, 0.0003, 0.01)
  paths   <- simulate_historical_bootstrap(returns, S0 = 50, T_days = 252,
                                            n_paths = 1000)
  expect_true(all(paths > 0))
})
