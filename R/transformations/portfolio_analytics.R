# =============================================================================
# Portfolio Analytics Engine
#
# Implements from first principles:
#   - Portfolio return & volatility (matrix algebra)
#   - Sharpe, Sortino, Calmar, Information ratios
#   - CAPM: alpha, beta, R², systematic vs idiosyncratic risk
#   - Factor analysis (Fama-French style)
#   - Drawdown & underwater analysis
#   - Rolling risk metrics
#   - Mean-Variance Optimisation (Markowitz, 1952)
#   - Maximum Sharpe Ratio portfolio
#   - Minimum Variance portfolio
#   - Efficient Frontier construction
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(glue)
  library(Matrix)
})

source(here::here("R", "utilities", "helpers.R"))
source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "config.R"))

# =============================================================================
# SECTION 1: RETURN & RISK STATISTICS
# =============================================================================

#' Compute complete return statistics for a return vector
#'
#' All statistics derived from first-principles formulae, not black-box functions.
#' Annualisation uses the compound-return convention, NOT simple scaling.
#'
#' @param r          Numeric vector of period returns
#' @param prices     Numeric price vector (for drawdown calculations)
#' @param ann_factor Annualisation factor (252 daily, 12 monthly, etc.)
#' @param rf         Risk-free rate (annualised, same units as ann_factor)
#' @return Named numeric vector of statistics
#' @export
return_statistics <- function(r, prices = NULL, ann_factor = 252, rf = 0.0525) {

  # -- Core return moments (sample estimators) ---------------------------------
  n        <- length(r)
  mu       <- mean(r, na.rm = TRUE)             # arithmetic mean
  sigma    <- sd(r, na.rm = TRUE)               # sample std dev
  skew     <- (sum((r - mu)^3, na.rm = TRUE) / n) / sigma^3  # Fisher skewness
  kurt     <- (sum((r - mu)^4, na.rm = TRUE) / n) / sigma^4  # excess kurtosis

  # -- Annualised return: geometric convention --------------------------------
  # μ_ann = (1 + μ)^ann_factor − 1   [exact compound return]
  mu_ann   <- (1 + mu)^ann_factor - 1

  # -- Annualised volatility ---------------------------------------------------
  # σ_ann = σ × √(ann_factor)   [square-root-of-time rule under i.i.d.]
  sig_ann  <- sigma * sqrt(ann_factor)

  # -- Risk-adjusted ratios ---------------------------------------------------
  # Sharpe: (μ_ann − rf) / σ_ann
  sharpe   <- (mu_ann - rf) / sig_ann

  # Sortino: uses downside deviation (semi-deviation below target rf/ann_factor)
  target   <- rf / ann_factor
  down_dev <- sqrt(mean(pmin(r - target, 0)^2, na.rm = TRUE)) * sqrt(ann_factor)
  sortino  <- if (down_dev > 0) (mu_ann - rf) / down_dev else NA_real_

  # -- Drawdown ---------------------------------------------------------------
  mdd      <- if (!is.null(prices)) max_drawdown(prices) else NA_real_
  calmar   <- if (!is.null(prices) && !is.na(mdd) && mdd != 0)
                mu_ann / abs(mdd) else NA_real_

  # -- Value at Risk (parametric, normal) ------------------------------------
  # VaR_α = μ − z_α × σ   where z_α = Φ^{-1}(α)
  var_95   <- -(mu + qnorm(0.05) * sigma)          # 1-day 95% VaR
  var_99   <- -(mu + qnorm(0.01) * sigma)          # 1-day 99% VaR

  # -- Expected Shortfall / CVaR (parametric) --------------------------------
  # ES_α = μ − σ × φ(z_α) / α   where φ is the standard normal pdf
  es_95    <- -(mu - sigma * dnorm(qnorm(0.05)) / 0.05)
  es_99    <- -(mu - sigma * dnorm(qnorm(0.01)) / 0.01)

  # -- Cumulative return ------------------------------------------------------
  total_ret <- prod(1 + r, na.rm = TRUE) - 1

  c(
    n_obs          = n,
    total_return   = total_ret,
    ann_return     = mu_ann,
    ann_volatility = sig_ann,
    skewness       = skew,
    excess_kurtosis= kurt - 3,
    sharpe_ratio   = sharpe,
    sortino_ratio  = sortino,
    max_drawdown   = mdd,
    calmar_ratio   = calmar,
    var_95_daily   = var_95,
    var_99_daily   = var_99,
    es_95_daily    = es_95,
    es_99_daily    = es_99
  )
}

#' Compute portfolio-level return statistics from weights and asset returns
#'
#' Portfolio return:   r_p = w' r_t   (vector dot product at each t)
#' Portfolio variance: σ²_p = w' Σ w  (quadratic form on covariance matrix)
#'
#' @param returns_wide Wide tibble: date + one column per asset
#' @param weights      Named numeric vector (must sum to ~1)
#' @param rf           Annual risk-free rate
#' @return Named list: returns vector, price index, statistics tibble
#' @export
portfolio_statistics <- function(returns_wide, weights, rf = 0.0525) {
  assets      <- names(weights)
  ret_matrix  <- as.matrix(returns_wide[, assets])

  # Portfolio returns: r_p,t = Σ_i w_i × r_i,t
  port_ret    <- as.numeric(ret_matrix %*% weights)

  # Portfolio NAV index (starts at 100)
  port_nav    <- 100 * cumprod(1 + port_ret)

  # Covariance matrix (annualised)
  Sigma       <- cov(ret_matrix, use = "complete.obs") * 252
  sigma_p     <- sqrt(as.numeric(t(weights) %*% Sigma %*% weights))

  stats       <- return_statistics(port_ret, port_nav, ann_factor = 252, rf = rf)

  list(
    returns    = port_ret,
    nav        = port_nav,
    dates      = returns_wide$date,
    weights    = weights,
    cov_matrix = Sigma,
    port_vol   = sigma_p,
    stats      = stats
  )
}

# =============================================================================
# SECTION 2: CAPM — Capital Asset Pricing Model
# =============================================================================

#' Estimate CAPM parameters via OLS for a single asset
#'
#' The CAPM regression:
#'   r_i,t − rf_t = α_i + β_i (r_m,t − rf_t) + ε_i,t
#'
#' where:
#'   α_i = Jensen's alpha (abnormal return unexplained by market exposure)
#'   β_i = systematic risk (covariance with market / market variance)
#'   R²  = fraction of asset variance explained by market factor
#'
#' @param asset_returns   Numeric vector of asset excess returns
#' @param market_returns  Numeric vector of market excess returns
#' @return Named list with alpha, beta, r_squared, t_stats, p_values, residuals
#' @export
capm_regression <- function(asset_returns, market_returns) {

  # Excess returns (rf already subtracted upstream)
  y <- asset_returns
  x <- market_returns
  n <- length(y)

  # OLS in matrix form: β = (X'X)^{-1} X'y
  X     <- cbind(1, x)                      # design matrix with intercept
  XtX   <- crossprod(X)                     # X'X  (2×2)
  XtX_inv <- solve(XtX)
  coefs <- XtX_inv %*% crossprod(X, y)      # β = (X'X)^{-1}X'y

  alpha <- coefs[1, 1]
  beta  <- coefs[2, 1]

  # Residuals and diagnostics
  y_hat   <- X %*% coefs
  resid   <- y - y_hat
  rss     <- sum(resid^2)
  tss     <- sum((y - mean(y))^2)
  r2      <- 1 - rss / tss
  r2_adj  <- 1 - (1 - r2) * (n - 1) / (n - 2)

  # Standard errors: s² (X'X)^{-1}
  s2      <- rss / (n - 2)
  se      <- sqrt(diag(s2 * XtX_inv))

  # t-statistics and two-sided p-values
  t_stats <- coefs[, 1] / se
  p_vals  <- 2 * pt(abs(t_stats), df = n - 2, lower.tail = FALSE)

  # Treynor ratio: (r_p - rf) / beta
  treynor <- (mean(y) * 252) / beta

  list(
    alpha         = alpha * 252,  # annualised
    beta          = beta,
    r_squared     = r2,
    r_squared_adj = r2_adj,
    treynor_ratio = treynor,
    t_alpha       = t_stats[1],
    t_beta        = t_stats[2],
    p_alpha       = p_vals[1],
    p_beta        = p_vals[2],
    se_alpha      = se[1],
    se_beta       = se[2],
    residuals     = as.numeric(resid),
    idiosyncratic_vol = sd(resid) * sqrt(252),
    systematic_vol    = beta * sd(market_returns) * sqrt(252)
  )
}

#' CAPM analysis for multiple assets against a benchmark
#'
#' @param returns_wide  Wide tibble: date + asset columns
#' @param benchmark_col Name of benchmark column in returns_wide
#' @param rf_annual     Annual risk-free rate
#' @return tibble with one row per asset, all CAPM statistics
#' @export
capm_multi <- function(returns_wide, benchmark_col = "SPY", rf_annual = 0.0525) {
  rf_daily <- rf_annual / 252
  assets   <- setdiff(names(returns_wide), c("date", benchmark_col))

  mkt_excess <- returns_wide[[benchmark_col]] - rf_daily

  purrr::map_dfr(assets, function(a) {
    asset_excess <- returns_wide[[a]] - rf_daily
    res          <- capm_regression(asset_excess, mkt_excess)
    tibble::tibble(
      asset         = a,
      alpha         = res$alpha,
      beta          = res$beta,
      r_squared     = res$r_squared,
      t_alpha       = res$t_alpha,
      p_alpha       = res$p_alpha,
      treynor_ratio = res$treynor_ratio,
      idio_vol      = res$idiosyncratic_vol,
      sys_vol       = res$systematic_vol
    )
  })
}

# =============================================================================
# SECTION 3: MEAN-VARIANCE OPTIMISATION (Markowitz, 1952)
# =============================================================================

#' Compute the Minimum Variance Portfolio weights
#'
#' Solves: min w' Σ w  subject to  Σ w_i = 1, w_i ≥ 0
#' Via analytical solution for the unconstrained case:
#'   w_MVP = (Σ^{-1} 1) / (1' Σ^{-1} 1)
#'
#' For constrained case (no short-selling), uses quadprog.
#'
#' @param cov_matrix  Annualised covariance matrix (p × p)
#' @param allow_short Logical — allow negative weights
#' @return Named numeric vector of weights
#' @export
min_variance_portfolio <- function(cov_matrix, allow_short = FALSE) {
  p    <- nrow(cov_matrix)
  ones <- rep(1, p)

  if (allow_short) {
    Sigma_inv <- solve(cov_matrix)
    w         <- Sigma_inv %*% ones
    w         <- w / sum(w)
    return(setNames(as.numeric(w), rownames(cov_matrix)))
  }

  requireNamespace("quadprog", quietly = TRUE)

  # Quadratic program: min (1/2) w' D w  s.t. A' w >= b
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, p)

  # Equality: sum(w) = 1  and non-negativity: w_i >= 0
  Amat <- cbind(ones, diag(p))
  bvec <- c(1, rep(0, p))

  sol  <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  setNames(sol$solution, rownames(cov_matrix))
}

#' Compute Maximum Sharpe Ratio Portfolio
#'
#' Solves: max (w' μ − rf) / sqrt(w' Σ w)
#' Via the analytical tangency portfolio formula:
#'   w* = Σ^{-1} (μ − rf·1) / [1' Σ^{-1} (μ − rf·1)]
#'
#' @param mu          Expected return vector (annualised, p × 1)
#' @param cov_matrix  Annualised covariance matrix (p × p)
#' @param rf          Annual risk-free rate
#' @param allow_short Logical
#' @return Named numeric vector of weights
#' @export
max_sharpe_portfolio <- function(mu, cov_matrix, rf = 0.0525, allow_short = FALSE) {
  p    <- length(mu)
  ones <- rep(1, p)

  if (allow_short) {
    Sigma_inv <- solve(cov_matrix)
    excess    <- mu - rf
    w         <- Sigma_inv %*% excess
    w         <- w / sum(w)
    return(setNames(as.numeric(w), names(mu)))
  }

  requireNamespace("quadprog", quietly = TRUE)

  # Transform to QP: maximise Sharpe ≡ minimise variance for given return target
  # Iterate over return targets to find the tangency point
  n_pts    <- 100L
  mu_range <- seq(min(mu) * 0.5, max(mu) * 1.5, length.out = n_pts)

  sharpe_vals <- numeric(n_pts)
  weight_list <- vector("list", n_pts)

  for (i in seq_along(mu_range)) {
    tryCatch({
      Dmat <- 2 * cov_matrix
      dvec <- rep(0, p)
      Amat <- cbind(ones, diag(p), mu)
      bvec <- c(1, rep(0, p), mu_range[i])
      sol  <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
      w    <- sol$solution
      ret  <- sum(w * mu)
      vol  <- sqrt(as.numeric(t(w) %*% cov_matrix %*% w))
      sharpe_vals[i]  <- (ret - rf) / vol
      weight_list[[i]] <- w
    }, error = function(e) NULL)
  }

  best <- which.max(sharpe_vals)
  setNames(weight_list[[best]], names(mu))
}

#' Construct the Efficient Frontier
#'
#' Generates the mean-variance efficient frontier by solving the constrained
#' QP at a grid of target return levels.
#'
#' @param mu          Expected return vector
#' @param cov_matrix  Covariance matrix
#' @param n_points    Number of frontier portfolios
#' @param rf          Risk-free rate (for Sharpe computation)
#' @return tibble: target_return, portfolio_vol, sharpe_ratio, weights (list-col)
#' @export
efficient_frontier <- function(mu, cov_matrix, n_points = 100L, rf = 0.0525) {
  requireNamespace("quadprog", quietly = TRUE)

  p        <- length(mu)
  ones     <- rep(1, p)
  mu_min   <- min(mu)
  mu_max   <- max(mu)
  targets  <- seq(mu_min, mu_max, length.out = n_points)

  purrr::map_dfr(targets, function(target) {
    tryCatch({
      Dmat <- 2 * cov_matrix
      dvec <- rep(0, p)
      Amat <- cbind(ones, diag(p), mu)
      bvec <- c(1, rep(0, p), target)

      sol  <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
      w    <- sol$solution
      vol  <- sqrt(as.numeric(t(w) %*% cov_matrix %*% w))

      tibble::tibble(
        target_return  = target,
        portfolio_vol  = vol,
        sharpe_ratio   = (target - rf) / vol,
        weights        = list(setNames(w, names(mu)))
      )
    }, error = function(e) NULL)
  })
}

# =============================================================================
# SECTION 4: ROLLING RISK METRICS
# =============================================================================

#' Compute rolling risk metrics for a return series
#'
#' @param returns    Numeric vector of returns
#' @param dates      Date vector aligned with returns
#' @param window     Rolling window width (trading days)
#' @param ann_factor Annualisation factor
#' @param rf         Annual risk-free rate
#' @return tibble: date, roll_vol, roll_sharpe, roll_beta (if benchmark provided)
#' @export
rolling_risk_metrics <- function(returns, dates, window = 63L,
                                  ann_factor = 252, rf = 0.0525) {
  n     <- length(returns)
  rf_pd <- rf / ann_factor

  roll_vol    <- roll_apply(returns, window, sd) * sqrt(ann_factor)
  roll_mean   <- roll_apply(returns, window, mean) * ann_factor
  roll_sharpe <- (roll_mean - rf) / roll_vol

  # Rolling VaR (historical, 95%)
  roll_var95 <- roll_apply(returns, window, quantile, probs = 0.05)

  tibble::tibble(
    date         = dates,
    rolling_vol  = roll_vol,
    rolling_ret  = roll_mean,
    rolling_sharpe = roll_sharpe,
    rolling_var95  = -roll_var95
  )
}
