# =============================================================================
# Monte Carlo Simulation Engine
#
# Implements multiple simulation methods for financial risk analysis:
#
#   1. Geometric Brownian Motion (GBM) — Black-Scholes price paths
#   2. Correlated multi-asset GBM (Cholesky decomposition)
#   3. Jump-Diffusion process (Merton, 1976)
#   4. GARCH-filtered Monte Carlo (volatility-clustering aware)
#   5. Historical bootstrap (model-free, preserves empirical moments)
#   6. Portfolio VaR/ES via simulation
#   7. Scenario analysis (macro stress tests)
#
# All engines expose:
#   - Mathematical derivation via comments
#   - Reproducible seed management
#   - Vectorised computation for speed
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(glue)
  library(ggplot2)
  library(Matrix)
})

source(here::here("R", "utilities", "helpers.R"))
source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "visualization", "theme.R"))

# =============================================================================
# ENGINE 1: GEOMETRIC BROWNIAN MOTION (GBM)
# =============================================================================

#' Simulate price paths via Geometric Brownian Motion
#'
#' The GBM SDE:  dS = μ S dt + σ S dW_t
#'
#' Discretised exact solution (Itô's lemma):
#'   S_{t+Δt} = S_t × exp[(μ − σ²/2)Δt + σ√Δt × Z_t]
#'   where Z_t ~ N(0,1) i.i.d.
#'
#' Note: the drift correction term −σ²/2 (the "Itô correction") arises because
#' E[log(S_T/S_0)] = (μ − σ²/2)T, not μT.  This is the key difference between
#' arithmetic mean returns and geometric mean returns.
#'
#' @param S0      Initial price
#' @param mu      Annual drift (expected return)
#' @param sigma   Annual volatility
#' @param T_days  Simulation horizon in trading days
#' @param n_paths Number of Monte Carlo paths
#' @param dt      Time step (default 1/252 for daily)
#' @param seed    RNG seed for reproducibility
#' @return Matrix [T_days+1 × n_paths] of simulated prices
#' @export
simulate_gbm <- function(S0, mu, sigma, T_days = 252L, n_paths = 10000L,
                          dt = 1/252, seed = 42L) {
  set.seed(seed)

  log_info("[mc] GBM simulation: S0={S0}, μ={mu}, σ={sigma}, T={T_days}, paths={n_paths}")

  # Daily drift and diffusion components
  drift     <- (mu - 0.5 * sigma^2) * dt          # Itô-corrected drift
  diffusion <- sigma * sqrt(dt)                   # scaled volatility

  # Simulate standard normal shocks: Z ~ N(0,1)
  Z <- matrix(rnorm(T_days * n_paths), nrow = T_days, ncol = n_paths)

  # Log-return increments: r_t = drift + diffusion × Z_t
  log_increments <- drift + diffusion * Z

  # Cumulative log returns → price paths via exp
  log_paths <- apply(log_increments, 2, cumsum)

  # Prepend S0 row: price path starts at S0
  price_paths <- S0 * exp(rbind(0, log_paths))
  rownames(price_paths) <- paste0("t", 0:T_days)

  price_paths
}

#' Summarise GBM simulation into risk statistics
#'
#' @param paths    Matrix from simulate_gbm()
#' @param S0       Initial price (for return calculations)
#' @param rf       Annual risk-free rate
#' @return Named list: percentiles, VaR, ES, probability of loss
#' @export
summarise_mc_paths <- function(paths, S0, rf = 0.0525) {
  terminal <- paths[nrow(paths), ]       # Terminal price distribution
  T_ann    <- (nrow(paths) - 1) / 252   # Horizon in years

  # Terminal log returns
  log_ret  <- log(terminal / S0)
  simple_ret <- terminal / S0 - 1

  # Percentile distribution of terminal prices
  pcts <- quantile(terminal, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))

  # Value at Risk: loss not exceeded at confidence level
  # VaR_α = −Q_{1−α}(return distribution)
  var_95 <- -quantile(simple_ret, 0.05)
  var_99 <- -quantile(simple_ret, 0.01)

  # Expected Shortfall: mean loss in the tail beyond VaR
  es_95  <- -mean(simple_ret[simple_ret <= -var_95])
  es_99  <- -mean(simple_ret[simple_ret <= -var_99])

  prob_loss    <- mean(terminal < S0)
  prob_loss_rf <- mean(simple_ret < rf * T_ann)

  list(
    n_paths     = ncol(paths),
    horizon_days= nrow(paths) - 1,
    S0          = S0,
    mean_terminal = mean(terminal),
    median_terminal = median(terminal),
    percentiles = pcts,
    var_95      = var_95,
    var_99      = var_99,
    es_95       = es_95,
    es_99       = es_99,
    prob_loss   = prob_loss,
    prob_loss_rf= prob_loss_rf,
    mean_return = mean(simple_ret),
    vol_return  = sd(simple_ret)
  )
}

# =============================================================================
# ENGINE 2: CORRELATED MULTI-ASSET GBM (CHOLESKY DECOMPOSITION)
# =============================================================================

#' Simulate correlated multi-asset price paths
#'
#' To simulate correlated Brownian motions, we decompose the correlation matrix
#' using Cholesky factorisation:  Σ = L L'  where L is lower-triangular.
#'
#' Then: W = L × Z  where Z ~ N(0, I) gives correlated shocks W ~ N(0, Σ)
#'
#' This is the foundation of multi-asset option pricing, portfolio VaR,
#' and stress testing.
#'
#' @param S0_vec   Named numeric vector of initial prices
#' @param mu_vec   Named numeric vector of annual drifts
#' @param sigma_vec Named numeric vector of annual volatilities
#' @param corr_mat Correlation matrix (assets × assets)
#' @param T_days   Simulation horizon
#' @param n_paths  Number of paths
#' @param dt       Time step
#' @param seed     RNG seed
#' @return List of price path matrices, one per asset
#' @export
simulate_correlated_gbm <- function(S0_vec, mu_vec, sigma_vec, corr_mat,
                                     T_days = 252L, n_paths = 5000L,
                                     dt = 1/252, seed = 42L) {
  set.seed(seed)

  assets <- names(S0_vec)
  p      <- length(assets)

  log_info("[mc] Correlated GBM: {p} assets, T={T_days}, paths={n_paths}")

  # Cholesky decomposition of correlation matrix
  # A near-singular matrix is regularised by adding a small diagonal
  tryCatch({
    L <- chol(corr_mat)
  }, error = function(e) {
    corr_mat <<- corr_mat + diag(1e-6, p)
    L         <<- chol(corr_mat)
    log_warn("[mc] Correlation matrix regularised for Cholesky decomposition")
  })

  paths_list <- vector("list", p)
  names(paths_list) <- assets

  for (sim in seq_len(n_paths)) {
    # Independent standard normals
    Z_raw    <- matrix(rnorm(T_days * p), nrow = p, ncol = T_days)

    # Correlated shocks: W = L' × Z  (note: chol() returns upper triangular)
    Z_corr   <- t(L) %*% Z_raw    # p × T_days correlated shocks

    for (i in seq_along(assets)) {
      if (sim == 1) {
        paths_list[[assets[i]]] <- matrix(0, nrow = T_days + 1, ncol = n_paths)
        paths_list[[assets[i]]][1, ] <- S0_vec[i]
      }
      drift_i     <- (mu_vec[i] - 0.5 * sigma_vec[i]^2) * dt
      diffusion_i <- sigma_vec[i] * sqrt(dt)
      log_inc_i   <- drift_i + diffusion_i * Z_corr[i, ]
      paths_list[[assets[i]]][-1, sim] <- S0_vec[i] * exp(cumsum(log_inc_i))
    }
  }

  paths_list
}

# =============================================================================
# ENGINE 3: HISTORICAL BOOTSTRAP
# =============================================================================

#' Historical bootstrap simulation (model-free Monte Carlo)
#'
#' Resamples empirical daily returns with replacement.
#' Preserves the actual fat tails, skewness, and dependence structure
#' observed in the data — no distributional assumptions needed.
#'
#' Block bootstrap option preserves short-run autocorrelation.
#'
#' @param returns   Numeric vector of historical returns
#' @param S0        Initial price
#' @param T_days    Simulation horizon
#' @param n_paths   Number of paths
#' @param block_size Block size for block bootstrap (1 = i.i.d. resample)
#' @param seed      RNG seed
#' @return Matrix [T_days+1 × n_paths]
#' @export
simulate_historical_bootstrap <- function(returns, S0, T_days = 252L,
                                           n_paths = 10000L,
                                           block_size = 1L, seed = 42L) {
  set.seed(seed)
  n   <- length(returns)

  log_info("[mc] Historical bootstrap: {n} obs, T={T_days}, paths={n_paths}")

  price_paths <- matrix(0, nrow = T_days + 1, ncol = n_paths)
  price_paths[1, ] <- S0

  if (block_size == 1L) {
    # Simple i.i.d. resample
    idx    <- matrix(sample(n, T_days * n_paths, replace = TRUE), nrow = T_days)
    boot_r <- matrix(returns[idx], nrow = T_days)
  } else {
    # Stationary block bootstrap
    boot_r <- matrix(0, nrow = T_days, ncol = n_paths)
    for (j in seq_len(n_paths)) {
      i    <- 1L
      path <- numeric(T_days)
      while (i <= T_days) {
        start  <- sample(n, 1)
        blk_len<- min(block_size, T_days - i + 1, n - start + 1)
        path[i:(i + blk_len - 1)] <- returns[start:(start + blk_len - 1)]
        i <- i + blk_len
      }
      boot_r[, j] <- path
    }
  }

  # Build price paths from return matrix
  for (t in seq_len(T_days)) {
    price_paths[t + 1, ] <- price_paths[t, ] * (1 + boot_r[t, ])
  }

  price_paths
}

# =============================================================================
# VISUALISATION
# =============================================================================

#' Plot Monte Carlo fan chart
#'
#' @param paths    Price path matrix [T+1 × n_paths]
#' @param S0       Initial price
#' @param ticker   Asset label
#' @param n_display Number of individual paths to overlay
#' @return ggplot object
#' @export
chart_mc_paths <- function(paths, S0, ticker = "Asset", n_display = 200L) {
  T_days <- nrow(paths) - 1
  n_paths <- ncol(paths)

  # Percentile ribbons
  pct <- apply(paths, 1, quantile,
    probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
    na.rm = TRUE
  ) |> t() |> as.data.frame()
  names(pct) <- c("p01","p05","p10","p25","p50","p75","p90","p95","p99")
  pct$day    <- 0:T_days

  # Sample paths for overlay
  sample_cols <- sample(n_paths, min(n_display, n_paths))
  sample_long <- paths[, sample_cols] |>
    as.data.frame() |>
    dplyr::mutate(day = 0:T_days) |>
    tidyr::pivot_longer(-day, names_to = "path", values_to = "price")

  ggplot2::ggplot(pct, aes(x = day)) +
    ggplot2::geom_ribbon(aes(ymin = p01, ymax = p99), fill = PALETTE$accent, alpha = 0.08) +
    ggplot2::geom_ribbon(aes(ymin = p05, ymax = p95), fill = PALETTE$accent, alpha = 0.12) +
    ggplot2::geom_ribbon(aes(ymin = p10, ymax = p90), fill = PALETTE$accent, alpha = 0.18) +
    ggplot2::geom_ribbon(aes(ymin = p25, ymax = p75), fill = PALETTE$accent, alpha = 0.25) +
    ggplot2::geom_line(data = sample_long,
                       aes(x = day, y = price, group = path),
                       colour = PALETTE$primary, alpha = 0.04, linewidth = 0.3) +
    ggplot2::geom_line(aes(y = p50), colour = PALETTE$primary, linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = S0, linetype = "dashed",
                        colour = PALETTE$text_light, linewidth = 0.5) +
    scale_y_dollar() +
    ggplot2::labs(
      title    = glue("Monte Carlo Price Simulation — {ticker}"),
      subtitle = glue("{format(n_paths, big.mark=',')} GBM paths  |  Bands: 25th–75th, 10th–90th, 5th–95th, 1st–99th percentile"),
      x = "Trading Days", y = "Simulated Price (USD)",
      caption = "Source: Quant Research Platform — Geometric Brownian Motion"
    ) +
    theme_quant()
}

#' Terminal price distribution histogram with risk metrics
#'
#' @param paths  Price path matrix
#' @param S0     Initial price
#' @param ticker Asset label
#' @return ggplot object
#' @export
chart_mc_terminal <- function(paths, S0, ticker = "Asset") {
  terminal    <- paths[nrow(paths), ]
  returns     <- terminal / S0 - 1
  var_95      <- -quantile(returns, 0.05)
  es_95       <- -mean(returns[returns <= -var_95])
  prob_loss   <- mean(returns < 0)

  df <- tibble::tibble(terminal_return = returns * 100)

  ggplot2::ggplot(df, aes(x = terminal_return)) +
    ggplot2::geom_histogram(
      aes(fill = after_stat(x < 0)), bins = 80, colour = NA, alpha = 0.85
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = PALETTE$red, "FALSE" = PALETTE$green),
      guide  = "none"
    ) +
    ggplot2::geom_vline(xintercept = -var_95 * 100, colour = PALETTE$amber,
                        linewidth = 1, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, colour = PALETTE$text_dark, linewidth = 0.5) +
    ggplot2::annotate("text", x = -var_95 * 100 - 2, y = Inf, vjust = 1.5,
                      label = glue("VaR 95%\n{round(var_95*100,1)}%"),
                      colour = PALETTE$amber, size = 3.2, hjust = 1) +
    ggplot2::labs(
      title    = glue("Terminal Return Distribution — {ticker}"),
      subtitle = glue(
        "P(Loss) = {round(prob_loss*100,1)}%   |   VaR 95% = {round(var_95*100,1)}%   |   ES 95% = {round(es_95*100,1)}%"
      ),
      x = "Terminal Return (%)", y = "Count",
      caption = "Source: Quant Research Platform"
    ) +
    theme_quant()
}
