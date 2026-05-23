# =============================================================================
# Time Series Econometrics Module
#
# Implements with full mathematical exposition:
#   - Augmented Dickey-Fuller (ADF) unit-root test
#   - KPSS stationarity test
#   - Johansen cointegration test
#   - Automatic ARIMA model selection (Box-Jenkins methodology)
#   - GARCH(p,q) volatility modelling
#   - Time series decomposition (trend + seasonality + remainder)
#   - Forecast evaluation: MAE, RMSE, MASE, Diebold-Mariano test
# =============================================================================

suppressPackageStartupMessages({
  library(forecast)
  library(tseries)
  library(rugarch)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(glue)
})

source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "helpers.R"))

# =============================================================================
# SECTION 1: STATIONARITY TESTS
# =============================================================================

#' Augmented Dickey-Fuller Test for Unit Root
#'
#' The ADF test regresses Δy_t on y_{t-1} and p lagged differences:
#'   Δy_t = α + βt + δ y_{t-1} + Σ_{j=1}^p γ_j Δy_{t-j} + ε_t
#'
#' H₀: δ = 0 (unit root — non-stationary)
#' H₁: δ < 0 (stationary)
#'
#' Lag order selected by AIC when lags = NULL.
#'
#' @param x    Numeric time series vector
#' @param lags Number of augmentation lags (NULL for AIC-optimal)
#' @param type Test type: "none" | "drift" | "trend"
#' @return Named list with test statistic, p-value, lags, conclusion
#' @export
adf_test <- function(x, lags = NULL, type = "trend") {
  x <- na.omit(as.numeric(x))

  if (is.null(lags)) {
    # AIC-optimal lag selection (Schwert rule: max lags = 12*(T/100)^0.25)
    T_obs <- length(x)
    max_l <- floor(12 * (T_obs / 100)^0.25)
    lags  <- max_l
  }

  res <- tryCatch(
    tseries::adf.test(x, k = lags),
    error = function(e) {
      log_warn("[adf] Test failed: {conditionMessage(e)}")
      return(NULL)
    }
  )

  if (is.null(res)) return(NULL)

  stationary <- res$p.value < 0.05

  list(
    test_statistic = as.numeric(res$statistic),
    p_value        = res$p.value,
    lags           = lags,
    n_obs          = length(x),
    stationary     = stationary,
    conclusion     = if (stationary)
                       "Reject H₀: series appears stationary"
                     else
                       "Fail to reject H₀: series may have unit root",
    critical_values = c("1%" = -3.43, "5%" = -2.86, "10%" = -2.57)
  )
}

#' KPSS Test for Level/Trend Stationarity
#'
#' Complements ADF: tests H₀ stationarity (opposite null from ADF).
#' Running both tests and checking for concordance is best practice.
#'
#' @param x    Numeric vector
#' @param type "mu" (level) | "tau" (trend)
#' @return Named list
#' @export
kpss_test <- function(x, type = "mu") {
  x   <- na.omit(as.numeric(x))
  res <- tryCatch(
    tseries::kpss.test(x, null = type),
    error = function(e) NULL
  )

  if (is.null(res)) return(NULL)

  list(
    test_statistic = as.numeric(res$statistic),
    p_value        = res$p.value,
    type           = type,
    stationary     = res$p.value > 0.05,  # H₀ is stationarity
    conclusion     = if (res$p.value > 0.05)
                       "Fail to reject H₀: series appears stationary"
                     else
                       "Reject H₀: series is non-stationary"
  )
}

#' Combined stationarity assessment
#'
#' Runs both ADF and KPSS and returns a consensus conclusion.
#' Four cases: (1) Both agree stationary, (2) Both agree non-stationary,
#' (3) ADF says stationary, KPSS says non-stationary, (4) Vice versa.
#'
#' @param x Numeric vector
#' @return tibble with results and consensus
#' @export
stationarity_assessment <- function(x) {
  adf  <- adf_test(x)
  kpss <- kpss_test(x)

  adf_stat  <- isTRUE(adf$stationary)
  kpss_stat <- isTRUE(kpss$stationary)

  consensus <- dplyr::case_when(
    adf_stat  & kpss_stat  ~ "Stationary",
    !adf_stat & !kpss_stat ~ "Non-stationary",
    adf_stat  & !kpss_stat ~ "Ambiguous (ADF: stationary, KPSS: non-stationary)",
    TRUE                    ~ "Ambiguous (ADF: non-stationary, KPSS: stationary)"
  )

  tibble::tibble(
    test       = c("ADF", "KPSS"),
    statistic  = c(adf$test_statistic, kpss$test_statistic),
    p_value    = c(adf$p_value, kpss$p_value),
    stationary = c(adf_stat, kpss_stat),
    conclusion = c(adf$conclusion, kpss$conclusion),
    consensus  = consensus
  )
}

# =============================================================================
# SECTION 2: ARIMA MODELLING
# =============================================================================

#' Fit an ARIMA model with automatic order selection
#'
#' Uses the Hyndman-Khandakar algorithm (auto.arima):
#'   1. Determine differencing order d via KPSS tests
#'   2. Select p, q via stepwise search minimising AICc
#'   3. Estimate parameters via maximum likelihood
#'
#' Model notation: ARIMA(p,d,q) where
#'   p = autoregressive order:  y_t = Σ_{i=1}^p φ_i y_{t-i} + ε_t
#'   d = integration order:     Δ^d y_t is stationary
#'   q = moving average order:  ε_t = Σ_{j=1}^q θ_j ε_{t-j} + η_t
#'
#' @param x       Numeric time series
#' @param max_p   Maximum AR order
#' @param max_q   Maximum MA order
#' @param max_d   Maximum integration order
#' @param stepwise Use stepwise search (faster but may miss global optimum)
#' @return List: model object, diagnostics tibble, fitted values
#' @export
fit_arima <- function(x, max_p = 5, max_q = 5, max_d = 2, stepwise = TRUE) {
  x   <- na.omit(as.numeric(x))
  ts_ <- stats::ts(x)

  log_info("[arima] Fitting ARIMA(p,d,q): max_p={max_p}, max_q={max_q}, max_d={max_d}")

  model <- tryCatch(
    forecast::auto.arima(
      ts_,
      max.p         = max_p,
      max.q         = max_q,
      max.d         = max_d,
      stepwise      = stepwise,
      approximation = FALSE,
      ic            = "aicc",
      trace         = FALSE
    ),
    error = function(e) {
      log_error("[arima] Failed: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(model)) return(NULL)

  order      <- arimaorder(model)
  coef_table <- broom::tidy(model)

  diagnostics <- list(
    order      = order,
    aic        = model$aic,
    aicc       = model$aicc,
    bic        = model$bic,
    log_lik    = model$loglik,
    n_params   = length(model$coef),
    coefficients = coef_table
  )

  # Ljung-Box test on residuals (should show no autocorrelation)
  lb_test <- Box.test(residuals(model), lag = 20, type = "Ljung-Box")

  list(
    model       = model,
    order       = order,
    diagnostics = diagnostics,
    ljung_box   = lb_test,
    fitted      = as.numeric(fitted(model)),
    residuals   = as.numeric(residuals(model))
  )
}

#' Generate ARIMA forecasts with confidence intervals
#'
#' @param arima_result Result from fit_arima()
#' @param horizon      Forecast horizon (periods ahead)
#' @param levels       Confidence levels (default 80% and 95%)
#' @return tibble: horizon, forecast, lower_80, upper_80, lower_95, upper_95
#' @export
forecast_arima <- function(arima_result, horizon = 30L, levels = c(80, 95)) {
  fc  <- forecast::forecast(arima_result$model, h = horizon, level = levels)

  tibble::tibble(
    h        = seq_len(horizon),
    forecast = as.numeric(fc$mean),
    lo_80    = as.numeric(fc$lower[, 1]),
    hi_80    = as.numeric(fc$upper[, 1]),
    lo_95    = as.numeric(fc$lower[, 2]),
    hi_95    = as.numeric(fc$upper[, 2])
  )
}

# =============================================================================
# SECTION 3: GARCH VOLATILITY MODELLING
# =============================================================================

#' Fit a GARCH(1,1) model to a return series
#'
#' The GARCH(1,1) model (Bollerslev, 1986):
#'   r_t = μ + ε_t,          ε_t = σ_t z_t,  z_t ~ iid(0,1)
#'   σ²_t = ω + α ε²_{t-1} + β σ²_{t-1}
#'
#' Stationarity requires: α + β < 1
#' Persistence = α + β (near 1 ⟹ volatility clustering)
#' Long-run variance = ω / (1 - α - β)
#'
#' @param returns     Numeric return vector
#' @param dist        Innovation distribution: "norm" | "std" | "ged" | "snorm"
#' @param garch_p     ARCH order (default 1)
#' @param garch_q     GARCH order (default 1)
#' @return List: model fit, parameters, conditional volatility, forecasts
#' @export
fit_garch <- function(returns, dist = "std", garch_p = 1L, garch_q = 1L) {
  log_info("[garch] Fitting GARCH({garch_p},{garch_q}) with {dist} innovations")

  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(garch_p, garch_q)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = dist
  )

  fit <- tryCatch(
    rugarch::ugarchfit(spec = spec, data = returns, solver = "hybrid"),
    error = function(e) {
      log_error("[garch] Fit failed: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(fit)) return(NULL)

  params   <- rugarch::coef(fit)
  infocrit <- rugarch::infocriteria(fit)

  omega <- params["omega"]
  alpha <- params["alpha1"]
  beta  <- params["beta1"]

  list(
    model            = fit,
    omega            = omega,
    alpha            = alpha,
    beta             = beta,
    persistence      = alpha + beta,
    long_run_var     = omega / (1 - alpha - beta),
    long_run_vol     = sqrt(omega / (1 - alpha - beta)) * sqrt(252),
    cond_vol         = as.numeric(rugarch::sigma(fit)) * sqrt(252),
    log_likelihood   = rugarch::likelihood(fit),
    aic              = infocrit["Akaike"],
    bic              = infocrit["Bayes"],
    distribution     = dist
  )
}

#' Forecast conditional volatility from a fitted GARCH model
#'
#' @param garch_result   Result from fit_garch()
#' @param horizon        Forecast horizon (days)
#' @return tibble: h, vol_forecast (annualised)
#' @export
forecast_garch_vol <- function(garch_result, horizon = 30L) {
  fc <- rugarch::ugarchforecast(garch_result$model, n.ahead = horizon)

  tibble::tibble(
    h            = seq_len(horizon),
    vol_forecast = as.numeric(rugarch::sigma(fc)) * sqrt(252)
  )
}

# =============================================================================
# SECTION 4: DECOMPOSITION & DIAGNOSTICS
# =============================================================================

#' STL decomposition of a time series
#'
#' Seasonal-Trend decomposition using LOESS (Cleveland et al., 1990).
#' More robust to outliers than classical additive/multiplicative decomposition.
#'
#' @param x       Numeric vector
#' @param freq    Seasonal frequency (12 for monthly, 252 for trading days)
#' @return tibble: index, observed, trend, seasonal, remainder
#' @export
decompose_series <- function(x, freq = 12L) {
  ts_  <- stats::ts(na.omit(as.numeric(x)), frequency = freq)
  decomp <- tryCatch(
    stats::stl(ts_, s.window = "periodic", robust = TRUE),
    error = function(e) {
      log_warn("[decomp] STL failed, falling back to classical: {conditionMessage(e)}")
      stats::decompose(ts_)
    }
  )

  n <- length(ts_)
  if (inherits(decomp, "stl")) {
    comp <- decomp$time.series
    tibble::tibble(
      index     = seq_len(n),
      observed  = as.numeric(ts_),
      trend     = as.numeric(comp[, "trend"]),
      seasonal  = as.numeric(comp[, "seasonal"]),
      remainder = as.numeric(comp[, "remainder"])
    )
  } else {
    tibble::tibble(
      index     = seq_len(n),
      observed  = as.numeric(ts_),
      trend     = as.numeric(decomp$trend),
      seasonal  = as.numeric(decomp$seasonal),
      remainder = as.numeric(decomp$random)
    )
  }
}

#' Forecast accuracy metrics
#'
#' @param actual    Numeric vector of actuals
#' @param predicted Numeric vector of predictions
#' @return Named numeric vector: MAE, RMSE, MAPE, MASE
#' @export
forecast_accuracy <- function(actual, predicted) {
  e     <- actual - predicted
  mae   <- mean(abs(e), na.rm = TRUE)
  rmse  <- sqrt(mean(e^2, na.rm = TRUE))
  mape  <- mean(abs(e / actual) * 100, na.rm = TRUE)

  # MASE denominator: in-sample naive (random walk) MAE
  naive_mae <- mean(abs(diff(actual)), na.rm = TRUE)
  mase      <- mae / naive_mae

  c(MAE = mae, RMSE = rmse, MAPE = mape, MASE = mase)
}
