# =============================================================================
# Forecasting Pipeline Module
#
# Orchestrates a reproducible, multi-model forecasting workflow:
#   1. Data preparation and stationarity enforcement
#   2. Model fitting: ARIMA, ETS, TBATS, Prophet (if available)
#   3. Ensemble forecasting via weighted model averaging
#   4. Forecast evaluation with backtesting (walk-forward validation)
#   5. Uncertainty quantification via prediction intervals
#   6. Structured output for downstream reporting
# =============================================================================

suppressPackageStartupMessages({
  library(forecast)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(glue)
  library(lubridate)
  library(ggplot2)
})

source(here::here("R", "econometrics", "time_series.R"))
source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "visualization", "theme.R"))

# =============================================================================
# SECTION 1: MULTI-MODEL FORECAST ENGINE
# =============================================================================

#' Fit all candidate forecasting models to a time series
#'
#' @param x        Numeric vector (stationary or auto-differenced internally)
#' @param horizon  Forecast horizon
#' @param freq     Seasonal frequency (1 for non-seasonal financial data)
#' @return Named list of model fit objects
#' @export
fit_all_models <- function(x, horizon = 30L, freq = 1L) {
  ts_obj <- stats::ts(na.omit(as.numeric(x)), frequency = freq)
  models <- list()

  # --- ARIMA ------------------------------------------------------------------
  log_info("[forecast] Fitting ARIMA...")
  models$arima <- tryCatch(
    forecast::auto.arima(ts_obj, stepwise = FALSE, approximation = FALSE,
                         ic = "aicc", trace = FALSE),
    error = function(e) { log_warn("[forecast] ARIMA failed: {e$message}"); NULL }
  )

  # --- ETS (Exponential Smoothing State Space) --------------------------------
  # ETS(error, trend, seasonality) automatically selects:
  # Error: Additive (A) or Multiplicative (M)
  # Trend: None (N), Additive (A), Additive Damped (Ad)
  # Season: None (N), Additive (A), Multiplicative (M)
  log_info("[forecast] Fitting ETS...")
  models$ets <- tryCatch(
    forecast::ets(ts_obj, ic = "aicc"),
    error = function(e) { log_warn("[forecast] ETS failed: {e$message}"); NULL }
  )

  # --- Holt-Winters (if seasonal) ---------------------------------------------
  if (freq > 1) {
    log_info("[forecast] Fitting Holt-Winters...")
    models$holt_winters <- tryCatch(
      stats::HoltWinters(ts_obj),
      error = function(e) { log_warn("[forecast] HW failed: {e$message}"); NULL }
    )
  }

  # --- Naive baselines --------------------------------------------------------
  models$naive    <- forecast::naive(ts_obj, h = horizon)
  models$rw_drift <- forecast::rwf(ts_obj, h = horizon, drift = TRUE)
  models$snaive   <- forecast::snaive(ts_obj, h = horizon)

  purrr::compact(models)
}

#' Generate forecasts from all fitted models
#'
#' @param models   Named list from fit_all_models()
#' @param horizon  Forecast horizon
#' @param level    Confidence levels
#' @return Named list of forecast objects
#' @export
generate_forecasts <- function(models, horizon = 30L, level = c(80, 95)) {
  purrr::imap(models, function(m, name) {
    tryCatch(
      forecast::forecast(m, h = horizon, level = level),
      error = function(e) {
        log_warn("[forecast] Forecast failed for {name}: {e$message}")
        NULL
      }
    )
  }) |> purrr::compact()
}

#' Construct a simple ensemble forecast as weighted average of point forecasts
#'
#' Weights can be equal (simple average) or AIC-based (information-theoretic).
#' Information-theoretic weighting: w_i ∝ exp(−0.5 × ΔAIC_i)
#' where ΔAIC_i = AIC_i − min(AIC).
#'
#' @param forecasts Named list of forecast objects from generate_forecasts()
#' @param models    Named list of fitted models (for AIC extraction)
#' @param method    "equal" | "aic"
#' @return tibble: h, ensemble_mean, ensemble_lo_95, ensemble_hi_95
#' @export
ensemble_forecast <- function(forecasts, models = NULL, method = "equal") {
  fc_means <- purrr::map(forecasts, ~ as.numeric(.x$mean))
  horizon  <- length(fc_means[[1]])

  if (method == "aic" && !is.null(models)) {
    aics <- purrr::map_dbl(models, function(m) {
      tryCatch(AIC(m), error = function(e) NA_real_)
    })
    valid <- !is.na(aics)
    delta_aic <- aics[valid] - min(aics[valid])
    weights   <- exp(-0.5 * delta_aic)
    weights   <- weights / sum(weights)

    fc_means  <- fc_means[names(weights)]
  } else {
    n       <- length(fc_means)
    weights <- setNames(rep(1 / n, n), names(fc_means))
  }

  ensemble_mean <- purrr::imap(fc_means, function(fc, name) {
    fc * weights[[name]]
  }) |> purrr::reduce(`+`)

  # Ensemble intervals: variance = sum of (w_i² × variance_i)
  lo_95_list <- purrr::map(forecasts, ~ as.numeric(.x$lower[, 2]))
  hi_95_list <- purrr::map(forecasts, ~ as.numeric(.x$upper[, 2]))

  lo_95 <- Reduce(`+`, purrr::imap(lo_95_list, function(lo, name) lo * weights[[name]]))
  hi_95 <- Reduce(`+`, purrr::imap(hi_95_list, function(hi, name) hi * weights[[name]]))

  tibble::tibble(
    h              = seq_len(horizon),
    ensemble_mean  = ensemble_mean,
    ensemble_lo_95 = lo_95,
    ensemble_hi_95 = hi_95,
    method         = method,
    n_models       = length(forecasts)
  )
}

# =============================================================================
# SECTION 2: WALK-FORWARD (EXPANDING WINDOW) BACKTESTING
# =============================================================================

#' Walk-forward validation of a forecasting model
#'
#' Expanding-window (pseudo-out-of-sample) validation:
#'   For each t from (n_train) to (n − horizon):
#'     1. Fit model on data[1:t]
#'     2. Forecast data[(t+1):(t+horizon)]
#'     3. Compute forecast errors
#'
#' This is the gold standard for time-series model evaluation because it
#' respects temporal ordering and prevents look-ahead bias.
#'
#' @param x          Numeric vector
#' @param model_type "arima" | "ets"
#' @param horizon    Forecast horizon for each step
#' @param min_train  Minimum training window size
#' @return tibble: t, h, actual, forecast, error, squared_error
#' @export
walk_forward_validation <- function(x, model_type = "arima",
                                     horizon = 1L, min_train = 100L) {
  n        <- length(x)
  results  <- vector("list", n - min_train)
  log_info("[wfv] Walk-forward validation: n={n}, horizon={horizon}, model={model_type}")

  for (t in seq(min_train, n - horizon)) {
    train  <- x[1:t]
    actual <- x[(t + 1):(t + horizon)]

    model  <- tryCatch({
      ts_    <- stats::ts(na.omit(train))
      switch(model_type,
        arima = forecast::auto.arima(ts_, stepwise = TRUE, approximation = TRUE),
        ets   = forecast::ets(ts_, ic = "aicc"),
        stop(glue("[wfv] Unknown model: {model_type}"))
      )
    }, error = function(e) NULL)

    if (is.null(model)) next

    fc <- tryCatch(
      as.numeric(forecast::forecast(model, h = horizon)$mean),
      error = function(e) rep(NA_real_, horizon)
    )

    results[[t - min_train + 1]] <- tibble::tibble(
      t            = t,
      h            = seq_len(horizon),
      actual       = actual,
      forecast     = fc,
      error        = actual - fc,
      squared_error= (actual - fc)^2
    )
  }

  combined <- dplyr::bind_rows(purrr::compact(results))
  log_info("[wfv] Completed {nrow(combined)} forecast-actual pairs")
  combined
}

#' Compute backtesting performance summary
#'
#' @param wfv_result tibble from walk_forward_validation()
#' @return tibble with MAE, RMSE, MAPE, hit_rate for each horizon h
#' @export
wfv_summary <- function(wfv_result) {
  wfv_result |>
    dplyr::group_by(h) |>
    dplyr::summarise(
      n_forecasts  = dplyr::n(),
      mae          = mean(abs(error), na.rm = TRUE),
      rmse         = sqrt(mean(squared_error, na.rm = TRUE)),
      mape         = mean(abs(error / actual) * 100, na.rm = TRUE),
      bias         = mean(error, na.rm = TRUE),
      hit_rate_dir = mean(sign(actual) == sign(forecast), na.rm = TRUE),
      .groups      = "drop"
    )
}

# =============================================================================
# SECTION 3: FORECAST VISUALISATION
# =============================================================================

#' Plot a forecast with historical context and confidence fan
#'
#' @param x_hist    Historical numeric vector
#' @param dates_hist Historical date vector
#' @param fc_df     Forecast tibble with columns: h, forecast, lo_80, hi_80, lo_95, hi_95
#' @param dates_fc  Date vector for forecast horizon
#' @param title     Chart title
#' @return ggplot object
#' @export
chart_forecast <- function(x_hist, dates_hist, fc_df,
                            dates_fc = NULL, title = "Forecast") {
  if (is.null(dates_fc)) {
    last_date <- max(dates_hist)
    dates_fc  <- last_date + seq_len(nrow(fc_df))
  }

  hist_df <- tibble::tibble(date = dates_hist, value = x_hist)
  fc_plot <- tibble::tibble(date = dates_fc, forecast = fc_df$forecast,
                             lo_80 = fc_df$lo_80, hi_80 = fc_df$hi_80,
                             lo_95 = fc_df$lo_95, hi_95 = fc_df$hi_95)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fc_plot,
      aes(x = date, ymin = lo_95, ymax = hi_95),
      fill = PALETTE$accent, alpha = 0.15
    ) +
    ggplot2::geom_ribbon(
      data = fc_plot,
      aes(x = date, ymin = lo_80, ymax = hi_80),
      fill = PALETTE$accent, alpha = 0.25
    ) +
    ggplot2::geom_line(
      data = hist_df,
      aes(x = date, y = value),
      colour = PALETTE$primary, linewidth = 0.8
    ) +
    ggplot2::geom_line(
      data = fc_plot,
      aes(x = date, y = forecast),
      colour = PALETTE$secondary, linewidth = 1, linetype = "solid"
    ) +
    ggplot2::geom_vline(
      xintercept = max(dates_hist),
      linetype = "dotted", colour = PALETTE$text_light, linewidth = 0.5
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = "Shaded bands: 80% and 95% prediction intervals",
      x = NULL, y = NULL,
      caption = "Source: Quant Research Platform — ARIMA/ETS Ensemble"
    ) +
    theme_quant()
}
