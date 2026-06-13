# =============================================================================
# Shiny Dashboard — Server v2.0
# Quant Research & Economic Intelligence Platform
#
# Changes in v2.0:
#   - Country Explorer tab wired to world_bank.R (root cause fix)
#   - All API error messages are user-friendly with recovery instructions
#   - Loading states use withProgress() throughout
#   - Hardcoded CAPM rf_daily now uses the slider value
#   - QP solver failures wrapped in tryCatch; falls back to equal weight
#   - select-all / clear links for the country checkbox group
# =============================================================================

library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(glue)
library(here)

# Load platform modules
source(here::here("R", "utilities", "config.R"))
source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "helpers.R"))
source(here::here("R", "ingestion", "yahoo_finance.R"))
source(here::here("R", "ingestion", "fred_api.R"))
source(here::here("R", "ingestion", "world_bank.R"))
source(here::here("R", "cleaning", "clean_prices.R"))
source(here::here("R", "cleaning", "validate.R"))
source(here::here("R", "transformations", "portfolio_analytics.R"))
source(here::here("R", "econometrics", "time_series.R"))
source(here::here("R", "simulations", "monte_carlo.R"))
source(here::here("R", "forecasting", "forecast_pipeline.R"))
source(here::here("R", "visualization", "theme.R"))
source(here::here("R", "visualization", "charts.R"))

cfg <- get_config()
init_logger(cfg)
set_quant_theme()

# ---- Plotly dark theme helper -----------------------------------------------
plotly_theme <- function(p) {
  plotly::layout(p,
    paper_bgcolor = "#080E18",
    plot_bgcolor  = "#111C2C",
    font          = list(color = "#E2EAF6", family = "Inter, sans-serif", size = 12),
    xaxis         = list(gridcolor = "#1A2B42", zerolinecolor = "#1A2B42",
                         linecolor = "#243650"),
    yaxis         = list(gridcolor = "#1A2B42", zerolinecolor = "#1A2B42",
                         linecolor = "#243650"),
    legend        = list(bgcolor = "rgba(0,0,0,0)", font = list(size = 11))
  )
}

# ---- Formatting helpers -----------------------------------------------------
fmt_pct    <- function(x, digits = 2)  paste0(round(x * 100, digits), "%")
fmt_num    <- function(x, digits = 2)  format(round(x, digits), big.mark = ",")
fmt_dollar <- function(x, digits = 2)  paste0("$", format(round(x, digits), big.mark = ","))

# ---- User-friendly error notification ---------------------------------------
notify_error <- function(context, e) {
  msg <- conditionMessage(e)
  # Tailor the message for common failure modes
  friendly <- dplyr::case_when(
    grepl("yahoo|getSymbols|curl", msg, ignore.case = TRUE) ~
      paste0("Market data unavailable (", context, "). Yahoo Finance may be blocked.",
             " Retrying automatically. If this persists, check your network or try later."),
    grepl("fredr|FRED|rate.limit|401|403", msg, ignore.case = TRUE) ~
      paste0("FRED API error (", context, "). Set FRED_API_KEY in .Renviron",
             " for reliable access: https://fred.stlouisfed.org/docs/api/api_key.html"),
    grepl("WDI|worldbank|timeout|internet", msg, ignore.case = TRUE) ~
      paste0("World Bank API unavailable (", context, "). Check your internet connection."),
    TRUE ~ paste0(context, " failed: ", msg)
  )
  showNotification(friendly, type = "error", duration = 10)
  log_error("[server] {friendly}")
}

# ---- WB indicator axis label helper -----------------------------------------
wb_label <- function(key) {
  lbl <- WB_INDICATOR_LABELS[key]
  if (is.na(lbl)) key else lbl
}

# =============================================================================
server <- function(input, output, session) {

  # ---- Reactive containers --------------------------------------------------
  rv <- reactiveValues(
    prices_wide   = NULL,
    returns_wide  = NULL,
    port_result   = NULL,
    capm_result   = NULL,
    capm_ret_wide = NULL,
    mc_paths      = NULL,
    mc_summary    = NULL,
    macro_df      = NULL,
    fc_result     = NULL,
    wfv_result    = NULL,
    wb_data       = NULL   # World Bank country data
  )

  # ==========================================================================
  # TAB 1: PORTFOLIO ANALYTICS
  # ==========================================================================

  observeEvent(input$run_portfolio, {
    req(input$tickers, length(input$tickers) >= 1)

    withProgress(message = "Fetching market data...", value = 0.15, {

      tickers_all <- unique(c(input$tickers, input$benchmark))

      prices <- tryCatch(
        fetch_adjusted_prices(tickers_all,
                              from = input$date_range[1],
                              to   = input$date_range[2],
                              cfg  = cfg),
        error = function(e) { notify_error("Portfolio fetch", e); NULL }
      )

      if (is.null(prices) || nrow(prices) == 0L) {
        showNotification(
          "No price data returned. Check tickers and date range, or try a shorter period.",
          type = "warning", duration = 8
        )
        return()
      }

      incProgress(0.35, message = "Computing returns...")

      prices  <- fill_price_gaps(prices)
      tickers <- intersect(input$tickers, names(prices))
      if (length(tickers) == 0L) {
        showNotification("None of the selected tickers returned data.", type = "warning")
        return()
      }
      prices <- prices[, c("date", tickers)]

      ret_wide <- prices |>
        dplyr::mutate(dplyr::across(-date, ~ c(NA_real_, log_returns(.x)))) |>
        dplyr::slice(-1)

      rv$prices_wide  <- prices
      rv$returns_wide <- ret_wide

      incProgress(0.35, message = "Optimising portfolio...")

      rf    <- input$rf_rate / 100
      mu_v  <- colMeans(ret_wide[, tickers], na.rm = TRUE) * 252
      Sigma <- cov(ret_wide[, tickers], use = "complete.obs") * 252

      eq_w <- setNames(rep(1 / length(tickers), length(tickers)), tickers)

      weights <- tryCatch(
        switch(input$opt_method,
          equal      = eq_w,
          min_var    = min_variance_portfolio(Sigma),
          max_sharpe = max_sharpe_portfolio(mu_v, Sigma, rf = rf),
          custom     = eq_w,
          eq_w
        ),
        error = function(e) {
          showNotification(
            paste("Portfolio optimisation failed (ill-conditioned covariance) — using equal weights.", conditionMessage(e)),
            type = "warning", duration = 7
          )
          eq_w
        }
      )

      if (is.null(weights)) weights <- eq_w

      rv$port_result <- portfolio_statistics(ret_wide, weights, rf = rf)

      incProgress(0.15, message = "Done.")
      showNotification("Portfolio analysis complete.", type = "message", duration = 3)
    })
  })

  # KPI outputs
  output$kpi_total_ret <- renderText({ req(rv$port_result); fmt_pct(rv$port_result$stats["total_return"]) })
  output$kpi_ann_ret   <- renderText({ req(rv$port_result); fmt_pct(rv$port_result$stats["ann_return"]) })
  output$kpi_vol       <- renderText({ req(rv$port_result); fmt_pct(rv$port_result$stats["ann_volatility"]) })
  output$kpi_sharpe    <- renderText({ req(rv$port_result); fmt_num(rv$port_result$stats["sharpe_ratio"]) })
  output$kpi_mdd       <- renderText({ req(rv$port_result); fmt_pct(rv$port_result$stats["max_drawdown"]) })
  output$kpi_var95     <- renderText({ req(rv$port_result); fmt_pct(rv$port_result$stats["var_95_daily"]) })

  output$plot_nav <- renderPlotly({
    req(rv$prices_wide, rv$port_result)
    w   <- rv$port_result$weights
    nav <- rv$port_result$nav
    df  <- tibble::tibble(date = rv$port_result$dates, Portfolio = nav)
    for (t in names(w)) {
      px <- rv$prices_wide[[t]]
      df[[t]] <- px / px[1] * 100
    }
    df_long <- tidyr::pivot_longer(df, -date, names_to = "series", values_to = "nav")
    plotly::plot_ly(df_long, x = ~date, y = ~nav, color = ~series,
                    type = "scatter", mode = "lines",
                    line = list(width = 1.6)) |>
      plotly::layout(yaxis = list(title = "Index (100 = start)"), xaxis = list(title = "")) |>
      plotly_theme()
  })

  output$plot_weights <- renderPlotly({
    req(rv$port_result)
    w  <- rv$port_result$weights
    df <- tibble::tibble(asset = names(w), weight = as.numeric(w))
    plotly::plot_ly(df, labels = ~asset, values = ~weight, type = "pie", hole = 0.52,
                    marker = list(colors = PALETTE$series,
                                  line   = list(color = "#080E18", width = 2))) |>
      plotly::layout(showlegend = TRUE) |>
      plotly_theme()
  })

  output$plot_drawdown <- renderPlotly({
    req(rv$port_result)
    dd <- drawdown_series(rv$port_result$nav) * 100
    df <- tibble::tibble(date = rv$port_result$dates, drawdown = dd)
    plotly::plot_ly(df, x = ~date, y = ~drawdown, type = "scatter",
                    mode = "lines", fill = "tozeroy",
                    line     = list(color = "#EF4444", width = 1.2),
                    fillcolor = "rgba(239,68,68,0.2)") |>
      plotly::layout(yaxis = list(title = "Drawdown (%)")) |>
      plotly_theme()
  })

  output$plot_roll_vol <- renderPlotly({
    req(rv$port_result)
    rvol <- roll_apply(rv$port_result$returns, 63, sd) * sqrt(252) * 100
    df   <- tibble::tibble(date = rv$port_result$dates, rolling_vol = rvol)
    plotly::plot_ly(df, x = ~date, y = ~rolling_vol, type = "scatter",
                    mode = "lines", line = list(color = "#06B6D4", width = 1.5)) |>
      plotly::layout(yaxis = list(title = "Annualised Vol (%)")) |>
      plotly_theme()
  })

  output$tbl_stats <- DT::renderDataTable({
    req(rv$returns_wide, rv$prices_wide)
    tickers <- setdiff(names(rv$returns_wide), "date")
    purrr::map_dfr(tickers, function(t) {
      s <- return_statistics(rv$returns_wide[[t]], rv$prices_wide[[t]],
                             rf = input$rf_rate / 100)
      tibble::tibble(
        Ticker        = t,
        `Ann Return`  = fmt_pct(s["ann_return"]),
        `Ann Vol`     = fmt_pct(s["ann_volatility"]),
        `Sharpe`      = fmt_num(s["sharpe_ratio"]),
        `Sortino`     = fmt_num(s["sortino_ratio"]),
        `Max DD`      = fmt_pct(s["max_drawdown"]),
        `Skewness`    = fmt_num(s["skewness"]),
        `Kurtosis`    = fmt_num(s["excess_kurtosis"]),
        `VaR 95%`     = fmt_pct(s["var_95_daily"])
      )
    }) |>
      DT::datatable(options = list(pageLength = 15, dom = "t"),
                    rownames = FALSE, class = "dt-table")
  })

  # ==========================================================================
  # TAB 2: CAPM / FACTOR ANALYSIS
  # ==========================================================================

  observeEvent(input$run_capm, {
    req(input$capm_tickers, length(input$capm_tickers) >= 1)

    withProgress(message = "Fetching CAPM data...", value = 0.2, {
      tickers_all <- unique(c(input$capm_tickers, input$capm_benchmark))
      prices <- tryCatch(
        fetch_adjusted_prices(tickers_all, from = input$capm_dates[1],
                              to = input$capm_dates[2], cfg = cfg),
        error = function(e) { notify_error("CAPM fetch", e); NULL }
      )
      if (is.null(prices)) return()

      incProgress(0.4, message = "Computing CAPM regressions...")

      prices   <- fill_price_gaps(prices)
      ret_wide <- prices |>
        dplyr::mutate(dplyr::across(-date, ~ c(NA_real_, log_returns(.x)))) |>
        dplyr::slice(-1)

      rv$capm_ret_wide <- ret_wide
      rv$capm_result   <- capm_multi(ret_wide,
                                     benchmark_col = input$capm_benchmark,
                                     rf_annual     = input$rf_rate / 100)
      showNotification("CAPM analysis complete.", type = "message", duration = 3)
    })
  })

  output$tbl_capm <- DT::renderDataTable({
    req(rv$capm_result)
    rv$capm_result |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))) |>
      DT::datatable(options = list(pageLength = 15, dom = "t"),
                    rownames = FALSE, class = "dt-table")
  })

  output$plot_scl <- renderPlotly({
    req(rv$capm_ret_wide, rv$capm_result)
    ret    <- rv$capm_ret_wide
    res    <- rv$capm_result
    bm     <- input$capm_benchmark
    rf_d   <- input$rf_rate / 100 / 252
    cols   <- PALETTE$series

    mkt_xs <- ret[[bm]] - rf_d
    x_rng  <- range(mkt_xs, na.rm = TRUE)
    x_line <- seq(x_rng[1], x_rng[2], length.out = 100)

    p <- plotly::plot_ly()
    for (i in seq_along(res$asset)) {
      a       <- res$asset[i]
      col     <- cols[((i - 1) %% length(cols)) + 1]
      ast_xs  <- ret[[a]] - rf_d
      alpha_d <- res$alpha[i] / 252
      y_line  <- alpha_d + res$beta[i] * x_line

      p <- p |>
        plotly::add_trace(x = mkt_xs, y = ast_xs, type = "scatter",
                          mode = "markers", name = glue("{a} (data)"),
                          marker = list(color = col, size = 3, opacity = 0.25),
                          showlegend = FALSE) |>
        plotly::add_trace(x = x_line, y = y_line, type = "scatter",
                          mode = "lines", name = a,
                          line = list(color = col, width = 2))
    }
    p |>
      plotly::layout(
        xaxis  = list(title = glue("{bm} Excess Return")),
        yaxis  = list(title = "Asset Excess Return"),
        legend = list(orientation = "h")
      ) |>
      plotly_theme()
  })

  output$plot_alpha_beta <- renderPlotly({
    req(rv$capm_result)
    df <- rv$capm_result
    plotly::plot_ly(df,
      x    = ~beta,
      y    = ~round(alpha * 100, 2),
      text = ~asset,
      type = "scatter",
      mode = "markers+text",
      textposition = "top center",
      marker = list(
        size       = ~pmin(r_squared * 40 + 8, 30),
        color      = ~r_squared,
        colorscale = list(c(0, "#0D1520"), c(0.5, "#1968E3"), c(1, "#0CB886")),
        showscale  = TRUE,
        colorbar   = list(title = "R²")
      )
    ) |>
      plotly::add_hline(y = 0, line = list(dash = "dot", color = "#4E6280", width = 1)) |>
      plotly::add_vline(x = 1, line = list(dash = "dot", color = "#4E6280", width = 1)) |>
      plotly::layout(
        xaxis = list(title = "Beta (Systematic Risk)"),
        yaxis = list(title = "Annualised Alpha (%)")
      ) |>
      plotly_theme()
  })

  # ==========================================================================
  # TAB 3: MONTE CARLO
  # ==========================================================================

  observeEvent(input$run_mc, {
    withProgress(message = "Running Monte Carlo simulation...", value = 0.1, {
      mu    <- input$mc_drift / 100
      sigma <- input$mc_vol   / 100

      if (input$mc_model == "gbm") {
        paths <- simulate_gbm(
          S0      = input$mc_S0,
          mu      = mu,
          sigma   = sigma,
          T_days  = input$mc_horizon,
          n_paths = input$mc_paths
        )
      } else {
        px <- tryCatch(
          fetch_yahoo(input$mc_ticker, cfg = cfg),
          error = function(e) { notify_error("MC historical fetch", e); NULL }
        )
        if (is.null(px)) return()
        ret   <- log_returns(px$adjusted)
        paths <- simulate_historical_bootstrap(ret, S0 = input$mc_S0,
                                               T_days  = input$mc_horizon,
                                               n_paths = input$mc_paths)
      }

      incProgress(0.7, message = "Summarising results...")
      rv$mc_paths   <- paths
      rv$mc_summary <- summarise_mc_paths(paths, S0 = input$mc_S0, rf = input$rf_rate / 100)
      showNotification("Simulation complete.", type = "message", duration = 3)
    })
  })

  output$mc_mean_terminal <- renderText({ req(rv$mc_summary); fmt_dollar(rv$mc_summary$mean_terminal) })
  output$mc_p75           <- renderText({ req(rv$mc_summary); fmt_dollar(rv$mc_summary$percentiles["75%"]) })
  output$mc_p25           <- renderText({ req(rv$mc_summary); fmt_dollar(rv$mc_summary$percentiles["25%"]) })
  output$mc_var95         <- renderText({ req(rv$mc_summary); fmt_pct(rv$mc_summary$var_95) })
  output$mc_es95          <- renderText({ req(rv$mc_summary); fmt_pct(rv$mc_summary$es_95) })
  output$mc_prob_loss     <- renderText({ req(rv$mc_summary); fmt_pct(rv$mc_summary$prob_loss) })

  output$plot_mc_paths <- renderPlotly({
    req(rv$mc_paths)
    paths  <- rv$mc_paths
    T_days <- nrow(paths) - 1

    pct <- apply(paths, 1, quantile, probs = c(0.05, 0.25, 0.50, 0.75, 0.95)) |>
      t() |> as.data.frame()
    names(pct) <- c("p05","p25","p50","p75","p95")
    pct$day <- 0:T_days

    plotly::plot_ly(pct, x = ~day) |>
      plotly::add_ribbons(ymin = ~p05, ymax = ~p95,
                          fillcolor = "rgba(25,104,227,0.10)", line = list(width = 0),
                          name = "5th–95th %ile") |>
      plotly::add_ribbons(ymin = ~p25, ymax = ~p75,
                          fillcolor = "rgba(25,104,227,0.22)", line = list(width = 0),
                          name = "25th–75th %ile") |>
      plotly::add_lines(y = ~p50, line = list(color = "#F0B429", width = 2), name = "Median") |>
      plotly::layout(xaxis = list(title = "Trading Days"),
                     yaxis = list(title = "Simulated Price ($)")) |>
      plotly_theme()
  })

  output$plot_mc_dist <- renderPlotly({
    req(rv$mc_paths, rv$mc_summary)
    terminal <- rv$mc_paths[nrow(rv$mc_paths), ]
    returns  <- (terminal / input$mc_S0 - 1) * 100
    var95    <- rv$mc_summary$var_95 * 100

    plotly::plot_ly(
      x      = returns,
      type   = "histogram",
      nbinsx = 80,
      marker = list(
        color = ifelse(returns < 0, "rgba(239,68,68,0.75)", "rgba(12,184,134,0.75)"),
        line  = list(width = 0)
      )
    ) |>
      plotly::add_vline(x = 0,     line = list(dash = "dot",  color = "#E2EAF6", width = 1)) |>
      plotly::add_vline(x = -var95, line = list(dash = "dash", color = "#F0B429", width = 1.5),
                        annotation_text = "VaR 95%",
                        annotation_font = list(color = "#F0B429")) |>
      plotly::layout(xaxis = list(title = "Terminal Return (%)"),
                     yaxis = list(title = "Count")) |>
      plotly_theme()
  })

  # ==========================================================================
  # TAB 4: MACRO EXPLORER
  # ==========================================================================

  observeEvent(input$run_macro, {
    req(input$macro_series, length(input$macro_series) >= 1)

    withProgress(message = "Loading FRED data...", value = 0.3, {
      df <- tryCatch(
        fetch_fred_multi(series_keys = input$macro_series,
                         from = input$macro_dates[1],
                         to   = input$macro_dates[2],
                         cfg  = cfg),
        error = function(e) { notify_error("FRED fetch", e); NULL }
      )
      if (is.null(df)) return()
      incProgress(0.6)
      rv$macro_df <- df
      showNotification(
        glue("Loaded {dplyr::n_distinct(df$series_id)} macro series, {nrow(df)} observations."),
        type = "message", duration = 4
      )
    })
  })

  output$plot_macro_panel <- renderPlot({
    req(rv$macro_df)
    chart_macro_panel(rv$macro_df, series = input$macro_series)
  }, bg = "#080E18")

  output$plot_yield_curve <- renderPlotly({
    yc <- tryCatch(fetch_yield_curve(cfg = cfg), error = function(e) NULL)
    if (is.null(yc) || nrow(yc) == 0) {
      return(plotly::plot_ly() |>
        plotly::layout(
          annotations = list(text = "Yield curve unavailable — check FRED API key",
                             xref = "paper", yref = "paper", x = .5, y = .5,
                             showarrow = FALSE, font = list(color = "#8A9BB8"))
        ) |> plotly_theme())
    }
    plotly::plot_ly(yc, x = ~maturity_years, y = ~yield,
                    type = "scatter", mode = "lines+markers",
                    line   = list(color = "#F0B429", width = 2.5),
                    marker = list(color = "#F0B429", size = 8)) |>
      plotly::layout(xaxis = list(title = "Maturity (years)"),
                     yaxis = list(title = "Yield (%)")) |>
      plotly_theme()
  })

  output$plot_macro_corr <- renderPlot({
    req(rv$macro_df)
    df_wide <- rv$macro_df |>
      dplyr::select(date, series_id, value) |>
      tidyr::pivot_wider(names_from = series_id, values_from = value)
    if (ncol(df_wide) <= 2) return(NULL)
    chart_correlation_matrix(df_wide, title = "Macro Indicator Correlation")
  }, bg = "#080E18")

  # ==========================================================================
  # TAB 5: FORECASTING
  # ==========================================================================

  observeEvent(input$run_forecast, {
    withProgress(message = "Fetching data & fitting model...", value = 0.2, {
      x <- tryCatch({
        if (input$fc_series_type == "price") {
          px <- fetch_yahoo(input$fc_ticker, cfg = cfg)
          if (is.null(px)) stop("Price data unavailable")
          log(px$adjusted)
        } else {
          df <- fetch_fred(input$fc_fred_series, cfg = cfg)
          if (is.null(df)) stop("FRED series unavailable")
          df$value
        }
      }, error = function(e) { notify_error("Forecast data", e); NULL })

      if (is.null(x)) return()

      incProgress(0.4, message = "Fitting ARIMA model...")
      fit <- fit_arima(x, max_p = 5, max_q = 5)
      if (is.null(fit)) {
        showNotification("ARIMA model fitting failed — series may be too short or irregular.",
                         type = "error")
        return()
      }

      fc <- forecast_arima(fit, horizon = input$fc_horizon)
      rv$fc_result <- list(fit = fit, fc = fc, x = x)

      incProgress(0.3, message = "Walk-forward validation...")
      if (input$fc_backtest) {
        rv$wfv_result <- tryCatch(
          walk_forward_validation(x, model_type = input$fc_model_type,
                                  horizon = 1L, min_train = input$fc_min_train),
          error = function(e) {
            showNotification(paste("Walk-forward validation failed:", conditionMessage(e)),
                             type = "warning")
            NULL
          }
        )
      }
      showNotification("Forecast complete.", type = "message", duration = 3)
    })
  })

  output$fc_model_label <- renderText({
    req(rv$fc_result)
    ord <- rv$fc_result$fit$order
    glue("ARIMA({ord[1]},{ord[2]},{ord[3]})")
  })
  output$fc_aic  <- renderText({ req(rv$fc_result); fmt_num(rv$fc_result$fit$diagnostics$aicc) })
  output$fc_rmse <- renderText({
    req(rv$fc_result)
    fmt_num(sqrt(mean((rv$fc_result$x - rv$fc_result$fit$fitted)^2, na.rm = TRUE)))
  })
  output$fc_mae  <- renderText({
    req(rv$fc_result)
    fmt_num(mean(abs(rv$fc_result$fit$residuals), na.rm = TRUE))
  })
  output$fc_mape <- renderText({
    req(rv$fc_result)
    x <- rv$fc_result$x; f <- rv$fc_result$fit$fitted
    fmt_num(mean(abs((x - f) / x) * 100, na.rm = TRUE))
  })
  output$fc_ljung <- renderText({
    req(rv$fc_result)
    fmt_num(rv$fc_result$fit$ljung_box$p.value)
  })

  output$plot_forecast <- renderPlot({
    req(rv$fc_result)
    fc <- rv$fc_result$fc
    x  <- rv$fc_result$x
    chart_forecast(
      x_hist     = tail(x, 200),
      dates_hist = seq(Sys.Date() - 199, Sys.Date(), by = "day")[seq_len(min(200, length(x)))],
      fc_df      = fc,
      title      = glue("{input$fc_series_type} Forecast — {input$fc_model_type}")
    )
  }, bg = "#080E18")

  output$plot_wfv <- renderPlot({
    req(rv$wfv_result, nrow(rv$wfv_result) > 0)
    wfv_summ <- wfv_summary(rv$wfv_result)
    tidyr::pivot_longer(wfv_summ, c(mae, rmse), names_to = "metric", values_to = "value") |>
      ggplot2::ggplot(ggplot2::aes(x = factor(h), y = value, fill = metric)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.85) +
      ggplot2::scale_fill_manual(
        values = c(mae = PALETTE$accent, rmse = PALETTE$gold),
        labels = c(mae = "MAE", rmse = "RMSE")
      ) +
      ggplot2::labs(
        title   = "Walk-Forward Validation — Forecast Error by Horizon",
        x = "Horizon (h)", y = "Error", fill = NULL,
        caption = "MAE = Mean Absolute Error  |  RMSE = Root Mean Squared Error"
      ) +
      theme_quant()
  }, bg = "#080E18")

  # ==========================================================================
  # TAB 6: COUNTRY EXPLORER
  # ==========================================================================

  # Select-all / clear buttons
  observeEvent(input$wb_select_all,   {
    updateCheckboxGroupInput(session, "wb_countries", selected = G20_COUNTRIES)
  })
  observeEvent(input$wb_deselect_all, {
    updateCheckboxGroupInput(session, "wb_countries", selected = character(0))
  })

  observeEvent(input$run_wb, {
    req(length(input$wb_countries) >= 1)

    withProgress(message = "Fetching World Bank data...", value = 0.2, {
      wb <- tryCatch(
        fetch_world_bank(
          indicators = WB_INDICATORS[c(
            "gdp_per_capita_usd", "gdp_growth_pct", "inflation_cpi",
            "population", "life_expectancy", "internet_penetration",
            "govt_debt_gdp", "gini_index"
          )],
          countries  = input$wb_countries,
          start_year = 2000L,
          end_year   = as.integer(format(Sys.Date(), "%Y")) - 1L,
          cfg        = cfg
        ),
        error = function(e) { notify_error("World Bank fetch", e); NULL }
      )
      incProgress(0.8)

      if (is.null(wb) || nrow(wb) == 0L) {
        showNotification(
          "World Bank returned no data. Check your internet connection and selected countries.",
          type = "warning", duration = 8
        )
        return()
      }

      rv$wb_data <- wb
      n_ctry <- dplyr::n_distinct(wb$country)
      n_ind  <- dplyr::n_distinct(wb$indicator)
      showNotification(
        glue("Loaded {nrow(wb)} observations: {n_ctry} countries, {n_ind} indicators."),
        type = "message", duration = 4
      )
    })
  })

  # KPI summary
  .wb_slice <- reactive({
    req(rv$wb_data, input$wb_indicator, input$wb_year)
    rv$wb_data |>
      dplyr::filter(indicator == input$wb_indicator,
                    year      == input$wb_year,
                    !is.na(value))
  })

  output$wb_kpi_max_country <- renderText({
    df <- .wb_slice()
    if (nrow(df) == 0) return("—")
    row <- df |> dplyr::slice_max(value, n = 1)
    paste0(G20_FLAGS[row$iso2c], " ", row$country, "\n", fmt_num(row$value))
  })
  output$wb_kpi_min_country <- renderText({
    df <- .wb_slice()
    if (nrow(df) == 0) return("—")
    row <- df |> dplyr::slice_min(value, n = 1)
    paste0(G20_FLAGS[row$iso2c], " ", row$country, "\n", fmt_num(row$value))
  })
  output$wb_kpi_median <- renderText({
    df <- .wb_slice()
    if (nrow(df) == 0) return("—")
    fmt_num(median(df$value, na.rm = TRUE))
  })
  output$wb_kpi_us <- renderText({
    df <- .wb_slice()
    us <- df[df$iso2c == "US", ]
    if (nrow(us) == 0) return("N/A")
    fmt_num(us$value[1])
  })

  # Bar chart: country comparison for selected indicator + year
  output$plot_wb_bar <- renderPlotly({
    df <- .wb_slice()

    if (nrow(df) == 0) {
      return(plotly::plot_ly() |>
        plotly::layout(annotations = list(
          text = glue("No data for {wb_label(input$wb_indicator)} in {input$wb_year}"),
          xref = "paper", yref = "paper", x = .5, y = .5, showarrow = FALSE,
          font = list(color = "#8A9BB8", size = 13)
        )) |> plotly_theme())
    }

    df <- dplyr::arrange(df, value)
    colours <- dplyr::case_when(
      df$iso2c == "US" ~ "#F0B429",
      df$value == max(df$value) ~ "#0CB886",
      df$value == min(df$value) ~ "#EF4444",
      TRUE ~ "#1968E3"
    )

    plotly::plot_ly(
      df,
      x          = ~value,
      y          = ~reorder(country, value),
      type       = "bar",
      orientation = "h",
      marker     = list(color = colours),
      text       = ~paste0(country, ": ", fmt_num(value)),
      hovertemplate = "%{text}<extra></extra>"
    ) |>
      plotly::layout(
        xaxis = list(title = wb_label(input$wb_indicator)),
        yaxis = list(title = ""),
        margin = list(l = 120)
      ) |>
      plotly_theme()
  })

  # Bubble scatter: GDP per capita vs Life expectancy, sized by population
  output$plot_wb_bubble <- renderPlotly({
    req(rv$wb_data, input$wb_year)

    yr <- input$wb_year
    wide <- rv$wb_data |>
      dplyr::filter(year == yr,
                    indicator %in% c("gdp_per_capita_usd", "life_expectancy", "population")) |>
      dplyr::select(country, iso2c, indicator, value) |>
      tidyr::pivot_wider(names_from = indicator, values_from = value) |>
      dplyr::filter(!is.na(gdp_per_capita_usd), !is.na(life_expectancy))

    if (nrow(wide) == 0) return(plotly::plot_ly() |> plotly_theme())

    pop_size <- if ("population" %in% names(wide)) sqrt(wide$population / 1e6) * 3 else 12

    plotly::plot_ly(
      wide,
      x    = ~gdp_per_capita_usd,
      y    = ~life_expectancy,
      size = pop_size,
      text = ~paste0(
        "<b>", country, "</b><br>",
        "GDP/capita: $", formatC(gdp_per_capita_usd, format = "f", digits = 0, big.mark = ","), "<br>",
        "Life exp.: ", round(life_expectancy, 1), " yrs"
      ),
      type = "scatter",
      mode = "markers+text",
      textposition = "top center",
      textfont = list(size = 9, color = "#8A9BB8"),
      hovertemplate = "%{text}<extra></extra>",
      marker = list(
        opacity   = 0.82,
        sizemode  = "diameter",
        color     = PALETTE$series,
        line      = list(width = 1, color = "rgba(255,255,255,0.15)")
      )
    ) |>
      plotly::layout(
        xaxis = list(title = "GDP per Capita (USD)", type = "log"),
        yaxis = list(title = "Life Expectancy (years)")
      ) |>
      plotly_theme()
  })

  # Time-series trend for selected indicator
  output$plot_wb_trend <- renderPlotly({
    req(rv$wb_data, input$wb_indicator)

    df <- rv$wb_data |>
      dplyr::filter(indicator == input$wb_indicator, !is.na(value)) |>
      dplyr::arrange(iso2c, year)

    if (nrow(df) == 0) return(plotly::plot_ly() |> plotly_theme())

    plotly::plot_ly(df, x = ~year, y = ~value, color = ~country,
                    type = "scatter", mode = "lines",
                    line = list(width = 1.8)) |>
      plotly::layout(
        xaxis  = list(title = "Year", dtick = 5),
        yaxis  = list(title = wb_label(input$wb_indicator)),
        legend = list(orientation = "h", y = -0.25)
      ) |>
      plotly_theme()
  })

  # Country data table
  output$tbl_wb <- DT::renderDataTable({
    req(rv$wb_data, input$wb_indicator)
    rv$wb_data |>
      dplyr::filter(indicator == input$wb_indicator, !is.na(value)) |>
      dplyr::select(Country = country, Year = year, Value = value) |>
      dplyr::arrange(Year, Country) |>
      dplyr::mutate(Value = round(Value, 2)) |>
      DT::datatable(
        options  = list(pageLength = 15, dom = "ftip", scrollY = "220px"),
        rownames = FALSE,
        class    = "dt-table"
      )
  })

  # ==========================================================================
  # TAB 7: REPORT GENERATOR
  # ==========================================================================

  output$report_preview <- renderUI({
    title  <- if (nchar(trimws(input$report_title))  > 0) input$report_title  else "Quant Research Report"
    author <- if (nchar(trimws(input$report_author)) > 0) input$report_author else "Quant Research Platform"
    HTML(glue(
      '<div style="padding:24px;color:#E2EAF6;font-family:monospace;">',
      '<div style="color:#F0B429;font-size:1.1rem;font-weight:700;margin-bottom:14px;">{title}</div>',
      '<table style="color:#E2EAF6;font-size:.85rem;border-collapse:collapse;width:100%;">',
      '<tr><td style="color:#8A9BB8;padding:4px 16px 4px 0;">Template</td><td>{input$report_type}</td></tr>',
      '<tr><td style="color:#8A9BB8;padding:4px 16px 4px 0;">Format</td><td>{toupper(input$report_format)}</td></tr>',
      '<tr><td style="color:#8A9BB8;padding:4px 16px 4px 0;">Author</td><td>{author}</td></tr>',
      '<tr><td style="color:#8A9BB8;padding:4px 16px 4px 0;">Charts</td><td>{input$report_include_charts}</td></tr>',
      '<tr><td style="color:#8A9BB8;padding:4px 16px 4px 0;">Tables</td><td>{input$report_include_tables}</td></tr>',
      '</table>',
      '<p style="color:#4E6280;margin-top:20px;font-size:.8rem;">',
      'Click <strong style="color:#E2EAF6;">Download Report</strong> to render and download.</p>',
      '</div>'
    ))
  })

  observeEvent(input$gen_report, {
    showNotification("Report configured — click Download Report to render.",
                     type = "message", duration = 4)
  })

  output$dl_report <- downloadHandler(
    filename = function() {
      ext <- if (input$report_format == "html") ".html" else ".pdf"
      paste0("quant_report_", format(Sys.Date(), "%Y%m%d"), ext)
    },
    content = function(file) {
      template <- here::here("reports", "automated",
                              glue("{input$report_type}_report.Rmd"))
      if (!file.exists(template)) {
        writeLines("Report template not found.", file)
        return()
      }
      rmarkdown::render(
        input         = template,
        output_file   = file,
        output_format = if (input$report_format == "html") "html_document" else "pdf_document",
        params        = list(cfg    = cfg,
                             title  = input$report_title,
                             author = input$report_author)
      )
    }
  )

  output$dl_mc_paths <- downloadHandler(
    filename = function() paste0("mc_paths_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$mc_paths)
      readr::write_csv(as.data.frame(rv$mc_paths), file)
    }
  )
}
