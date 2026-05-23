# =============================================================================
# Shiny Dashboard — Server
# Quant Research & Economic Intelligence Platform
#
# Architecture: reactive value containers isolate state, observers trigger
# expensive computations only when inputs change, all data flows are
# explicitly typed and validated before entering analytics modules.
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

# ---- Plotly theme helper ----------------------------------------------------
plotly_theme <- function(p) {
  plotly::layout(p,
    paper_bgcolor = "#0D1117",
    plot_bgcolor  = "#161B22",
    font          = list(color = "#E6EDF3", family = "Inter"),
    xaxis         = list(gridcolor = "#30363D", zerolinecolor = "#30363D"),
    yaxis         = list(gridcolor = "#30363D", zerolinecolor = "#30363D"),
    legend        = list(bgcolor = "rgba(0,0,0,0)")
  )
}

fmt_pct <- function(x, digits = 2)  paste0(round(x * 100, digits), "%")
fmt_num <- function(x, digits = 2)  format(round(x, digits), big.mark = ",")
fmt_dollar <- function(x, digits = 2) paste0("$", format(round(x, digits), big.mark = ","))

# =============================================================================
server <- function(input, output, session) {

  # ---- Reactive containers --------------------------------------------------
  rv <- reactiveValues(
    prices_wide   = NULL,
    returns_wide  = NULL,
    port_result   = NULL,
    capm_result   = NULL,
    mc_paths      = NULL,
    mc_summary    = NULL,
    macro_df      = NULL,
    fc_result     = NULL,
    wfv_result    = NULL
  )

  # ==========================================================================
  # TAB 1: PORTFOLIO ANALYTICS
  # ==========================================================================

  observeEvent(input$run_portfolio, {
    req(input$tickers, length(input$tickers) >= 1)
    withProgress(message = "Fetching market data...", value = 0.2, {
      tickers_all <- unique(c(input$tickers, input$benchmark))

      prices <- tryCatch(
        fetch_adjusted_prices(tickers_all, from = input$date_range[1],
                              to = input$date_range[2], cfg = cfg),
        error = function(e) {
          showNotification(glue("Data fetch failed: {e$message}"), type = "error")
          NULL
        }
      )

      if (is.null(prices)) return()

      incProgress(0.4, message = "Computing returns...")

      # Fill gaps and compute log returns
      prices <- fill_price_gaps(prices)
      tickers <- intersect(input$tickers, names(prices))
      prices  <- prices[, c("date", tickers)]

      ret_wide <- prices |>
        dplyr::mutate(across(-date, log_returns |> (\(f) function(x) c(NA, f(x)))()))
      ret_wide <- ret_wide[-1, ]   # drop first NA row

      rv$prices_wide  <- prices
      rv$returns_wide <- ret_wide

      incProgress(0.3, message = "Optimising portfolio...")

      rf_daily <- input$rf_rate / 100 / 252
      mu_vec   <- colMeans(ret_wide[, tickers], na.rm = TRUE) * 252
      Sigma    <- cov(ret_wide[, tickers], use = "complete.obs") * 252

      weights <- switch(input$opt_method,
        equal    = setNames(rep(1 / length(tickers), length(tickers)), tickers),
        min_var  = min_variance_portfolio(Sigma),
        max_sharpe = max_sharpe_portfolio(mu_vec, Sigma, rf = input$rf_rate / 100),
        equal    = setNames(rep(1 / length(tickers), length(tickers)), tickers)
      )

      rv$port_result <- portfolio_statistics(ret_wide, weights, rf = input$rf_rate / 100)
    })
  })

  # KPI outputs
  output$kpi_total_ret  <- renderText({
    req(rv$port_result)
    fmt_pct(rv$port_result$stats["total_return"])
  })
  output$kpi_ann_ret    <- renderText({
    req(rv$port_result)
    fmt_pct(rv$port_result$stats["ann_return"])
  })
  output$kpi_vol        <- renderText({
    req(rv$port_result)
    fmt_pct(rv$port_result$stats["ann_volatility"])
  })
  output$kpi_sharpe     <- renderText({
    req(rv$port_result)
    fmt_num(rv$port_result$stats["sharpe_ratio"])
  })
  output$kpi_mdd        <- renderText({
    req(rv$port_result)
    fmt_pct(rv$port_result$stats["max_drawdown"])
  })
  output$kpi_var95      <- renderText({
    req(rv$port_result)
    fmt_pct(rv$port_result$stats["var_95_daily"])
  })

  # NAV chart
  output$plot_nav <- renderPlotly({
    req(rv$prices_wide, rv$port_result)
    w   <- rv$port_result$weights
    nav <- rv$port_result$nav
    df  <- tibble::tibble(date = rv$port_result$dates, Portfolio = nav)

    # Add individual tickers (rebased)
    for (t in names(w)) {
      px <- rv$prices_wide[[t]]
      df[[t]] <- px / px[1] * 100
    }

    df_long <- tidyr::pivot_longer(df, -date, names_to = "series", values_to = "nav")

    p <- plotly::plot_ly(df_long, x = ~date, y = ~nav, color = ~series,
                          type = "scatter", mode = "lines",
                          line = list(width = 1.5)) |>
      plotly::layout(yaxis = list(title = "Index (100 = start)"),
                     xaxis = list(title = "")) |>
      plotly_theme()
    p
  })

  # Weights donut chart
  output$plot_weights <- renderPlotly({
    req(rv$port_result)
    w  <- rv$port_result$weights
    df <- tibble::tibble(asset = names(w), weight = as.numeric(w))
    plotly::plot_ly(df, labels = ~asset, values = ~weight,
                     type = "pie", hole = 0.5,
                     marker = list(colors = PALETTE$series)) |>
      plotly::layout(showlegend = TRUE) |>
      plotly_theme()
  })

  # Drawdown chart
  output$plot_drawdown <- renderPlotly({
    req(rv$port_result)
    nav <- rv$port_result$nav
    dd  <- drawdown_series(nav) * 100
    df  <- tibble::tibble(date = rv$port_result$dates, drawdown = dd)
    plotly::plot_ly(df, x = ~date, y = ~drawdown, type = "scatter",
                     mode = "lines", fill = "tozeroy",
                     line = list(color = "#C0392B", width = 1),
                     fillcolor = "rgba(192,57,43,0.25)") |>
      plotly::layout(yaxis = list(title = "Drawdown (%)")) |>
      plotly_theme()
  })

  # Rolling vol chart
  output$plot_roll_vol <- renderPlotly({
    req(rv$port_result)
    ret  <- rv$port_result$returns
    dates <- rv$port_result$dates
    rvol  <- roll_apply(ret, 63, sd) * sqrt(252) * 100
    df    <- tibble::tibble(date = dates, rolling_vol = rvol)
    plotly::plot_ly(df, x = ~date, y = ~rolling_vol, type = "scatter",
                     mode = "lines",
                     line = list(color = "#2980B9", width = 1.5)) |>
      plotly::layout(yaxis = list(title = "Annualised Vol (%)")) |>
      plotly_theme()
  })

  # Stats table
  output$tbl_stats <- DT::renderDataTable({
    req(rv$returns_wide)
    tickers <- setdiff(names(rv$returns_wide), "date")
    rf_d    <- input$rf_rate / 100 / 252

    stats_df <- purrr::map_dfr(tickers, function(t) {
      r    <- rv$returns_wide[[t]]
      px   <- rv$prices_wide[[t]]
      s    <- return_statistics(r, px, rf = input$rf_rate / 100)
      tibble::tibble(
        Ticker      = t,
        `Ann Return`   = fmt_pct(s["ann_return"]),
        `Ann Vol`      = fmt_pct(s["ann_volatility"]),
        `Sharpe`       = fmt_num(s["sharpe_ratio"]),
        `Sortino`      = fmt_num(s["sortino_ratio"]),
        `Max DD`       = fmt_pct(s["max_drawdown"]),
        `Skewness`     = fmt_num(s["skewness"]),
        `Kurtosis`     = fmt_num(s["excess_kurtosis"]),
        `VaR 95%`      = fmt_pct(s["var_95_daily"])
      )
    })

    DT::datatable(stats_df, options = list(pageLength = 15, dom = "t"),
                  rownames = FALSE, class = "dt-table")
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
        # Historical bootstrap: fetch real returns
        px <- tryCatch(
          fetch_yahoo(input$mc_ticker, cfg = cfg),
          error = function(e) NULL
        )
        if (is.null(px)) {
          showNotification("Failed to fetch historical data", type = "error")
          return()
        }
        ret   <- log_returns(px$adjusted)
        paths <- simulate_historical_bootstrap(ret, S0 = input$mc_S0,
                                               T_days = input$mc_horizon,
                                               n_paths = input$mc_paths)
      }

      incProgress(0.7, message = "Summarising results...")
      rv$mc_paths   <- paths
      rv$mc_summary <- summarise_mc_paths(paths, S0 = input$mc_S0, rf = 0.0525)
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
    paths   <- rv$mc_paths
    T_days  <- nrow(paths) - 1
    n_paths <- ncol(paths)

    pct <- apply(paths, 1, quantile, probs = c(0.05, 0.25, 0.50, 0.75, 0.95)) |>
      t() |> as.data.frame()
    names(pct) <- c("p05","p25","p50","p75","p95")
    pct$day <- 0:T_days

    plotly::plot_ly(pct, x = ~day) |>
      plotly::add_ribbons(ymin = ~p05, ymax = ~p95,
                           fillcolor = "rgba(41,128,185,0.12)", line = list(width = 0),
                           name = "5th–95th %ile") |>
      plotly::add_ribbons(ymin = ~p25, ymax = ~p75,
                           fillcolor = "rgba(41,128,185,0.25)", line = list(width = 0),
                           name = "25th–75th %ile") |>
      plotly::add_lines(y = ~p50, line = list(color = "#1A3A5C", width = 2),
                         name = "Median") |>
      plotly::layout(xaxis = list(title = "Trading Days"),
                     yaxis = list(title = "Simulated Price ($)")) |>
      plotly_theme()
  })

  output$plot_mc_dist <- renderPlotly({
    req(rv$mc_paths, rv$mc_summary)
    terminal  <- rv$mc_paths[nrow(rv$mc_paths), ]
    S0        <- input$mc_S0
    returns   <- (terminal / S0 - 1) * 100
    var95     <- rv$mc_summary$var_95 * 100

    plotly::plot_ly(
      x    = returns,
      type = "histogram",
      nbinsx = 80,
      marker = list(
        color = ifelse(returns < 0, "rgba(192,57,43,0.8)", "rgba(39,174,96,0.8)"),
        line  = list(width = 0)
      )
    ) |>
      plotly::add_vline(x = 0, line = list(dash = "dot", color = "#E6EDF3")) |>
      plotly::add_vline(x = -var95, line = list(dash = "dash", color = "#E67E22"),
                         annotation_text = glue("VaR 95%")) |>
      plotly::layout(xaxis = list(title = "Terminal Return (%)"),
                     yaxis = list(title = "Count")) |>
      plotly_theme()
  })

  # ==========================================================================
  # TAB 4: MACRO EXPLORER
  # ==========================================================================

  observeEvent(input$run_macro, {
    req(input$macro_series, length(input$macro_series) >= 1)
    withProgress(message = "Loading macro data from FRED...", value = 0.3, {
      df <- tryCatch(
        fetch_fred_multi(
          series_keys = input$macro_series,
          from        = input$macro_dates[1],
          to          = input$macro_dates[2],
          cfg         = cfg
        ),
        error = function(e) {
          showNotification(glue("FRED fetch failed: {e$message}"), type = "error")
          NULL
        }
      )
      incProgress(0.6)
      rv$macro_df <- df
    })
  })

  output$plot_macro_panel <- renderPlot({
    req(rv$macro_df)
    chart_macro_panel(rv$macro_df, series = input$macro_series)
  }, bg = "#0D1117")

  output$plot_yield_curve <- renderPlotly({
    yc <- tryCatch(
      fetch_yield_curve(cfg = cfg),
      error = function(e) NULL
    )
    if (is.null(yc) || nrow(yc) == 0) return(plotly::plot_ly())

    plotly::plot_ly(yc, x = ~maturity_years, y = ~yield,
                     type = "scatter", mode = "lines+markers",
                     line = list(color = "#D4A017", width = 2),
                     marker = list(color = "#D4A017", size = 8)) |>
      plotly::layout(xaxis = list(title = "Maturity (years)"),
                     yaxis = list(title = "Yield (%)")) |>
      plotly_theme()
  })

  # ==========================================================================
  # TAB 5: FORECASTING
  # ==========================================================================

  observeEvent(input$run_forecast, {
    withProgress(message = "Fetching data & fitting models...", value = 0.2, {
      x <- tryCatch({
        if (input$fc_series_type == "price") {
          px <- fetch_yahoo(input$fc_ticker, cfg = cfg)
          log(px$adjusted)     # log price for ARIMA on returns
        } else {
          df <- fetch_fred(input$fc_fred_series, cfg = cfg)
          df$value
        }
      }, error = function(e) {
        showNotification(glue("Data fetch failed: {e$message}"), type = "error")
        NULL
      })
      if (is.null(x)) return()

      incProgress(0.4, message = "Fitting models...")
      fit  <- fit_arima(x, max_p = 5, max_q = 5)

      if (is.null(fit)) {
        showNotification("ARIMA fit failed", type = "error")
        return()
      }

      fc <- forecast_arima(fit, horizon = input$fc_horizon)
      rv$fc_result  <- list(fit = fit, fc = fc, x = x)

      incProgress(0.4, message = "Walk-forward validation...")
      if (input$fc_backtest) {
        rv$wfv_result <- walk_forward_validation(x, model_type = input$fc_model_type,
                                                  horizon = 1L,
                                                  min_train = input$fc_min_train)
      }
    })
  })

  output$fc_model_label <- renderText({
    req(rv$fc_result)
    ord <- rv$fc_result$fit$order
    glue("ARIMA({ord[1]},{ord[2]},{ord[3]})")
  })
  output$fc_aic   <- renderText({ req(rv$fc_result); fmt_num(rv$fc_result$fit$diagnostics$aicc) })
  output$fc_rmse  <- renderText({
    req(rv$fc_result)
    actual  <- rv$fc_result$x
    fitted_ <- rv$fc_result$fit$fitted
    fmt_num(sqrt(mean((actual - fitted_)^2, na.rm = TRUE)))
  })
  output$fc_mae   <- renderText({
    req(rv$fc_result)
    fmt_num(mean(abs(rv$fc_result$fit$residuals), na.rm = TRUE))
  })
  output$fc_mape  <- renderText({
    req(rv$fc_result)
    x <- rv$fc_result$x
    f <- rv$fc_result$fit$fitted
    fmt_num(mean(abs((x - f) / x) * 100, na.rm = TRUE))
  })
  output$fc_ljung <- renderText({
    req(rv$fc_result)
    fmt_num(rv$fc_result$fit$ljung_box$p.value)
  })

  output$plot_forecast <- renderPlot({
    req(rv$fc_result)
    fc  <- rv$fc_result$fc
    x   <- rv$fc_result$x
    n   <- length(x)
    chart_forecast(
      x_hist    = tail(x, 200),
      dates_hist = seq(Sys.Date() - 199, Sys.Date(), by = "day")[1:200],
      fc_df     = fc |> dplyr::rename(lo_80 = lo_80, hi_80 = hi_80,
                                       lo_95 = lo_95, hi_95 = hi_95),
      title     = glue("{input$fc_series_type} Forecast — {input$fc_model_type}")
    )
  }, bg = "#0D1117")

  # ==========================================================================
  # REPORT DOWNLOAD
  # ==========================================================================

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
        input       = template,
        output_file = file,
        output_format = if (input$report_format == "html")
                          "html_document" else "pdf_document",
        params      = list(cfg = cfg, title = input$report_title,
                            author = input$report_author)
      )
    }
  )

  # Export MC paths
  output$dl_mc_paths <- downloadHandler(
    filename = function() paste0("mc_paths_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$mc_paths)
      readr::write_csv(as.data.frame(rv$mc_paths), file)
    }
  )
}
