# =============================================================================
# Institutional Chart Library
# Publication-quality charts for financial and macroeconomic analysis.
# All functions return ggplot objects — composable and reproducible.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(glue)
  library(lubridate)
  library(plotly)
})

source(here::here("R", "visualization", "theme.R"))
source(here::here("R", "utilities", "helpers.R"))

# =============================================================================
# PRICE & RETURN CHARTS
# =============================================================================

#' Multi-asset NAV performance chart (rebased to 100)
#'
#' @param prices_wide Wide tibble: date + ticker columns
#' @param title       Chart title
#' @param subtitle    Chart subtitle
#' @param rebase_date Rebase date (default: first date)
#' @return ggplot object
#' @export
chart_performance <- function(prices_wide,
                               title    = "Portfolio Performance",
                               subtitle = "Rebased to 100",
                               rebase_date = NULL) {
  if (is.null(rebase_date)) rebase_date <- min(prices_wide$date)

  tickers <- setdiff(names(prices_wide), "date")

  # Rebase: divide all prices by their value at rebase_date × 100
  rebase_values <- prices_wide |>
    dplyr::filter(date == rebase_date) |>
    dplyr::select(all_of(tickers)) |>
    as.list()

  rebased <- prices_wide |>
    dplyr::mutate(across(all_of(tickers), ~ .x / rebase_values[[cur_column()]] * 100)) |>
    tidyr::pivot_longer(all_of(tickers), names_to = "ticker", values_to = "nav")

  ggplot2::ggplot(rebased, aes(x = date, y = nav, colour = ticker)) +
    ggplot2::geom_line(linewidth = 0.75, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed",
                        colour = PALETTE$text_light, linewidth = 0.4) +
    scale_colour_quant() +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                          expand = expansion(0.01)) +
    scale_y_dollar(prefix = "") +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = NULL,
      y        = "Index (rebased = 100)",
      colour   = NULL,
      caption  = glue("Source: Yahoo Finance  |  Rebase date: {rebase_date}")
    ) +
    theme_quant()
}

#' Candlestick chart for a single asset (daily OHLC)
#'
#' @param ohlc_df tibble with date, open, high, low, close
#' @param ticker  Asset name for title
#' @param n_days  Number of most recent days to display
#' @return plotly object (interactive)
#' @export
chart_candlestick <- function(ohlc_df, ticker = "", n_days = 252L) {
  df <- ohlc_df |>
    dplyr::arrange(date) |>
    dplyr::slice_tail(n = n_days)

  plotly::plot_ly(
    df,
    x    = ~date,
    open = ~open, high = ~high, low = ~low, close = ~close,
    type = "candlestick",
    name = ticker,
    increasing = list(line = list(color = PALETTE$green)),
    decreasing = list(line = list(color = PALETTE$red))
  ) |>
    plotly::layout(
      title      = glue("{ticker} — OHLC Price"),
      xaxis      = list(rangeslider = list(visible = FALSE), title = ""),
      yaxis      = list(title = "Price (USD)"),
      paper_bgcolor = PALETTE$background,
      plot_bgcolor  = PALETTE$panel_bg
    )
}

#' Rolling volatility (annualised) chart
#'
#' @param returns_long Long-format tibble: date, ticker, return
#' @param window      Rolling window (trading days)
#' @return ggplot object
#' @export
chart_rolling_volatility <- function(returns_long, window = 63L) {
  vol <- returns_long |>
    dplyr::arrange(ticker, date) |>
    dplyr::group_by(ticker) |>
    dplyr::mutate(
      rolling_vol = roll_apply(return, window, sd) * sqrt(252) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(rolling_vol))

  ggplot2::ggplot(vol, aes(x = date, y = rolling_vol, colour = ticker)) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    scale_colour_quant() +
    scale_y_pct(accuracy = 1) +
    ggplot2::labs(
      title    = glue("{window}-Day Rolling Volatility (Annualised)"),
      subtitle = "Computed using square-root-of-time scaling",
      x        = NULL, y = "Annualised Volatility (%)", colour = NULL,
      caption  = "Source: Quant Research Platform"
    ) +
    theme_quant()
}

# =============================================================================
# RISK VISUALISATIONS
# =============================================================================

#' Drawdown area chart
#'
#' @param prices_wide Wide tibble: date + ticker columns
#' @param fill_alpha  Transparency for fill area
#' @return ggplot object
#' @export
chart_drawdown <- function(prices_wide, fill_alpha = 0.35) {
  tickers <- setdiff(names(prices_wide), "date")

  dd <- purrr::map_dfr(tickers, function(t) {
    p  <- prices_wide[[t]]
    dd <- drawdown_series(p)
    tibble::tibble(date = prices_wide$date, ticker = t, drawdown = dd * 100)
  })

  ggplot2::ggplot(dd, aes(x = date, y = drawdown, fill = ticker, colour = ticker)) +
    ggplot2::geom_area(alpha = fill_alpha, position = "identity") +
    ggplot2::geom_hline(yintercept = 0, colour = PALETTE$text_light, linewidth = 0.3) +
    scale_colour_quant() +
    scale_fill_quant() +
    scale_y_pct(accuracy = 1) +
    ggplot2::labs(
      title    = "Underwater Chart — Drawdown from Peak",
      subtitle = "Shaded area represents peak-to-trough loss",
      x        = NULL, y = "Drawdown (%)", fill = NULL, colour = NULL,
      caption  = "Source: Quant Research Platform"
    ) +
    theme_quant()
}

#' Return distribution histogram with normal density overlay
#'
#' @param returns    Numeric return vector
#' @param ticker     Asset label
#' @param ann_factor Annualisation factor (for title)
#' @return ggplot object
#' @export
chart_return_distribution <- function(returns, ticker = "", ann_factor = 252) {
  df  <- tibble::tibble(ret = returns * 100)
  mu  <- mean(df$ret, na.rm = TRUE)
  sig <- sd(df$ret, na.rm = TRUE)

  # Jarque-Bera-like summary statistics
  sk  <- (sum((df$ret - mu)^3) / length(df$ret)) / sig^3
  ku  <- (sum((df$ret - mu)^4) / length(df$ret)) / sig^4

  ggplot2::ggplot(df, aes(x = ret)) +
    ggplot2::geom_histogram(
      aes(y = after_stat(density)),
      bins = 60, fill = PALETTE$primary, alpha = 0.7, colour = NA
    ) +
    ggplot2::stat_function(
      fun  = dnorm,
      args = list(mean = mu, sd = sig),
      colour = PALETTE$secondary, linewidth = 1, linetype = "solid"
    ) +
    ggplot2::geom_vline(xintercept = mu, colour = PALETTE$gold,
                        linewidth = 0.8, linetype = "dashed") +
    scale_y_compact() +
    ggplot2::labs(
      title    = glue("{ticker} — Return Distribution"),
      subtitle = glue("Skewness: {round(sk, 3)}   Excess Kurtosis: {round(ku - 3, 3)}   (red = Normal fit)"),
      x        = "Daily Return (%)", y = "Density",
      caption  = "Source: Quant Research Platform"
    ) +
    theme_quant()
}

# =============================================================================
# CORRELATION & COVARIANCE
# =============================================================================

#' Institutional correlation matrix heatmap
#'
#' @param returns_wide Wide tibble: date + ticker columns of returns
#' @param title        Chart title
#' @return ggplot object
#' @export
chart_correlation_matrix <- function(returns_wide, title = "Asset Correlation Matrix") {
  tickers <- setdiff(names(returns_wide), "date")
  R       <- cor(returns_wide[, tickers], use = "pairwise.complete.obs")

  cor_long <- R |>
    as.data.frame() |>
    tibble::rownames_to_column("asset1") |>
    tidyr::pivot_longer(-asset1, names_to = "asset2", values_to = "corr")

  ggplot2::ggplot(cor_long, aes(x = asset1, y = asset2, fill = corr)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(aes(label = round(corr, 2)),
                       size = 3.2, colour = PALETTE$text_dark) +
    scale_fill_returns(low = PALETTE$red, high = PALETTE$green, midpoint = 0) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = title,
      x = NULL, y = NULL, fill = "Correlation",
      caption = "Pearson correlation of daily log-returns"
    ) +
    theme_quant() +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right", legend.direction = "vertical"
    )
}

# =============================================================================
# MACROECONOMIC CHARTS
# =============================================================================

#' Yield curve visualisation
#'
#' @param yield_curve_df tibble from fetch_yield_curve(): date, maturity_years, yield
#' @param dates          Specific dates to plot (vector); if NULL plots latest
#' @return ggplot object
#' @export
chart_yield_curve <- function(yield_curve_df, dates = NULL) {
  if (is.null(dates)) {
    yield_curve_df <- yield_curve_df |>
      dplyr::filter(date == max(date))
  } else {
    yield_curve_df <- dplyr::filter(yield_curve_df, date %in% as.Date(dates))
  }

  ggplot2::ggplot(
    yield_curve_df,
    aes(x = maturity_years, y = yield, colour = factor(date), group = factor(date))
  ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.5) +
    scale_colour_quant() +
    ggplot2::scale_x_continuous(
      breaks = c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30),
      labels = function(x) ifelse(x < 1, paste0(x * 12, "M"), paste0(x, "Y"))
    ) +
    scale_y_pct(accuracy = 0.1) +
    ggplot2::labs(
      title    = "US Treasury Yield Curve",
      subtitle = "Annualised yields by maturity",
      x = "Maturity", y = "Yield (%)", colour = "Date",
      caption = "Source: Federal Reserve Economic Data (FRED)"
    ) +
    theme_quant()
}

#' Macroeconomic indicator time series panel
#'
#' @param macro_df  Long tibble: date, series_id, value, title
#' @param series    Vector of series_ids to plot (NULL = all)
#' @return ggplot facet object
#' @export
chart_macro_panel <- function(macro_df, series = NULL) {
  if (!is.null(series)) {
    macro_df <- dplyr::filter(macro_df, series_id %in% series)
  }

  ggplot2::ggplot(macro_df, aes(x = date, y = value)) +
    ggplot2::geom_line(colour = PALETTE$primary, linewidth = 0.65) +
    ggplot2::geom_hline(yintercept = 0, colour = PALETTE$text_light,
                        linewidth = 0.3, linetype = "dotted") +
    ggplot2::facet_wrap(~ title, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_date(date_labels = "'%y") +
    ggplot2::labs(
      title   = "Macroeconomic Indicators",
      x = NULL, y = NULL,
      caption = "Source: Federal Reserve Economic Data (FRED)"
    ) +
    theme_quant()
}

# =============================================================================
# EFFICIENT FRONTIER
# =============================================================================

#' Plot the efficient frontier with special portfolios highlighted
#'
#' @param frontier_df   tibble from efficient_frontier()
#' @param mvp_weights   Minimum variance portfolio weights (optional)
#' @param msr_weights   Max Sharpe portfolio weights (optional)
#' @param asset_df      tibble of individual assets: ticker, ann_return, ann_vol
#' @return ggplot object
#' @export
chart_efficient_frontier <- function(frontier_df,
                                      asset_df    = NULL,
                                      rf          = 0.0525) {
  p <- ggplot2::ggplot(frontier_df,
    aes(x = portfolio_vol * 100, y = target_return * 100, colour = sharpe_ratio)
  ) +
    ggplot2::geom_path(linewidth = 1.5) +
    ggplot2::scale_colour_gradient2(
      low = PALETTE$red, mid = PALETTE$gold, high = PALETTE$green,
      midpoint = mean(frontier_df$sharpe_ratio, na.rm = TRUE),
      name = "Sharpe\nRatio"
    ) +
    ggplot2::geom_point(
      data = frontier_df |> dplyr::slice_max(sharpe_ratio, n = 1),
      aes(x = portfolio_vol * 100, y = target_return * 100),
      colour = PALETTE$gold, size = 5, shape = 18
    ) +
    ggplot2::annotate(
      "text", x = frontier_df$portfolio_vol[which.max(frontier_df$sharpe_ratio)] * 100 + 0.3,
      y = max(frontier_df$sharpe_ratio * frontier_df$target_return, na.rm = TRUE),
      label = "Max Sharpe", colour = PALETTE$gold, size = 3.5, fontface = "bold"
    )

  if (!is.null(asset_df)) {
    p <- p + ggplot2::geom_point(
      data    = asset_df,
      mapping = aes(x = ann_vol * 100, y = ann_return * 100),
      colour = PALETTE$secondary, size = 3, shape = 16, inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data    = asset_df,
      mapping = aes(x = ann_vol * 100, y = ann_return * 100, label = ticker),
      colour  = PALETTE$secondary, size = 2.8, vjust = -0.8, inherit.aes = FALSE
    )
  }

  p +
    scale_x_pct(accuracy = 1) +
    scale_y_pct(accuracy = 1) +
    ggplot2::labs(
      title    = "Mean-Variance Efficient Frontier",
      subtitle = "Markowitz (1952) — long-only constrained optimisation",
      x = "Annualised Volatility (%)", y = "Expected Annual Return (%)",
      caption = "Star (★) = Maximum Sharpe Ratio portfolio"
    ) +
    theme_quant()
}
