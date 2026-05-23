# =============================================================================
# Shiny Dashboard — UI
# Quant Research & Economic Intelligence Platform
#
# Design philosophy: Bloomberg Terminal meets institutional research software.
# Navigation is modular — each tab is a self-contained analytical workspace.
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(shinyjs)

# ---- Colour constants (mirrors R/visualization/theme.R) --------------------
CLR <- list(
  navy    = "#1A3A5C",
  red     = "#C0392B",
  blue    = "#2980B9",
  gold    = "#D4A017",
  green   = "#27AE60",
  bg      = "#0D1117",    # dark terminal background
  panel   = "#161B22",
  border  = "#30363D",
  text    = "#E6EDF3",
  muted   = "#8B949E"
)

# ---- Custom dark terminal CSS -----------------------------------------------
terminal_css <- "
body { background-color: #0D1117; color: #E6EDF3; }
.navbar-brand { font-weight: 700; letter-spacing: 1px; color: #D4A017 !important; }
.nav-link.active { border-bottom: 2px solid #D4A017 !important; color: #D4A017 !important; }
.card { background-color: #161B22; border: 1px solid #30363D; border-radius: 6px; }
.card-header { background-color: #1A3A5C; color: #E6EDF3; font-weight: 600;
               font-size: 0.85rem; letter-spacing: 0.5px; text-transform: uppercase; }
.metric-box { background: #161B22; border: 1px solid #30363D; border-radius: 6px;
              padding: 14px 18px; text-align: center; }
.metric-value { font-size: 1.6rem; font-weight: 700; font-family: monospace; }
.metric-label { font-size: 0.72rem; color: #8B949E; text-transform: uppercase;
                letter-spacing: 0.8px; margin-top: 2px; }
.metric-pos   { color: #27AE60; }
.metric-neg   { color: #C0392B; }
.metric-neu   { color: #D4A017; }
.sidebar { background-color: #161B22; border-right: 1px solid #30363D; }
.selectize-input, .form-control { background-color: #0D1117 !important;
  border-color: #30363D !important; color: #E6EDF3 !important; }
.section-header { color: #8B949E; font-size: 0.7rem; font-weight: 600;
                  letter-spacing: 1.5px; text-transform: uppercase;
                  border-bottom: 1px solid #30363D; padding-bottom: 6px; margin: 12px 0 8px; }
.dt-table { background: #161B22 !important; color: #E6EDF3 !important; }
table.dataTable { background-color: #161B22 !important; color: #E6EDF3 !important; }
table.dataTable thead th { background-color: #1A3A5C !important; color: #E6EDF3 !important; }
.badge-quant { background: #1A3A5C; color: #D4A017; padding: 2px 8px;
               border-radius: 4px; font-size: 0.7rem; font-weight: 600; }
"

# ---- Helper: KPI metric box ------------------------------------------------
metric_box <- function(id_val, id_lbl, label,
                        colour_class = "metric-neu", width = 2) {
  div(
    class = "metric-box",
    div(class = paste("metric-value", colour_class), textOutput(id_val, inline = TRUE)),
    div(class = "metric-label", label)
  )
}

# ============================================================================
# PAGE LAYOUT
# ============================================================================

ui <- bslib::page_navbar(
  title = span(
    span("Q", style = "color:#D4A017"),
    "UANT RESEARCH PLATFORM"
  ),
  theme = bslib::bs_theme(
    version    = 5,
    bg         = CLR$bg,
    fg         = CLR$text,
    primary    = CLR$navy,
    secondary  = CLR$blue,
    base_font  = bslib::font_google("Inter"),
    code_font  = bslib::font_google("JetBrains Mono"),
    "navbar-bg"= CLR$panel
  ),
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(terminal_css)))
  ),
  selected = "Portfolio",

  # ==========================================================================
  # TAB 1: PORTFOLIO ANALYTICS
  # ==========================================================================
  bslib::nav_panel(
    "Portfolio",
    icon = bsicons::bs_icon("graph-up"),

    bslib::layout_sidebar(
      # ---- Sidebar -----------------------------------------------------------
      sidebar = bslib::sidebar(
        width = 260,
        div(class = "section-header", "Configuration"),

        selectizeInput(
          "tickers", "Assets",
          choices  = c("SPY","QQQ","AGG","GLD","TLT","EEM","VNQ","GSG","BTC-USD","ETH-USD"),
          selected = c("SPY","QQQ","AGG","GLD","TLT"),
          multiple = TRUE,
          options  = list(plugins = list("remove_button"), maxItems = 10)
        ),

        dateRangeInput(
          "date_range", "Date Range",
          start = Sys.Date() - 365 * 3,
          end   = Sys.Date(),
          min   = "2000-01-01"
        ),

        sliderInput("rf_rate", "Risk-Free Rate (%)",
                    min = 0, max = 10, value = 5.25, step = 0.05, post = "%"),

        div(class = "section-header", "Benchmark"),
        selectInput("benchmark", NULL,
                    choices = c("SPY","QQQ","IWM","VTI"), selected = "SPY"),

        div(class = "section-header", "Optimisation"),
        selectInput("opt_method", "Portfolio",
                    choices = c("Equal Weight"   = "equal",
                                "Min Variance"   = "min_var",
                                "Max Sharpe"     = "max_sharpe",
                                "Custom Weights" = "custom")),

        conditionalPanel(
          condition = "input.opt_method == 'custom'",
          numericInput("custom_w_json", "Weights (comma-sep)", value = NULL)
        ),

        actionButton("run_portfolio", "Run Analysis",
                     class = "btn btn-primary w-100 mt-2",
                     icon  = icon("play"))
      ),

      # ---- Main panel --------------------------------------------------------
      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),

        div(class = "metric-box",
            div(class = "metric-value metric-pos", textOutput("kpi_total_ret")),
            div(class = "metric-label", "Total Return")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("kpi_ann_ret")),
            div(class = "metric-label", "Ann. Return")),
        div(class = "metric-box",
            div(class = "metric-value metric-neg", textOutput("kpi_vol")),
            div(class = "metric-label", "Ann. Volatility")),
        div(class = "metric-box",
            div(class = "metric-value metric-neu", textOutput("kpi_sharpe")),
            div(class = "metric-label", "Sharpe Ratio")),
        div(class = "metric-box",
            div(class = "metric-value metric-neg", textOutput("kpi_mdd")),
            div(class = "metric-label", "Max Drawdown")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("kpi_var95")),
            div(class = "metric-label", "VaR 95% (daily)"))
      ),

      br(),

      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header("NAV Performance"),
          plotlyOutput("plot_nav", height = "340px")
        ),
        bslib::card(
          bslib::card_header("Portfolio Weights"),
          plotlyOutput("plot_weights", height = "340px")
        )
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Drawdown (Underwater)"),
          plotlyOutput("plot_drawdown", height = "280px")
        ),
        bslib::card(
          bslib::card_header("Rolling Volatility (63-day)"),
          plotlyOutput("plot_roll_vol", height = "280px")
        )
      ),

      bslib::card(
        bslib::card_header("Asset Statistics Table"),
        DT::dataTableOutput("tbl_stats", height = "300px")
      )
    )
  ),

  # ==========================================================================
  # TAB 2: CAPM & FACTOR ANALYSIS
  # ==========================================================================
  bslib::nav_panel(
    "CAPM / Factor",
    icon = bsicons::bs_icon("calculator"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 240,
        div(class = "section-header", "Settings"),
        selectizeInput("capm_tickers", "Assets",
          choices  = c("AAPL","MSFT","GOOGL","AMZN","META","TSLA","NVDA","JPM","GS","BRK-B"),
          selected = c("AAPL","MSFT","GOOGL","AMZN"),
          multiple = TRUE),
        selectInput("capm_benchmark", "Market Proxy",
          choices = c("SPY","QQQ","VTI"), selected = "SPY"),
        dateRangeInput("capm_dates", "Period",
          start = Sys.Date() - 365 * 3, end = Sys.Date()),
        actionButton("run_capm", "Compute CAPM",
          class = "btn btn-primary w-100 mt-2", icon = icon("play"))
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Security Characteristic Lines (CAPM Regressions)"),
          plotlyOutput("plot_scl", height = "380px")
        ),
        bslib::card(
          bslib::card_header("Alpha vs Beta — Risk Decomposition"),
          plotlyOutput("plot_alpha_beta", height = "380px")
        )
      ),

      bslib::card(
        bslib::card_header("CAPM Results Table"),
        DT::dataTableOutput("tbl_capm")
      )
    )
  ),

  # ==========================================================================
  # TAB 3: MONTE CARLO SIMULATION
  # ==========================================================================
  bslib::nav_panel(
    "Monte Carlo",
    icon = bsicons::bs_icon("shuffle"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        div(class = "section-header", "Simulation Parameters"),
        selectInput("mc_ticker",  "Asset", choices = c("SPY","AAPL","BTC-USD","GLD")),
        numericInput("mc_paths",  "Simulated Paths",  value = 5000,  min = 100, max = 50000),
        numericInput("mc_horizon","Horizon (days)",   value = 252,   min = 1,   max = 1260),
        numericInput("mc_S0",     "Initial Price ($)", value = 100,  min = 0.01),

        div(class = "section-header", "Process"),
        selectInput("mc_model", "Model",
          choices = c("Geometric Brownian Motion" = "gbm",
                      "Historical Bootstrap"      = "historical")),

        numericInput("mc_drift", "Annual Drift (%)",    value = 10,  step = 0.5),
        numericInput("mc_vol",   "Annual Volatility (%)", value = 20, step = 0.5),

        actionButton("run_mc", "Run Simulation",
          class = "btn btn-primary w-100 mt-2", icon = icon("play")),
        downloadButton("dl_mc_paths", "Export Paths", class = "btn btn-outline-secondary w-100 mt-1")
      ),

      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("mc_mean_terminal")),
            div(class = "metric-label", "Mean Terminal Price")),
        div(class = "metric-box",
            div(class = "metric-value metric-pos", textOutput("mc_p75")),
            div(class = "metric-label", "75th Percentile")),
        div(class = "metric-box",
            div(class = "metric-value metric-neg", textOutput("mc_p25")),
            div(class = "metric-label", "25th Percentile")),
        div(class = "metric-box",
            div(class = "metric-value metric-neg", textOutput("mc_var95")),
            div(class = "metric-label", "VaR 95% (horizon)")),
        div(class = "metric-box",
            div(class = "metric-value metric-neg", textOutput("mc_es95")),
            div(class = "metric-label", "ES 95% (horizon)")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("mc_prob_loss")),
            div(class = "metric-label", "P(Loss)"))
      ),

      br(),

      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header("Simulated Price Paths — Fan Chart"),
          plotlyOutput("plot_mc_paths", height = "400px")
        ),
        bslib::card(
          bslib::card_header("Terminal Return Distribution"),
          plotlyOutput("plot_mc_dist", height = "400px")
        )
      )
    )
  ),

  # ==========================================================================
  # TAB 4: MACROECONOMIC EXPLORER
  # ==========================================================================
  bslib::nav_panel(
    "Macro Explorer",
    icon = bsicons::bs_icon("globe"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        div(class = "section-header", "Indicators"),
        checkboxGroupInput(
          "macro_series", NULL,
          choices  = c(
            "Real GDP Growth"       = "gdp_growth_pct",
            "CPI Inflation"         = "cpi_all",
            "Core CPI"              = "cpi_core",
            "Unemployment Rate"     = "unemployment",
            "Fed Funds Rate"        = "fed_funds",
            "10Y Treasury Yield"    = "t_note_10y",
            "2Y Treasury Yield"     = "t_note_2y",
            "Yield Spread (10Y-2Y)" = "yield_spread",
            "M2 Money Supply"       = "m2_money",
            "VIX"                   = "vix",
            "HY Credit Spread"      = "credit_spread_hy"
          ),
          selected = c("gdp_growth_pct","cpi_all","unemployment","fed_funds","yield_spread")
        ),
        dateRangeInput("macro_dates", "Date Range",
          start = "2000-01-01", end = Sys.Date()),
        actionButton("run_macro", "Load Data",
          class = "btn btn-primary w-100 mt-2", icon = icon("play"))
      ),

      bslib::card(
        bslib::card_header("Macroeconomic Indicators Panel"),
        plotOutput("plot_macro_panel", height = "600px")
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("US Yield Curve"),
          plotlyOutput("plot_yield_curve", height = "360px")
        ),
        bslib::card(
          bslib::card_header("Indicator Correlation Heatmap"),
          plotOutput("plot_macro_corr", height = "360px")
        )
      )
    )
  ),

  # ==========================================================================
  # TAB 5: FORECASTING LABORATORY
  # ==========================================================================
  bslib::nav_panel(
    "Forecasting",
    icon = bsicons::bs_icon("bar-chart-steps"),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        div(class = "section-header", "Target Series"),
        selectInput("fc_series_type", "Data Type",
          choices = c("Asset Price" = "price", "FRED Macro" = "fred")),
        conditionalPanel(
          condition = "input.fc_series_type == 'price'",
          selectInput("fc_ticker", "Ticker", choices = c("SPY","QQQ","GLD","BTC-USD"))
        ),
        conditionalPanel(
          condition = "input.fc_series_type == 'fred'",
          selectInput("fc_fred_series", "FRED Series",
            choices = c("CPI" = "CPIAUCSL", "Unemployment" = "UNRATE",
                        "10Y Yield" = "DGS10", "GDP" = "GDPC1"))
        ),
        numericInput("fc_horizon", "Horizon (periods)", value = 30, min = 1, max = 365),
        selectInput("fc_model_type", "Primary Model",
          choices = c("ARIMA" = "arima", "ETS" = "ets", "Ensemble" = "ensemble")),
        div(class = "section-header", "Backtesting"),
        checkboxInput("fc_backtest", "Run Walk-Forward Validation", value = FALSE),
        numericInput("fc_min_train", "Min Training Window", value = 100, min = 30),
        actionButton("run_forecast", "Generate Forecast",
          class = "btn btn-primary w-100 mt-2", icon = icon("play"))
      ),

      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("fc_model_label")),
            div(class = "metric-label", "Model")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("fc_aic")),
            div(class = "metric-label", "AICc")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("fc_rmse")),
            div(class = "metric-label", "RMSE (in-sample)")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("fc_mae")),
            div(class = "metric-label", "MAE")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("fc_mape")),
            div(class = "metric-label", "MAPE (%)")),
        div(class = "metric-box",
            div(class = "metric-value", textOutput("fc_ljung")),
            div(class = "metric-label", "Ljung-Box p-val"))
      ),

      br(),

      bslib::card(
        bslib::card_header("Forecast with Prediction Intervals"),
        plotOutput("plot_forecast", height = "400px")
      ),

      conditionalPanel(
        condition = "input.fc_backtest",
        bslib::card(
          bslib::card_header("Walk-Forward Validation — Forecast Errors by Horizon"),
          plotOutput("plot_wfv", height = "300px")
        )
      )
    )
  ),

  # ==========================================================================
  # TAB 6: REPORT GENERATOR
  # ==========================================================================
  bslib::nav_panel(
    "Reports",
    icon = bsicons::bs_icon("file-earmark-pdf"),

    bslib::layout_columns(
      col_widths = c(4, 8),

      bslib::card(
        bslib::card_header("Generate Report"),
        bslib::card_body(
          selectInput("report_type", "Report Template",
            choices = c(
              "Portfolio Summary"       = "portfolio",
              "Economic Outlook"        = "macro",
              "Risk Report"             = "risk",
              "ARIMA Forecast Report"   = "forecast",
              "Full Platform Report"    = "full"
            )),
          selectInput("report_format", "Output Format",
            choices = c("HTML" = "html", "PDF" = "pdf")),
          textInput("report_title", "Custom Title", placeholder = "Optional"),
          textInput("report_author", "Author",      placeholder = "Quant Research Platform"),
          checkboxInput("report_include_charts", "Include all charts", value = TRUE),
          checkboxInput("report_include_tables", "Include data tables", value = TRUE),
          br(),
          actionButton("gen_report", "Generate Report",
            class = "btn btn-primary w-100", icon = icon("file-pdf")),
          br(), br(),
          downloadButton("dl_report", "Download Report",
            class = "btn btn-outline-secondary w-100")
        )
      ),

      bslib::card(
        bslib::card_header("Report Preview"),
        bslib::card_body(
          htmlOutput("report_preview")
        )
      )
    )
  ),

  # Navigation footer
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$span(class = "badge-quant", glue::glue("v1.0.0"))
  )
)
