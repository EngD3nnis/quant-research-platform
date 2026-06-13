# =============================================================================
# Shiny Dashboard — UI v2.0
# Quant Research & Economic Intelligence Platform
#
# Design system:
#   Deep-ocean dark theme with blue-tinted surfaces, vibrant accent colours,
#   CSS custom properties for consistency, skeleton loading animations,
#   micro-interactions on interactive elements, and a new Country Explorer tab.
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(shinyjs)

# ---- Design tokens -----------------------------------------------------------
CLR <- list(
  # Backgrounds (deepest to most elevated)
  bg_base    = "#080E18",
  bg_surface = "#0D1520",
  bg_card    = "#111C2C",
  bg_raised  = "#172336",
  bg_hover   = "#1C2B42",

  # Borders
  border_1   = "#1A2B42",
  border_2   = "#243650",
  border_3   = "#2E4468",

  # Text
  text_1     = "#E2EAF6",
  text_2     = "#8A9BB8",
  text_3     = "#4E6280",

  # Accent colours
  blue       = "#1968E3",
  blue_dim   = "#1345A8",
  gold       = "#F0B429",
  gold_dim   = "#B0820E",
  green      = "#0CB886",
  green_dim  = "#097A5B",
  red        = "#EF4444",
  red_dim    = "#A02020",
  purple     = "#8B5CF6",
  cyan       = "#06B6D4"
)

# ---- Global CSS --------------------------------------------------------------
platform_css <- "
/* ---------- CSS CUSTOM PROPERTIES ---------------------------------------- */
:root {
  --bg-base:    #080E18;
  --bg-surface: #0D1520;
  --bg-card:    #111C2C;
  --bg-raised:  #172336;
  --bg-hover:   #1C2B42;
  --border-1:   #1A2B42;
  --border-2:   #243650;
  --border-3:   #2E4468;
  --text-1:     #E2EAF6;
  --text-2:     #8A9BB8;
  --text-3:     #4E6280;
  --blue:       #1968E3;
  --gold:       #F0B429;
  --green:      #0CB886;
  --red:        #EF4444;
  --purple:     #8B5CF6;
  --cyan:       #06B6D4;
  --radius-sm:  6px;
  --radius-md:  10px;
  --radius-lg:  14px;
  --shadow-sm:  0 2px 8px rgba(0,0,0,.35);
  --shadow-md:  0 4px 20px rgba(0,0,0,.45);
  --shadow-lg:  0 8px 32px rgba(0,0,0,.55);
  --transition: all .18s ease;
}

/* ---------- BASE RESET ---------------------------------------------------- */
*, *::before, *::after { box-sizing: border-box; }

html, body {
  background: var(--bg-base) !important;
  color: var(--text-1) !important;
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  -webkit-font-smoothing: antialiased;
}

/* ---------- CUSTOM SCROLLBAR ---------------------------------------------- */
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: var(--bg-surface); }
::-webkit-scrollbar-thumb { background: var(--border-3); border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: var(--blue); }

/* ---------- NAVBAR --------------------------------------------------------- */
.navbar {
  background: var(--bg-surface) !important;
  border-bottom: 1px solid var(--border-1) !important;
  box-shadow: var(--shadow-sm) !important;
  padding: 0 20px !important;
}
.navbar-brand {
  font-weight: 800 !important;
  letter-spacing: 1.5px !important;
  font-size: .9rem !important;
  color: var(--text-1) !important;
  padding: 12px 0 !important;
}
.nav-link {
  color: var(--text-2) !important;
  font-size: .82rem !important;
  font-weight: 500 !important;
  letter-spacing: .3px !important;
  padding: 14px 14px !important;
  border-bottom: 2px solid transparent !important;
  transition: var(--transition) !important;
}
.nav-link:hover { color: var(--text-1) !important; }
.nav-link.active {
  color: var(--gold) !important;
  border-bottom-color: var(--gold) !important;
  background: transparent !important;
}

/* ---------- SIDEBAR -------------------------------------------------------- */
.bslib-sidebar-layout > .sidebar {
  background: var(--bg-surface) !important;
  border-right: 1px solid var(--border-1) !important;
  padding: 16px 14px !important;
}

/* ---------- CARDS ---------------------------------------------------------- */
.card {
  background: var(--bg-card) !important;
  border: 1px solid var(--border-1) !important;
  border-radius: var(--radius-lg) !important;
  box-shadow: var(--shadow-sm) !important;
  transition: border-color .2s ease, box-shadow .2s ease !important;
  overflow: hidden !important;
}
.card:hover {
  border-color: var(--border-2) !important;
  box-shadow: var(--shadow-md) !important;
}
.card-header {
  background: var(--bg-raised) !important;
  border-bottom: 1px solid var(--border-1) !important;
  color: var(--text-2) !important;
  font-weight: 600 !important;
  font-size: .75rem !important;
  letter-spacing: 1px !important;
  text-transform: uppercase !important;
  padding: 10px 16px !important;
}
.card-body { padding: 14px !important; }

/* ---------- KPI METRIC BOXES ----------------------------------------------- */
.metric-box {
  background: var(--bg-raised) !important;
  border: 1px solid var(--border-1) !important;
  border-radius: var(--radius-md) !important;
  padding: 14px 16px !important;
  text-align: center !important;
  transition: var(--transition) !important;
  position: relative !important;
  overflow: hidden !important;
}
.metric-box::after {
  content: '' !important;
  position: absolute !important;
  top: 0; left: 0; right: 0 !important;
  height: 2px !important;
  opacity: 0 !important;
  transition: opacity .2s ease !important;
  background: linear-gradient(90deg, var(--blue), transparent) !important;
}
.metric-box:hover { border-color: var(--border-2) !important; }
.metric-box:hover::after { opacity: 1 !important; }

.metric-value {
  font-size: 1.55rem !important;
  font-weight: 700 !important;
  font-family: 'JetBrains Mono', 'Courier New', monospace !important;
  line-height: 1.2 !important;
  letter-spacing: -.5px !important;
}
.metric-label {
  font-size: .67rem !important;
  color: var(--text-3) !important;
  text-transform: uppercase !important;
  letter-spacing: 1px !important;
  margin-top: 4px !important;
}
.metric-pos { color: var(--green) !important; }
.metric-neg { color: var(--red)   !important; }
.metric-neu { color: var(--gold)  !important; }
.metric-blue { color: var(--cyan) !important; }

/* ---------- SECTION HEADERS ------------------------------------------------ */
.section-header {
  color: var(--text-3) !important;
  font-size: .67rem !important;
  font-weight: 700 !important;
  letter-spacing: 1.5px !important;
  text-transform: uppercase !important;
  border-bottom: 1px solid var(--border-1) !important;
  padding-bottom: 5px !important;
  margin: 14px 0 8px !important;
}

/* ---------- FORM CONTROLS -------------------------------------------------- */
.form-control,
.selectize-input,
.form-select {
  background: var(--bg-base) !important;
  border: 1px solid var(--border-2) !important;
  border-radius: var(--radius-sm) !important;
  color: var(--text-1) !important;
  font-size: .82rem !important;
  transition: border-color .15s ease, box-shadow .15s ease !important;
}
.form-control:focus,
.selectize-input.focus {
  border-color: var(--blue) !important;
  box-shadow: 0 0 0 3px rgba(25,104,227,.18) !important;
  outline: none !important;
}
.selectize-dropdown {
  background: var(--bg-raised) !important;
  border: 1px solid var(--border-2) !important;
  border-radius: var(--radius-sm) !important;
}
.selectize-dropdown-content .option {
  color: var(--text-1) !important;
  font-size: .82rem !important;
}
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
  background: var(--blue) !important;
}
label, .control-label { color: var(--text-2) !important; font-size: .78rem !important; }
.irs--shiny .irs-bar { background: var(--blue) !important; border-color: var(--blue-dim) !important; }
.irs--shiny .irs-handle { background: var(--text-1) !important; border-color: var(--border-3) !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: var(--bg-raised) !important; color: var(--gold) !important;
}

/* ---------- BUTTONS -------------------------------------------------------- */
.btn-primary {
  background: var(--blue) !important;
  border-color: var(--blue) !important;
  color: #fff !important;
  font-weight: 600 !important;
  font-size: .8rem !important;
  letter-spacing: .3px !important;
  border-radius: var(--radius-sm) !important;
  transition: var(--transition) !important;
}
.btn-primary:hover {
  background: #2479F5 !important;
  box-shadow: 0 0 0 3px rgba(25,104,227,.3) !important;
}
.btn-outline-secondary {
  background: transparent !important;
  border: 1px solid var(--border-2) !important;
  color: var(--text-2) !important;
  font-size: .8rem !important;
  border-radius: var(--radius-sm) !important;
  transition: var(--transition) !important;
}
.btn-outline-secondary:hover {
  background: var(--bg-hover) !important;
  border-color: var(--border-3) !important;
  color: var(--text-1) !important;
}

/* ---------- DATA TABLES ---------------------------------------------------- */
.dt-table,
table.dataTable {
  background: var(--bg-card) !important;
  color: var(--text-1) !important;
  font-size: .8rem !important;
}
table.dataTable thead th {
  background: var(--bg-raised) !important;
  color: var(--text-2) !important;
  border-bottom: 1px solid var(--border-2) !important;
  font-weight: 600 !important;
  font-size: .72rem !important;
  letter-spacing: .5px !important;
  text-transform: uppercase !important;
}
table.dataTable tbody tr:hover td { background: var(--bg-hover) !important; }
table.dataTable tbody td { border-bottom: 1px solid var(--border-1) !important; }
.dataTables_wrapper .dataTables_filter input {
  background: var(--bg-base) !important;
  border: 1px solid var(--border-2) !important;
  color: var(--text-1) !important;
  border-radius: var(--radius-sm) !important;
}
.dataTables_wrapper .dataTables_length select {
  background: var(--bg-base) !important;
  color: var(--text-1) !important;
}
.dataTables_info, .dataTables_paginate { color: var(--text-2) !important; font-size: .75rem !important; }
.paginate_button { color: var(--text-2) !important; border-radius: 4px !important; }
.paginate_button.current {
  background: var(--blue) !important;
  border-color: var(--blue) !important;
  color: #fff !important;
}

/* ---------- SKELETON LOADING ANIMATION ------------------------------------- */
@keyframes shimmer {
  0%   { background-position: -400px 0; }
  100% { background-position: 400px 0; }
}
.skeleton {
  background: linear-gradient(
    90deg,
    var(--bg-card) 25%,
    var(--bg-hover) 50%,
    var(--bg-card) 75%
  ) !important;
  background-size: 800px 100% !important;
  animation: shimmer 1.6s infinite !important;
  border-radius: var(--radius-sm) !important;
  color: transparent !important;
}

/* ---------- EMPTY STATE ---------------------------------------------------- */
.empty-state {
  display: flex !important;
  flex-direction: column !important;
  align-items: center !important;
  justify-content: center !important;
  padding: 48px 24px !important;
  color: var(--text-3) !important;
  text-align: center !important;
}
.empty-state .empty-icon {
  font-size: 2.5rem !important;
  margin-bottom: 12px !important;
  opacity: .5 !important;
}
.empty-state .empty-title {
  font-size: .95rem !important;
  font-weight: 600 !important;
  color: var(--text-2) !important;
  margin-bottom: 6px !important;
}
.empty-state .empty-desc {
  font-size: .8rem !important;
  line-height: 1.5 !important;
  max-width: 280px !important;
}

/* ---------- NOTIFICATION/BADGE --------------------------------------------- */
.badge-quant {
  background: var(--bg-raised) !important;
  color: var(--gold) !important;
  border: 1px solid var(--border-2) !important;
  padding: 3px 10px !important;
  border-radius: 20px !important;
  font-size: .68rem !important;
  font-weight: 700 !important;
  letter-spacing: .5px !important;
}
.api-status {
  display: inline-flex !important;
  align-items: center !important;
  gap: 5px !important;
  font-size: .72rem !important;
  color: var(--text-3) !important;
  padding: 3px 8px !important;
}
.api-dot {
  width: 6px !important;
  height: 6px !important;
  border-radius: 50% !important;
  display: inline-block !important;
}
.api-dot.ok   { background: var(--green) !important; box-shadow: 0 0 6px var(--green) !important; }
.api-dot.warn { background: var(--gold)  !important; }
.api-dot.err  { background: var(--red)   !important; }

/* ---------- COUNTRY CARDS -------------------------------------------------- */
.country-flag {
  font-size: 1.6rem !important;
  line-height: 1 !important;
}

/* ---------- CHECKBOX GROUP ------------------------------------------------- */
.checkbox label, .radio label {
  color: var(--text-2) !important;
  font-size: .8rem !important;
}

/* ---------- PLOTLY CHART CONTAINERS ---------------------------------------- */
.js-plotly-plot .plotly .modebar {
  background: transparent !important;
}
"

# ---- Helper: KPI metric box -------------------------------------------------
metric_box <- function(id_val, label, colour_class = "metric-neu") {
  div(
    class = "metric-box",
    div(
      class = paste("metric-value", colour_class),
      textOutput(id_val, inline = TRUE)
    ),
    div(class = "metric-label", label)
  )
}

# ---- Helper: empty state placeholder ----------------------------------------
empty_state <- function(icon = "bi-bar-chart", title = "No data yet",
                         desc = "Configure the settings and click Run.") {
  div(
    class = "empty-state",
    tags$div(class = paste("bi", icon, "empty-icon"), style = "font-size:2.2rem;"),
    tags$div(class = "empty-title", title),
    tags$div(class = "empty-desc", desc)
  )
}

# ---- Country Explorer choices (populated from world_bank.R via global.R) ----
# G20_COUNTRY_NAMES, WB_INDICATORS, WB_INDICATOR_LABELS are available because
# world_bank.R is sourced in global.R before ui.R is evaluated.
wb_country_choices <- setNames(
  G20_COUNTRIES,
  paste(G20_FLAGS[G20_COUNTRIES], G20_COUNTRY_NAMES[G20_COUNTRIES])
)

wb_indicator_choices <- setNames(
  names(WB_INDICATORS)[names(WB_INDICATORS) %in% names(WB_INDICATOR_LABELS)],
  WB_INDICATOR_LABELS[names(WB_INDICATORS)[names(WB_INDICATORS) %in% names(WB_INDICATOR_LABELS)]]
)

# =============================================================================
# PAGE LAYOUT
# =============================================================================

ui <- bslib::page_navbar(
  title = tags$span(
    tags$span("Q", style = paste0("color:", CLR$gold, ";font-weight:900;")),
    tags$span("UANT", style = "letter-spacing:2px;font-weight:800;"),
    tags$span(" RESEARCH", style = paste0("color:", CLR$text_2, ";font-weight:400;letter-spacing:3px;font-size:.85em;"))
  ),
  theme = bslib::bs_theme(
    version   = 5,
    bg        = CLR$bg_base,
    fg        = CLR$text_1,
    primary   = CLR$blue,
    secondary = CLR$text_2,
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("JetBrains Mono"),
    `navbar-bg` = CLR$bg_surface
  ),
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(platform_css)),
      tags$link(
        rel  = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
      )
    )
  ),
  selected = "Portfolio",
  window_title = "Quant Research Platform",

  # ==========================================================================
  # TAB 1: PORTFOLIO ANALYTICS
  # ==========================================================================
  bslib::nav_panel(
    title = tagList(tags$i(class = "bi bi-graph-up me-1"), "Portfolio"),
    value = "Portfolio",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 265,
        open  = TRUE,

        div(class = "section-header", "Assets & Period"),
        selectizeInput(
          "tickers", "Select Assets",
          choices  = c("SPY","QQQ","AGG","GLD","TLT","EEM","VNQ","GSG","BTC-USD","ETH-USD"),
          selected = c("SPY","QQQ","AGG","GLD","TLT"),
          multiple = TRUE,
          options  = list(plugins = list("remove_button"), maxItems = 10,
                          placeholder = "Add tickers...")
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
        selectInput("opt_method", "Portfolio Method",
                    choices = c(
                      "Equal Weight"   = "equal",
                      "Min Variance"   = "min_var",
                      "Max Sharpe"     = "max_sharpe",
                      "Custom Weights" = "custom"
                    )),
        conditionalPanel(
          condition = "input.opt_method == 'custom'",
          textInput("custom_weights", "Weights (comma-separated)", placeholder = "0.3,0.2,0.2,0.15,0.15")
        ),
        br(),
        actionButton("run_portfolio", tagList(tags$i(class = "bi bi-play-fill me-1"), "Run Analysis"),
                     class = "btn btn-primary w-100"),
        br(), br(),
        div(class = "api-status",
            tags$span(class = "api-dot warn"),
            "Yahoo Finance (via quantmod)")
      ),

      # KPI row
      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        metric_box("kpi_total_ret", "Total Return",    "metric-pos"),
        metric_box("kpi_ann_ret",   "Ann. Return",     "metric-neu"),
        metric_box("kpi_vol",       "Ann. Volatility", "metric-neg"),
        metric_box("kpi_sharpe",    "Sharpe Ratio",    "metric-neu"),
        metric_box("kpi_mdd",       "Max Drawdown",    "metric-neg"),
        metric_box("kpi_var95",     "VaR 95% (1-day)", "metric-blue")
      ),
      br(),
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-activity me-1"), "NAV Performance (Rebased 100)")),
          plotly::plotlyOutput("plot_nav", height = "340px")
        ),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-pie-chart me-1"), "Portfolio Weights")),
          plotly::plotlyOutput("plot_weights", height = "340px")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-water me-1"), "Drawdown (Underwater)")),
          plotly::plotlyOutput("plot_drawdown", height = "280px")
        ),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-tsunami me-1"), "Rolling Volatility (63-day)")),
          plotly::plotlyOutput("plot_roll_vol", height = "280px")
        )
      ),
      bslib::card(
        bslib::card_header(tagList(tags$i(class = "bi bi-table me-1"), "Per-Asset Statistics")),
        DT::dataTableOutput("tbl_stats")
      )
    )
  ),

  # ==========================================================================
  # TAB 2: CAPM & FACTOR ANALYSIS
  # ==========================================================================
  bslib::nav_panel(
    title = tagList(tags$i(class = "bi bi-calculator me-1"), "CAPM / Factor"),
    value = "CAPM",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 245,
        div(class = "section-header", "Settings"),
        selectizeInput("capm_tickers", "Assets",
          choices  = c("AAPL","MSFT","GOOGL","AMZN","META","TSLA","NVDA","JPM","GS","BRK-B"),
          selected = c("AAPL","MSFT","GOOGL","AMZN"),
          multiple = TRUE,
          options  = list(plugins = list("remove_button"))
        ),
        selectInput("capm_benchmark", "Market Proxy",
          choices = c("SPY","QQQ","VTI"), selected = "SPY"),
        dateRangeInput("capm_dates", "Period",
          start = Sys.Date() - 365 * 3, end = Sys.Date()),
        br(),
        actionButton("run_capm", tagList(tags$i(class = "bi bi-play-fill me-1"), "Compute CAPM"),
          class = "btn btn-primary w-100")
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Security Characteristic Lines"),
          plotly::plotlyOutput("plot_scl", height = "380px")
        ),
        bslib::card(
          bslib::card_header("Alpha vs Beta — Risk Decomposition"),
          plotly::plotlyOutput("plot_alpha_beta", height = "380px")
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
    title = tagList(tags$i(class = "bi bi-shuffle me-1"), "Monte Carlo"),
    value = "MonteCarlo",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 265,
        div(class = "section-header", "Simulation Parameters"),
        selectInput("mc_ticker",  "Asset", choices = c("SPY","AAPL","BTC-USD","GLD")),
        numericInput("mc_paths",  "Simulated Paths",   value = 5000, min = 100,  max = 50000),
        numericInput("mc_horizon","Horizon (days)",    value = 252,  min = 1,    max = 1260),
        numericInput("mc_S0",     "Initial Price ($)", value = 100,  min = 0.01),

        div(class = "section-header", "Process"),
        selectInput("mc_model", "Model",
          choices = c("Geometric Brownian Motion" = "gbm",
                      "Historical Bootstrap"      = "historical")),
        numericInput("mc_drift", "Annual Drift (%)",     value = 10, step = 0.5),
        numericInput("mc_vol",   "Annual Volatility (%)", value = 20, step = 0.5),
        br(),
        actionButton("run_mc", tagList(tags$i(class = "bi bi-play-fill me-1"), "Run Simulation"),
          class = "btn btn-primary w-100"),
        downloadButton("dl_mc_paths", tagList(tags$i(class = "bi bi-download me-1"), "Export Paths"),
          class = "btn btn-outline-secondary w-100 mt-1")
      ),

      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        metric_box("mc_mean_terminal", "Mean Terminal",    "metric-neu"),
        metric_box("mc_p75",           "75th Percentile", "metric-pos"),
        metric_box("mc_p25",           "25th Percentile", "metric-neg"),
        metric_box("mc_var95",         "VaR 95%",         "metric-neg"),
        metric_box("mc_es95",          "ES 95%",          "metric-neg"),
        metric_box("mc_prob_loss",     "P(Loss)",         "metric-blue")
      ),
      br(),
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header("Simulated Price Paths — Fan Chart"),
          plotly::plotlyOutput("plot_mc_paths", height = "400px")
        ),
        bslib::card(
          bslib::card_header("Terminal Return Distribution"),
          plotly::plotlyOutput("plot_mc_dist", height = "400px")
        )
      )
    )
  ),

  # ==========================================================================
  # TAB 4: MACROECONOMIC EXPLORER
  # ==========================================================================
  bslib::nav_panel(
    title = tagList(tags$i(class = "bi bi-bank me-1"), "Macro Explorer"),
    value = "Macro",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 265,
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
        div(class = "section-header", "Period"),
        dateRangeInput("macro_dates", NULL,
          start = "2000-01-01", end = Sys.Date()),
        br(),
        actionButton("run_macro", tagList(tags$i(class = "bi bi-play-fill me-1"), "Load Data"),
          class = "btn btn-primary w-100"),
        div(class = "api-status mt-2",
            tags$span(class = "api-dot warn"),
            "FRED (key recommended)")
      ),

      bslib::card(
        bslib::card_header("Macroeconomic Indicators Panel"),
        plotOutput("plot_macro_panel", height = "580px")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("US Yield Curve (Latest)"),
          plotly::plotlyOutput("plot_yield_curve", height = "360px")
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
    title = tagList(tags$i(class = "bi bi-bar-chart-steps me-1"), "Forecasting"),
    value = "Forecasting",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 265,
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
        br(),
        actionButton("run_forecast", tagList(tags$i(class = "bi bi-play-fill me-1"), "Generate Forecast"),
          class = "btn btn-primary w-100")
      ),

      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        metric_box("fc_model_label", "Model",          "metric-neu"),
        metric_box("fc_aic",         "AICc",           "metric-neu"),
        metric_box("fc_rmse",        "RMSE (in-sample)","metric-blue"),
        metric_box("fc_mae",         "MAE",            "metric-blue"),
        metric_box("fc_mape",        "MAPE (%)",       "metric-blue"),
        metric_box("fc_ljung",       "Ljung-Box p-val","metric-neu")
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
  # TAB 6: COUNTRY EXPLORER  (NEW — addresses the "country data" bug)
  # ==========================================================================
  bslib::nav_panel(
    title = tagList(tags$i(class = "bi bi-globe2 me-1"), "Countries"),
    value = "Countries",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,

        div(class = "section-header", "Indicator"),
        selectInput(
          "wb_indicator", NULL,
          choices  = wb_indicator_choices,
          selected = "gdp_per_capita_usd"
        ),

        div(class = "section-header", "Reference Year"),
        sliderInput(
          "wb_year", NULL,
          min   = 2000,
          max   = as.integer(format(Sys.Date(), "%Y")) - 1L,
          value = as.integer(format(Sys.Date(), "%Y")) - 2L,
          step  = 1,
          sep   = ""
        ),

        div(class = "section-header", "Countries"),
        div(
          style = "max-height:260px;overflow-y:auto;padding-right:4px;",
          checkboxGroupInput(
            "wb_countries", NULL,
            choices  = wb_country_choices,
            selected = G20_COUNTRIES
          )
        ),
        actionLink("wb_select_all",   "Select all",  style = "font-size:.75rem;color:var(--blue);"),
        tags$span(" / ", style = "font-size:.75rem;color:var(--text-3);"),
        actionLink("wb_deselect_all", "Clear",       style = "font-size:.75rem;color:var(--text-3);"),

        br(), br(),
        actionButton("run_wb", tagList(tags$i(class = "bi bi-play-fill me-1"), "Fetch Data"),
          class = "btn btn-primary w-100"),
        div(class = "api-status mt-2",
            tags$span(class = "api-dot ok"),
            "World Bank API (no key needed)")
      ),

      # KPI summary row
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        metric_box("wb_kpi_max_country",  "Highest",  "metric-pos"),
        metric_box("wb_kpi_min_country",  "Lowest",   "metric-neg"),
        metric_box("wb_kpi_median",       "G20 Median","metric-neu"),
        metric_box("wb_kpi_us",           "United States","metric-blue")
      ),
      br(),

      # Main charts
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-bar-chart-horizontal me-1"), "Country Comparison")),
          plotly::plotlyOutput("plot_wb_bar", height = "440px")
        ),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-circle-square me-1"), "Development Scatter — GDP vs Life Expectancy")),
          plotly::plotlyOutput("plot_wb_bubble", height = "440px")
        )
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-graph-up me-1"), "Time-Series Trend")),
          plotly::plotlyOutput("plot_wb_trend", height = "320px")
        ),
        bslib::card(
          bslib::card_header(tagList(tags$i(class = "bi bi-table me-1"), "Country Data Table")),
          DT::dataTableOutput("tbl_wb", height = "320px")
        )
      )
    )
  ),

  # ==========================================================================
  # TAB 7: REPORT GENERATOR
  # ==========================================================================
  bslib::nav_panel(
    title = tagList(tags$i(class = "bi bi-file-earmark-pdf me-1"), "Reports"),
    value = "Reports",

    bslib::layout_columns(
      col_widths = c(4, 8),

      bslib::card(
        bslib::card_header("Generate Report"),
        bslib::card_body(
          selectInput("report_type", "Report Template",
            choices = c(
              "Portfolio Summary"     = "portfolio",
              "Economic Outlook"      = "macro",
              "Risk Report"           = "risk",
              "ARIMA Forecast Report" = "forecast",
              "Full Platform Report"  = "full"
            )),
          selectInput("report_format", "Output Format",
            choices = c("HTML" = "html", "PDF" = "pdf")),
          textInput("report_title",  "Custom Title",  placeholder = "Optional"),
          textInput("report_author", "Author",        placeholder = "Quant Research Platform"),
          checkboxInput("report_include_charts", "Include all charts",  value = TRUE),
          checkboxInput("report_include_tables", "Include data tables", value = TRUE),
          br(),
          actionButton("gen_report", tagList(tags$i(class = "bi bi-file-pdf me-1"), "Prepare Report"),
            class = "btn btn-primary w-100"),
          br(), br(),
          downloadButton("dl_report", tagList(tags$i(class = "bi bi-download me-1"), "Download Report"),
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

  # Navigation right-side items
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$span(class = "badge-quant", "v1.1.0")
  )
)
