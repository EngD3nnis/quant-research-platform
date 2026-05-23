# =============================================================================
# FRED (Federal Reserve Economic Data) Ingestion Module
# Fetches macroeconomic time series via the fredr package.
# Covers: GDP, CPI, unemployment, interest rates, yield curves, etc.
# =============================================================================

suppressPackageStartupMessages({
  library(fredr)
  library(dplyr)
  library(lubridate)
  library(glue)
  library(purrr)
})

source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "config.R"))

# ---- Well-Known Series Catalogue ---------------------------------------------

#' Curated catalogue of FRED series used by the platform
#' @export
FRED_SERIES <- list(
  # Growth & Activity
  gdp_real          = list(id = "GDPC1",   name = "Real GDP (Chained 2012 USD)", freq = "quarterly"),
  gdp_nominal       = list(id = "GDP",     name = "Nominal GDP (USD bn)",        freq = "quarterly"),
  industrial_prod   = list(id = "INDPRO",  name = "Industrial Production Index", freq = "monthly"),
  retail_sales      = list(id = "RSAFS",   name = "Retail Sales",                freq = "monthly"),

  # Inflation
  cpi_all           = list(id = "CPIAUCSL",  name = "CPI All Urban Consumers (SA)", freq = "monthly"),
  cpi_core          = list(id = "CPILFESL",  name = "Core CPI (ex Food & Energy)", freq = "monthly"),
  pce               = list(id = "PCE",       name = "Personal Consumption Expenditures", freq = "monthly"),
  pce_core          = list(id = "PCEPILFE",  name = "Core PCE",                   freq = "monthly"),

  # Labor Market
  unemployment      = list(id = "UNRATE",    name = "Unemployment Rate",          freq = "monthly"),
  nonfarm_payrolls  = list(id = "PAYEMS",    name = "Nonfarm Payrolls (000s)",    freq = "monthly"),
  labour_force_part = list(id = "CIVPART",   name = "Labor Force Participation",  freq = "monthly"),

  # Interest Rates & Credit
  fed_funds         = list(id = "FEDFUNDS",  name = "Federal Funds Rate",         freq = "monthly"),
  t_bill_3m         = list(id = "TB3MS",     name = "3-Month T-Bill Rate",        freq = "monthly"),
  t_note_2y         = list(id = "DGS2",      name = "2-Year Treasury Yield",      freq = "daily"),
  t_note_10y        = list(id = "DGS10",     name = "10-Year Treasury Yield",     freq = "daily"),
  t_bond_30y        = list(id = "DGS30",     name = "30-Year Treasury Yield",     freq = "daily"),
  yield_spread      = list(id = "T10Y2Y",    name = "10Y-2Y Yield Spread",        freq = "daily"),
  credit_spread_hy  = list(id = "BAMLH0A0HYM2", name = "HY Credit Spread (OAS)", freq = "daily"),

  # Money Supply & Financial Conditions
  m2_money          = list(id = "M2SL",      name = "M2 Money Supply",            freq = "monthly"),
  vix               = list(id = "VIXCLS",    name = "CBOE Volatility Index",      freq = "daily"),

  # Housing
  housing_starts    = list(id = "HOUST",     name = "Housing Starts (000s units)",freq = "monthly"),
  case_shiller      = list(id = "CSUSHPINSA",name = "Case-Shiller Home Price Index", freq = "monthly")
)

# ---- Core Fetcher ------------------------------------------------------------

#' Fetch a single FRED series
#'
#' @param series_id  FRED series identifier (e.g. "UNRATE")
#' @param from       Start date
#' @param to         End date
#' @param use_cache  Return cached data if available
#' @param cfg        Configuration list
#' @return tibble with columns: date, value, series_id, units, title
#' @export
fetch_fred <- function(series_id,
                       from      = "2000-01-01",
                       to        = Sys.Date(),
                       use_cache = TRUE,
                       cfg       = get_config()) {

  # Authenticate with FRED API key
  api_key <- cfg$data_sources$fred$api_key
  if (nchar(api_key) > 0) {
    fredr::fredr_set_key(api_key)
  } else {
    log_warn("[fred] No API key set — using public endpoint (rate limited)")
  }

  cache_path <- file.path(
    cfg$paths$data_raw,
    glue("fred_{series_id}_{from}_{to}.rds")
  )

  if (use_cache && file.exists(cache_path)) {
    log_info("[fred] Cache hit for {series_id}")
    return(readRDS(cache_path))
  }

  log_info("[fred] Fetching series: {series_id}")

  info <- tryCatch(fredr::fredr_series(series_id), error = function(e) NULL)
  obs  <- tryCatch(
    fredr::fredr(
      series_id         = series_id,
      observation_start = as.Date(from),
      observation_end   = as.Date(to)
    ),
    error = function(e) {
      log_error("[fred] Failed {series_id}: {conditionMessage(e)}")
      return(NULL)
    }
  )

  if (is.null(obs)) return(NULL)

  df <- obs |>
    dplyr::select(date, value) |>
    dplyr::mutate(
      series_id = series_id,
      title     = if (!is.null(info)) info$title else series_id,
      units     = if (!is.null(info)) info$units else "unknown"
    ) |>
    dplyr::filter(!is.na(value))

  saveRDS(df, cache_path)
  log_info("[fred] Fetched {nrow(df)} observations for {series_id}")
  df
}

#' Fetch a named set of FRED series and return combined long-format tibble
#'
#' @param series_keys Character vector of keys from FRED_SERIES catalogue
#'                    OR explicit FRED series IDs
#' @param from        Start date
#' @param to          End date
#' @param cfg         Configuration list
#' @return Long-format tibble: date, series_id, value, title, units
#' @export
fetch_fred_multi <- function(series_keys = names(FRED_SERIES),
                              from        = "2000-01-01",
                              to          = Sys.Date(),
                              cfg         = get_config()) {

  # Resolve catalogue keys → FRED series IDs
  ids <- purrr::map_chr(series_keys, function(k) {
    if (k %in% names(FRED_SERIES)) FRED_SERIES[[k]]$id else k
  })

  results <- purrr::map(ids, function(id) {
    tryCatch(
      fetch_fred(id, from, to, cfg = cfg),
      error = function(e) {
        log_warn("[fred] Skipping {id}: {conditionMessage(e)}")
        NULL
      }
    )
  })

  dplyr::bind_rows(purrr::compact(results))
}

#' Compute the US yield curve (daily snapshot at a given date)
#'
#' @param date Target date (Date scalar)
#' @param cfg  Configuration list
#' @return tibble: maturity_years, yield, date
#' @export
fetch_yield_curve <- function(date = Sys.Date(), cfg = get_config()) {
  maturities <- c(
    "0.25" = "TB3MS",
    "0.5"  = "TB6MS",
    "1"    = "DGS1",
    "2"    = "DGS2",
    "3"    = "DGS3",
    "5"    = "DGS5",
    "7"    = "DGS7",
    "10"   = "DGS10",
    "20"   = "DGS20",
    "30"   = "DGS30"
  )

  from <- date - 30  # window to find the closest available observation

  purrr::imap_dfr(maturities, function(id, mat) {
    df <- fetch_fred(id, from = from, to = date, cfg = cfg)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df |>
      dplyr::slice_tail(n = 1) |>
      dplyr::mutate(maturity_years = as.numeric(mat)) |>
      dplyr::select(date, maturity_years, yield = value)
  })
}
