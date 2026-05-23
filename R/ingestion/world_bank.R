# =============================================================================
# World Bank API Ingestion Module
# Fetches cross-country macroeconomic indicators via the {WDI} package.
# Covers: GDP per capita, inflation, trade, debt, development indicators.
# =============================================================================

suppressPackageStartupMessages({
  library(WDI)
  library(dplyr)
  library(glue)
  library(purrr)
})

source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "config.R"))

# ---- Indicator Catalogue -----------------------------------------------------

#' World Bank indicator catalogue
#' @export
WB_INDICATORS <- list(
  gdp_per_capita_usd    = "NY.GDP.PCAP.CD",
  gdp_growth_pct        = "NY.GDP.MKTP.KD.ZG",
  gdp_ppp               = "NY.GDP.MKTP.PP.CD",
  inflation_cpi         = "FP.CPI.TOTL.ZG",
  current_account_gdp   = "BN.CAB.XOKA.GD.ZS",
  fdi_net_inflows       = "BX.KLT.DINV.WD.GD.ZS",
  exports_gdp           = "NE.EXP.GNFS.ZS",
  imports_gdp           = "NE.IMP.GNFS.ZS",
  govt_debt_gdp         = "GC.DOD.TOTL.GD.ZS",
  population            = "SP.POP.TOTL",
  poverty_headcount     = "SI.POV.DDAY",
  gini_index            = "SI.POV.GINI",
  life_expectancy       = "SP.DYN.LE00.IN",
  literacy_rate         = "SE.ADT.LITR.ZS",
  internet_penetration  = "IT.NET.USER.ZS"
)

#' G20 country ISO2 codes
#' @export
G20_COUNTRIES <- c(
  "AR","AU","BR","CA","CN","DE","FR","GB","ID","IN",
  "IT","JP","KR","MX","RU","SA","TR","US","ZA"
)

# ---- Core Fetcher ------------------------------------------------------------

#' Fetch World Bank indicators for a set of countries
#'
#' @param indicators Named character vector of WB indicator codes
#'                   (use WB_INDICATORS or supply directly)
#' @param countries  ISO2 country codes (default: G20)
#' @param start_year Integer start year
#' @param end_year   Integer end year
#' @param use_cache  Return cached file if available
#' @param cfg        Configuration list
#' @return Long-format tibble: country, iso2c, year, indicator, value
#' @export
fetch_world_bank <- function(indicators  = WB_INDICATORS,
                              countries   = G20_COUNTRIES,
                              start_year  = 2000L,
                              end_year    = as.integer(format(Sys.Date(), "%Y")) - 1L,
                              use_cache   = TRUE,
                              cfg         = get_config()) {

  cache_path <- file.path(
    cfg$paths$data_raw,
    glue("world_bank_{start_year}_{end_year}.rds")
  )

  if (use_cache && file.exists(cache_path)) {
    log_info("[wb] Cache hit for World Bank data")
    return(readRDS(cache_path))
  }

  log_info("[wb] Fetching {length(indicators)} indicators for {length(countries)} countries")

  raw <- tryCatch(
    WDI::WDI(
      country   = countries,
      indicator = unlist(indicators),
      start     = start_year,
      end       = end_year,
      extra     = TRUE
    ),
    error = function(e) {
      log_error("[wb] WDI fetch failed: {conditionMessage(e)}")
      return(NULL)
    }
  )

  if (is.null(raw)) return(NULL)

  # Pivot to long format with human-readable indicator names
  inv_map <- setNames(names(indicators), unlist(indicators))

  df <- raw |>
    tibble::as_tibble() |>
    dplyr::select(country, iso2c, year, all_of(unlist(indicators))) |>
    tidyr::pivot_longer(
      cols      = -c(country, iso2c, year),
      names_to  = "indicator_code",
      values_to = "value"
    ) |>
    dplyr::mutate(
      indicator = inv_map[indicator_code],
      year      = as.integer(year)
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::select(country, iso2c, year, indicator, indicator_code, value)

  saveRDS(df, cache_path)
  log_info("[wb] Saved {nrow(df)} observations")
  df
}

#' Fetch a cross-country comparison for a single indicator
#'
#' @param indicator_key Key from WB_INDICATORS (e.g. "gdp_per_capita_usd")
#' @param countries     ISO2 country codes
#' @param year          Single reference year (most recent if NULL)
#' @param cfg           Configuration list
#' @return tibble: country, iso2c, year, value
#' @export
fetch_wb_crosssection <- function(indicator_key,
                                   countries = G20_COUNTRIES,
                                   year      = NULL,
                                   cfg       = get_config()) {
  df <- fetch_world_bank(
    indicators = WB_INDICATORS[indicator_key],
    countries  = countries,
    cfg        = cfg
  )

  if (!is.null(year)) {
    df <- dplyr::filter(df, .data$year == !!year)
  } else {
    df <- df |>
      dplyr::group_by(country, iso2c) |>
      dplyr::slice_max(year, n = 1) |>
      dplyr::ungroup()
  }

  df
}
