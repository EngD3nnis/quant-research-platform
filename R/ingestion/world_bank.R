# =============================================================================
# World Bank API Ingestion Module
# Fetches cross-country macroeconomic and development indicators via {WDI}.
#
# Bug fixed (v1.1):
#   dplyr::select(all_of(unlist(indicators))) would crash if WDI did not return
#   all requested codes. We now intersect with actual column names and log which
#   indicators were missing so the team can update deprecated codes.
#
# Reliability strategy:
#   - Cache for 7 days (WB data updates annually/quarterly)
#   - Retry up to 3 times on transient network failures
#   - Use extra=FALSE to avoid column-name ambiguity from metadata columns
# =============================================================================

suppressPackageStartupMessages({
  library(WDI)
  library(dplyr)
  library(glue)
  library(purrr)
})

source(here::here("R", "utilities", "logger.R"))
source(here::here("R", "utilities", "config.R"))

# WB data changes at most annually — cache for 7 days
.WB_CACHE_MAX_AGE_HOURS <- 7L * 24L

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

#' Human-readable labels for WB indicators (for chart axes / UI labels)
#' @export
WB_INDICATOR_LABELS <- c(
  gdp_per_capita_usd  = "GDP per Capita (USD)",
  gdp_growth_pct      = "GDP Growth (%)",
  gdp_ppp             = "GDP PPP (current intl $)",
  inflation_cpi       = "CPI Inflation (%)",
  current_account_gdp = "Current Account (% GDP)",
  fdi_net_inflows     = "FDI Net Inflows (% GDP)",
  exports_gdp         = "Exports (% GDP)",
  imports_gdp         = "Imports (% GDP)",
  govt_debt_gdp       = "Government Debt (% GDP)",
  population          = "Population",
  poverty_headcount   = "Poverty Headcount (%)",
  gini_index          = "Gini Index",
  life_expectancy     = "Life Expectancy (years)",
  literacy_rate       = "Literacy Rate (%)",
  internet_penetration = "Internet Users (% Pop)"
)

#' G20 country ISO2 codes
#' @export
G20_COUNTRIES <- c(
  "AR","AU","BR","CA","CN","DE","FR","GB","ID","IN",
  "IT","JP","KR","MX","RU","SA","TR","US","ZA"
)

#' Full country names for G20 members (keyed by ISO2 code)
#' Used by the Country Explorer UI for readable labels and flag emojis.
#' @export
G20_COUNTRY_NAMES <- c(
  AR = "Argentina",
  AU = "Australia",
  BR = "Brazil",
  CA = "Canada",
  CN = "China",
  DE = "Germany",
  FR = "France",
  GB = "United Kingdom",
  ID = "Indonesia",
  IN = "India",
  IT = "Italy",
  JP = "Japan",
  KR = "South Korea",
  MX = "Mexico",
  RU = "Russia",
  SA = "Saudi Arabia",
  TR = "Turkey",
  US = "United States",
  ZA = "South Africa"
)

#' Country flag emojis keyed by ISO2 code
#' @export
G20_FLAGS <- c(
  AR = "\U0001F1E6\U0001F1F7",  # 🇦🇷
  AU = "\U0001F1E6\U0001F1FA",  # 🇦🇺
  BR = "\U0001F1E7\U0001F1F7",  # 🇧🇷
  CA = "\U0001F1E8\U0001F1E6",  # 🇨🇦
  CN = "\U0001F1E8\U0001F1F3",  # 🇨🇳
  DE = "\U0001F1E9\U0001F1EA",  # 🇩🇪
  FR = "\U0001F1EB\U0001F1F7",  # 🇫🇷
  GB = "\U0001F1EC\U0001F1E7",  # 🇬🇧
  ID = "\U0001F1EE\U0001F1E9",  # 🇮🇩
  IN = "\U0001F1EE\U0001F1F3",  # 🇮🇳
  IT = "\U0001F1EE\U0001F1F9",  # 🇮🇹
  JP = "\U0001F1EF\U0001F1F5",  # 🇯🇵
  KR = "\U0001F1F0\U0001F1F7",  # 🇰🇷
  MX = "\U0001F1F2\U0001F1FD",  # 🇲🇽
  RU = "\U0001F1F7\U0001F1FA",  # 🇷🇺
  SA = "\U0001F1F8\U0001F1E6",  # 🇸🇦
  TR = "\U0001F1F9\U0001F1F7",  # 🇹🇷
  US = "\U0001F1FA\U0001F1F8",  # 🇺🇸
  ZA = "\U0001F1FF\U0001F1E6"   # 🇿🇦
)

# ---- Core Fetcher ------------------------------------------------------------

#' Fetch World Bank indicators for a set of countries
#'
#' @param indicators Named character vector of WB indicator codes
#'                   (use WB_INDICATORS or supply directly)
#' @param countries  ISO2 country codes (default: G20)
#' @param start_year Integer start year
#' @param end_year   Integer end year
#' @param use_cache  Return cached file if still fresh (< 7 days old)
#' @param cfg        Configuration list
#' @return Long-format tibble: country, iso2c, year, indicator, indicator_code, value
#'         Returns NULL if the WDI call fails entirely.
#' @export
fetch_world_bank <- function(indicators  = WB_INDICATORS,
                              countries   = G20_COUNTRIES,
                              start_year  = 2000L,
                              end_year    = as.integer(format(Sys.Date(), "%Y")) - 1L,
                              use_cache   = TRUE,
                              cfg         = get_config()) {

  cache_path <- file.path(
    cfg$paths$data_raw,
    glue("world_bank_{start_year}_{end_year}_{length(indicators)}ind_{length(countries)}ctry.rds")
  )

  if (use_cache && file.exists(cache_path)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(cache_path), units = "hours"))
    if (age_h < .WB_CACHE_MAX_AGE_HOURS) {
      log_info("[wb] Cache hit ({round(age_h, 1)} h old) — {nrow(readRDS(cache_path))} rows")
      return(readRDS(cache_path))
    }
    log_info("[wb] Cache stale ({round(age_h, 1)} h) — refreshing from World Bank API")
  }

  indicator_codes <- unlist(indicators)
  log_info("[wb] Fetching {length(indicator_codes)} indicators for {length(countries)} countries ({start_year}-{end_year})")

  # Retry with backoff (WB API can be slow)
  raw        <- NULL
  last_error <- "unknown error"

  for (attempt in seq_len(3L)) {
    raw <- tryCatch(
      WDI::WDI(
        country   = countries,
        indicator = indicator_codes,
        start     = start_year,
        end       = end_year,
        extra     = FALSE   # avoid extra metadata columns that can interfere with select
      ),
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(raw) && nrow(raw) > 0L) break
    if (attempt < 3L) {
      wait <- 2^attempt
      log_warn("[wb] WDI attempt {attempt}/3 failed — retrying in {wait}s. Error: {last_error}")
      Sys.sleep(wait)
    }
  }

  if (is.null(raw) || nrow(raw) == 0L) {
    log_error("[wb] WDI fetch failed after 3 attempts: {last_error}")
    return(NULL)
  }

  # ---- Robust column selection -----------------------------------------------
  # WDI may not return all requested indicator codes (deprecated or unavailable).
  # Only select the codes that actually appear in the result.
  available_codes <- intersect(indicator_codes, names(raw))
  missing_codes   <- setdiff(indicator_codes, names(raw))

  if (length(missing_codes) > 0L) {
    log_warn("[wb] {length(missing_codes)} indicator(s) not returned by WDI: {paste(missing_codes, collapse=', ')}")
  }

  if (length(available_codes) == 0L) {
    log_error("[wb] WDI returned no indicator columns — check that indicator codes are still valid")
    return(NULL)
  }

  # Inverse map: WB code -> user-friendly key (only for returned codes)
  inv_map <- setNames(names(indicators), unlist(indicators))
  inv_map <- inv_map[available_codes]

  df <- raw |>
    tibble::as_tibble() |>
    dplyr::select(country, iso2c, year, dplyr::all_of(available_codes)) |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(available_codes),
      names_to  = "indicator_code",
      values_to = "value"
    ) |>
    dplyr::mutate(
      indicator = inv_map[indicator_code],
      year      = as.integer(year)
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::select(country, iso2c, year, indicator, indicator_code, value)

  log_info("[wb] Fetched {nrow(df)} observations across {length(available_codes)} indicators")

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, cache_path)
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
  if (!indicator_key %in% names(WB_INDICATORS)) {
    stop(glue("[wb] Unknown indicator key: '{indicator_key}'. Valid keys: {paste(names(WB_INDICATORS), collapse=', ')}"))
  }

  df <- fetch_world_bank(
    indicators = WB_INDICATORS[indicator_key],
    countries  = countries,
    cfg        = cfg
  )

  if (is.null(df)) return(NULL)

  if (!is.null(year)) {
    df <- dplyr::filter(df, .data$year == !!as.integer(year))
  } else {
    df <- df |>
      dplyr::group_by(country, iso2c) |>
      dplyr::slice_max(year, n = 1L) |>
      dplyr::ungroup()
  }

  df
}
