# =============================================================================
# Institutional Visualization Theme
# A publication-quality ggplot2 theme modelled on Bloomberg/FT/WSJ aesthetics.
# Consistent use across all platform outputs ensures a professional identity.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# ---- Colour Palettes ---------------------------------------------------------

#' Institutional colour palette
#' @export
PALETTE <- list(
  # Primary action colours
  primary    = "#1A3A5C",   # Deep navy — main lines, titles
  secondary  = "#C0392B",   # Institutional red — contrast, alerts
  accent     = "#2980B9",   # Cornflower blue — secondary series
  gold       = "#D4A017",   # Quantitative gold — highlights
  green      = "#27AE60",   # Positive / returns
  amber      = "#E67E22",   # Warning / moderate risk
  red        = "#C0392B",   # Negative / high risk

  # Surface colours
  background = "#FAFAFA",
  panel_bg   = "#FFFFFF",
  grid_major = "#E8E8E8",
  grid_minor = "#F2F2F2",
  text_dark  = "#1C1C1C",
  text_mid   = "#555555",
  text_light = "#888888",

  # Multi-series palette (8 distinguishable colours)
  series = c(
    "#1A3A5C", "#C0392B", "#2980B9", "#27AE60",
    "#8E44AD", "#E67E22", "#16A085", "#D4A017"
  )
)

# ---- Base Theme --------------------------------------------------------------

#' Institutional ggplot2 theme
#'
#' @param base_size   Base font size (default 11)
#' @param base_family Font family (default "sans")
#' @return ggplot2 theme object
#' @export
theme_quant <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
  ggplot2::theme(
    # -- Plot background -------------------------------------------------------
    plot.background  = element_rect(fill = PALETTE$background, colour = NA),
    panel.background = element_rect(fill = PALETTE$panel_bg,   colour = NA),
    panel.border     = element_rect(fill = NA, colour = "#CCCCCC", linewidth = 0.4),

    # -- Grid lines ------------------------------------------------------------
    panel.grid.major = element_line(colour = PALETTE$grid_major, linewidth = 0.3),
    panel.grid.minor = element_line(colour = PALETTE$grid_minor, linewidth = 0.15),

    # -- Typography ------------------------------------------------------------
    plot.title    = element_text(
      colour = PALETTE$primary, size = base_size * 1.4,
      face = "bold", margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      colour = PALETTE$text_mid, size = base_size * 1.0,
      margin = margin(b = 8)
    ),
    plot.caption  = element_text(
      colour = PALETTE$text_light, size = base_size * 0.75,
      hjust = 0, margin = margin(t = 8)
    ),
    axis.title    = element_text(colour = PALETTE$text_dark, size = base_size * 0.9),
    axis.text     = element_text(colour = PALETTE$text_mid,  size = base_size * 0.85),
    axis.ticks    = element_line(colour = "#CCCCCC", linewidth = 0.3),

    # -- Legend ----------------------------------------------------------------
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key        = element_rect(fill = NA, colour = NA),
    legend.title      = element_text(colour = PALETTE$text_dark,  size = base_size * 0.9, face = "bold"),
    legend.text       = element_text(colour = PALETTE$text_mid,   size = base_size * 0.85),
    legend.position   = "bottom",
    legend.direction  = "horizontal",

    # -- Facets ----------------------------------------------------------------
    strip.background  = element_rect(fill = PALETTE$primary, colour = NA),
    strip.text        = element_text(colour = "white", size = base_size * 0.9, face = "bold"),

    # -- Margins ---------------------------------------------------------------
    plot.margin       = margin(12, 16, 8, 12)
  )
}

#' Apply the quant theme globally for the session
#' @export
set_quant_theme <- function() {
  ggplot2::theme_set(theme_quant())
  ggplot2::update_geom_defaults("line",  list(colour = PALETTE$primary, linewidth = 0.8))
  ggplot2::update_geom_defaults("point", list(colour = PALETTE$primary, size = 1.8))
  ggplot2::update_geom_defaults("col",   list(fill   = PALETTE$primary))
  ggplot2::update_geom_defaults("bar",   list(fill   = PALETTE$primary))
  invisible(NULL)
}

# ---- Scale Helpers -----------------------------------------------------------

#' Percentage scale for ggplot2 axes
#' @export
scale_y_pct <- function(accuracy = 0.1, suffix = "%", ...) {
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(accuracy = accuracy / 100, suffix = suffix),
    ...
  )
}

#' Dollar/financial scale for ggplot2 axes
#' @export
scale_y_dollar <- function(prefix = "$", big.mark = ",", ...) {
  ggplot2::scale_y_continuous(
    labels = scales::label_dollar(prefix = prefix, big.mark = big.mark),
    ...
  )
}

#' Compact number scale (1K, 1M, 1B)
#' @export
scale_y_compact <- function(...) {
  ggplot2::scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    ...
  )
}

#' Quant colour scale (multi-series)
#' @export
scale_colour_quant <- function(...) {
  ggplot2::scale_colour_manual(values = PALETTE$series, ...)
}

#' @export
scale_fill_quant <- function(...) {
  ggplot2::scale_fill_manual(values = PALETTE$series, ...)
}

#' Diverging fill scale centred at zero (e.g. for return heatmaps)
#' @export
scale_fill_returns <- function(low = PALETTE$red, mid = "white",
                                high = PALETTE$green, midpoint = 0, ...) {
  ggplot2::scale_fill_gradient2(
    low = low, mid = mid, high = high, midpoint = midpoint, ...
  )
}

# ---- Plot Saving Utility -----------------------------------------------------

#' Save a ggplot with institutional dimensions and quality
#'
#' @param plot   ggplot object
#' @param path   Output path (with extension .png, .svg, .pdf)
#' @param width  Width in inches
#' @param height Height in inches
#' @param dpi    Resolution
#' @export
save_plot <- function(plot, path, width = 12, height = 7, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = PALETTE$background
  )
  invisible(path)
}
