
#' Theme for Macroeconomic Surveys plots
#'
#' @param ... ggplot2:theme arguments
#' @param text_size plot text size
#' @param title_size title text size
#' @param strip_size strip text size
#' @param legend_size legend text size
#' @param legend_position legend position
#' @param font_family font family name. Use extrafont::load_font() to load
#' fonts
#'
#' @return a theme object
#' @export
theme_em <- function(
    ...,
    text_size = 11,
    title_size = 16,
    strip_size = 10,
    legend_size = 10,
    legend_position = "bottom",
    font_family = "Gotham Book"
  ) {
  ggplot2::theme_light(
    base_size = text_size,
    base_family = font_family
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = title_size),
      axis.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", size = strip_size),
      legend.text = ggplot2::element_text(size = legend_size),
      legend.position = legend_position,
      ...
    )
}

#' Macroeconomic Surveys colors
#'
#' @param which color or palette to pick from the list
#'
#' @return a vector with one or more colors
#' @export
colores_em <- function(which = NULL) {
  colores <- tibble::lst(
    blue  = "#0070C0",
    gray  = "#7E8083",
    green = "#007033",
    red   = "#C00000",

    paleta = c(
      blue  = "#0070C0",
      gray  = "#7E8083",
      green = "#007033",
      red   = "#C00000"
    ),

    paleta_ppt = c(
      "#d00000",
      "#e85d04",
      "#e5383b",
      "#f48c06"
    ),

    paleta_tableau = c(
      "#4E79A7",
      "#F28E2B",
      "#E15759",
      "#76B7B2",
      "#59A14F",
      "#EDC948",
      "#B07AA1",
      "#FF9DA7",
      "#9C755F",
      "#BAB0AC"
    )
  )

  if (!is.null(which)) {
    checkmate::assert_choice(which, names(colores))
    return(colores[[which]])
  }

  colores
}
