
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
