
#' Create boxplot
#'
#' Designed to plot boxplot out of the wide EEM data. Just provide the data frame
#' and the name of the variable to plot.
#'
#' @param data_eem eem data in wide format, or simmilar strutured data frame
#' @param variable variable key: "inflacion", "tc", "tcd", "pib" or "tpm"
#' @param ... ggplot2 theme configuration
#' @param min_date start date of the plot in yyyy-mm-dd format
#' @param color color to use from the colores_em()
#' @param font_size number with the funt size
#' @param labsx labs for x axis
#' @param labsy labs for y axis
#' @param font_family font family name. Use extrafont::load_font() to load
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' eem_boxplot(example_eem_data(), "inflacion_interanual", font_family = "sans")
#' eem_boxplot(example_eem_data(), "tcd_diciembre", font_family = "sans")
eem_boxplot <- function(
    data_eem,
    variable,
    ...,
    min_date = "2022-01-01",
    color = colores_em("blue"),
    font_size = 10,
    labsx = NULL,
    labsy = NULL,
    font_family = "Gotham Book"
  ) {
  data_exp <- data_eem |>
    dplyr::filter(periodo >= min_date)

  plot <- data_exp |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = periodo,
        y = .data[[variable]],
        group = periodo
      )
    ) +
    ggplot2::geom_boxplot(
      outlier.shape = NA,
      color = color
    )

  bp_limits <- data_exp |>
    dplyr::group_by(periodo) |>
    dplyr::summarise(
      min = boxplot.stats(.data[[variable]])[["stats"]][1],
      max = boxplot.stats(.data[[variable]])[["stats"]][5]
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      min = min(min),
      max = max(max)
    )

  serie_date_limits <- c(
    min(data_exp$periodo),
    max(data_exp$periodo)
  )

  plot +
    ggplot2::coord_cartesian(
      ylim = c(bp_limits$min, bp_limits$max)
    ) +
    theme_em(..., font_family = font_family) +
    ggplot2::labs(x = labsx, y = labsy) +
    ggplot2::scale_x_date(
      labels = date_label,
      breaks = my_date_breaks(
        serie_date_limits[1],
        serie_date_limits[2]
      )
    )
}

#' EEM ribbon plot
#'
#' Plot median or mean with distribution area. Useful to explore the evolution
#' of the expectations and it's distribution for a given variable.
#'
#' @param eem_long eem data in long format. Get it using `get_eem_data("long")`
#' @param plot_var variable to plot: "inflacion", "tc", "tcd", "pib" or "tpm"
#' @param ... ggplot2 theme configuration
#' @param horizon horizon to plot
#' @param stat string indicating "promedio" or "mediana". Promedio by default
#' @param start_year When should the plot start
#' @param ribbon_min a number between 0 and 1
#' @param ribbon_max a number between 0 and 1
#' @param ribbon_alpha a number between 0 and 1
#' @param ribbon_fill color to fill the ribbon. Literal color or hex code
#' @param color color for the line. Literal color or hex code
#' @param dot_size number indicating the size of the dots
#' @param breaks integer with the number of breaks
#' @param font_size number with the font size
#' @param font_family font family name. Use extrafont::load_font() to load
#'
#' @return a ggplot2 object
#' @export
eem_ribbon_plot <- function(
    eem_long,
    plot_var,
    ...,
    horizon = "12 meses",
    stat = "promedio",
    start_year = 2020,
    ribbon_min = 0.25,
    ribbon_max = 0.75,
    ribbon_alpha = 0.3,
    ribbon_fill = colores_em("gray"),
    color = NULL,
    dot_size = 2,
    breaks = 6,
    font_size = 13,
    font_family = "Gotham Book"
  ) {
  checkmate::assert_choice(plot_var, eem_details$variables$levels)
  checkmate::assert_choice(horizon, eem_details$horizontes$labels)
  checkmate::assert_number(start_year)
  checkmate::assert_number(ribbon_max)
  checkmate::assert_number(ribbon_min)
  checkmate::assert_number(ribbon_alpha)
  checkmate::assert_true(ribbon_min >= 0 && ribbon_min <= 1)
  checkmate::assert_true(ribbon_max >= 0 && ribbon_max <= 1)
  checkmate::assert_true(ribbon_alpha >= 0 && ribbon_alpha <= 1)
  checkmate::assert_true(ribbon_max >= ribbon_min)

  if (is.null(color)) {
    color <- colores_em()$paleta[sample(1:4, 1)]
  }

  eem_long |>
    dplyr::filter(
      variable == plot_var,
      horizonte == horizon,
      year >= start_year
    ) |>
    dplyr::group_by(horizonte, periodo) |>
    dplyr::summarise(
      promedio = mean(value, na.rm = TRUE),
      mediana = median(value, na.rm = TRUE),
      q1 = quantile(value, ribbon_min, na.rm = TRUE),
      q3 = quantile(value, ribbon_max, na.rm = TRUE),
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ round(., digits = 2))
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = periodo, text = round(.data[[stat]], 2))
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q1, ymax = q3),
      fill = ribbon_fill,
      color = NA,
      linewidth = 0,
      alpha = ribbon_alpha,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[stat]]), color = "white", linewidth = 1.5) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data[[stat]]),
      size = dot_size + 1,
      color = "white"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[stat]]),
      color = color
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data[[stat]]),
      size = dot_size,
      color = color
    ) +
    ggplot2::scale_x_date(
      breaks = scales::pretty_breaks(breaks),
      labels = date_label
    ) +
    ggplot2::scale_y_continuous(labels = function(x) scales::comma(x, 0.1)) +
    theme_em(
      text_size = font_size,
      font_family = font_family,
      ...
      ) +
    ggplot2::labs(x = NULL, y = NULL)
}
