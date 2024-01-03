

#' Create boxplot
#'
#' Designed to plot boxplot out of the wide EEM data. Just provide the data frame
#' and the name of the variable to plot.
#'
#' @param data_eem eem data in wide format, or simmilar strutured data frame
#' @param variable variable key: "inflacion", "tc", "tcd", "pib" or "tpm"
#' @param ...
#' @param min_date start date of the plot in yyyy-mm-dd format
#' @param color color to use from the colores_em()
#' @param font_size number with the funt size
#' @param labsx labs for x axis
#' @param labsy labs for y axis
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' eem_boxplot(example_eem_data(), "inflacion_interanual")
#' eem_boxplot(example_eem_data(), "tcd_diciembre")
eem_boxplot <- function(
    data_eem,
    variable,
    ...,
    min_date = "2022-01-01",
    color = colores_em("blue"),
    font_size = 10,
    labsx = NULL,
    labsy = NULL
  ) {
  data_exp <- data_eem |>
    dplyr::filter(periodo >= min_date)

  plot <- data_exp |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = periodo,
        y = dplyr::all_of(variable),
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
    theme_em(...) +
    ggplot2::labs(x = labsx, y = labsy) +
    ggplot2::scale_x_date(
      labels = date_label,
      breaks = my_date_breaks(
        serie_date_limits[1],
        serie_date_limits[2]
      )
    )
}
