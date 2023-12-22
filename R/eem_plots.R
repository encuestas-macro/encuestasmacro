eem_boxplot <- function(
    data_eem,
    variable,
    ...,
    min_date = "2021-01-01",
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
