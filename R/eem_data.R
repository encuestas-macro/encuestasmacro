#' Returns the historical series of the Macroeconomic Expectation Survey
#'
#' @param data_path an optional argument, needed when your are using the package
#' outside the project it was build to work on
#' @param format a string indicating the data frame format "wide" or "long"
#'
#' @return a data frame with the historical data
#' @export
get_data_eem <- function(format = c("wide", "long"), data_path = NULL) {
  format <- rlang::arg_match(format, values = c("wide", "long"))

  if (is.null(data_path)) {
    data_path <- file.path("eem", "data", "general_files", "rds", "eem_historico.rds")

    if (!base::file.exists(data_path)) {
      rlang::abort("You're not working in the project this package was designed for, please add the path to the data") #nolint
    }
  }

  if (!base::file.exists(data_path)) rlang::abort("The file doesn't exists")

  wide <- readRDS(data_path)

  if (format == "long") {

    long <- wide |>
      tidyr::pivot_longer(
        dplyr::matches("inflacion|tcd|tc|pib|tpm")
      ) |>
      tidyr::separate(name, into = c("variable", "horizonte")) |>
      dplyr::mutate(
        horizonte = factor(
          horizonte,
          eem_details$horizontes$levels,
          eem_details$horizontes$labels
        ),
        variable_label = factor(
          variable,
          eem_details$variables$levels,
          eem_details$variables$labels
        )
      ) |>
      dplyr::select(
        periodo,
        year,
        mes,
        dplyr::matches("informante|id_colaborador"),
        grupo,
        variable,
        variable_label,
        horizonte,
        value
      )

    return(long)
  }

  tibble::as_tibble(wide)
}

#' Forecast for the end of the year
#'
#' In the survey forecast for the end of a given year are collected up to 24 times.
#' 12 times the year before, as end of next year forecasts, and 12 times during the
#' current year ad en of year expectation. This functions gather those forecasts
#' into one series
#'
#' @param data_eem eem data in wide format
#' @param variable variable key: "inflacion", "tc", "tcd", "pib" or "tpm"
#' @param year_to_plot year to plot
#'
#' @return a data frame
#' @export
#'
#' @examples
#' eem_data_diciembre(get_data_eem(), "inflacion", 2023)
eem_data_diciembre <- function(
    data_eem,
    variable = "inflacion",
    year_to_plot = 2023
) {
  checkmate::check_choice(variable, c(eem_details$variables$levels))
  from_last_year <- data_eem |>
    dplyr::filter(year == year_to_plot - 1) |>
    dplyr::select(
      periodo,
      dplyr::matches("informante|colaborador"),
      expectativa = .data[[paste0(variable, "_diciembre2")]]
    )

  data_eem |>
    dplyr::filter(year == year_to_plot) |>
    dplyr::select(
      periodo,
      dplyr::matches("informante|colaborador"),
      expectativa = .data[[paste0(variable, "_diciembre")]]
    ) |>
    dplyr::bind_rows(from_last_year)
}

#' Get EEM example data
#'
#' @param format wide or long format
#'
#' @return a data frame
#' @export
#'
#' @examples
#' example_eem_data("wide")
#' example_eem_data("long")
example_eem_data <- function(format = "wide") {
  get_data_eem(
    data_path = system.file("unnamed_eem.rds", package = "encuestasmacro"),
    format = "wide"
  )
}
