#' Returns the historical series of the Macroeconomic Expectation Survey
#'
#' @param data_path an optional argument, needed when your are using the package
#' outside the project it was build to work on
#' @param format an string indicating the data frame format "wide" or "long"
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
