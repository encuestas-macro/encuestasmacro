
#' Read and prepare the monthly data
#'
#' @param data_year surevey year
#' @param data_mes survey month as number
#'
#' @return a data frame
#' @export
eem_month_data <- function(data_year, data_mes) {
  path <- file.path("eem", "data", "original_files", data_year)
  file <- list.files(path, pattern = data_mes)

  detalles_informantes <- readxl::read_excel(
    here::here("eem", "data", "general_files", "detalles_informantes.xlsx")
  ) |>
    dplyr::mutate(
      informante = stringr::str_to_lower(informante)
    )

  headers <- c(
    "informante",
    "inflacion_mes",
    "inflacion_diciembre",
    "inflacion_interanual",
    "inflacion_diciembre2",
    "inflacion_interanual2",
    "tc_mes",
    "tc_diciembre",
    "tc_interanual",
    "tc_diciembre2",
    "tc_interanual2",
    "pib_trimestre",
    "pib_diciembre",
    "pib_diciembre2",
    "tpm_mes",
    "tpm_trimestre",
    "tpm_diciembre",
    "tpm_interanual",
    "tpm_interanual2"
  )

  observadas <- get_series_observadas()
  tc_diciembre_anterior <- get_series_observadas() |>
    dplyr::filter(year == data_year - 1, mes == 12) |>
    dplyr::pull(tc_mes_obs)

  data <- readxl::read_excel(
    here::here(path, file),
    sheet = "Resumen",
    skip = 2,
    col_names = FALSE
  ) |>
    stats::setNames(headers) |>
    dplyr::filter(
      informante != "Respuestas",
      !dplyr::if_all(
        c(inflacion_mes:tpm_interanual2),
        ~is.na(.) | . == 0
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("inflacion|pib|tpm|tc"),
        as.numeric
      ),
      dplyr::across(
        dplyr::matches("inflacion|pib|tpm"),
        ~. * 100
      ),
      informante = stringr::str_to_lower(informante),
      mes = crear_mes(data_mes)
    ) |>
    dplyr::left_join(
      detalles_informantes,
      by = "informante"
    )

  if (data_year == 2020 & data_mes == "mayo") {
    observada_mayo <- observadas |>
      dplyr::filter(year == 2020, mes == 5)

    data <- data |>
      dplyr::mutate(
        tc_mes = observada_mayo$tc_mes_obs,
        tpm_mes = observada_mayo$tpm_mes_obs
      )
  }

  data <- data|>
    dplyr::mutate(
      tcd_diciembre = ((tc_diciembre / tc_diciembre_anterior) - 1) * 100 ,
      tcd_interanual = ((tc_interanual / tc_mes) - 1) * 100,
      tcd_diciembre2 = ((tc_diciembre2 / tc_diciembre) - 1) * 100,
      tcd_interanual2 = ((tc_interanual2 / tc_interanual) - 1) * 100
    )

  data <- data |>
    dplyr::mutate(
      year = data_year,
      mes = crear_mes(data_mes),
      periodo = lubridate::ymd(paste(year, mes, "01", sep = '/'))
    ) |>
    dplyr::select(
      periodo, year, mes, grupo, informante, dplyr::everything()
    )

  data
}

#' Update the historical data
#'
#' @param month_data monthly data
#' @param new_data is this a new entry?
#' @export
update_eem_historic <- function(month_data, new_data = TRUE) {
  historico <- get_eem_historic()
  date_month_data <- max(month_data$periodo)
  date_historic <- max(historico$periodo)

  if(new_data) {
    if(date_month_data < date_historic) {
      stop(
        'La data del mes que pretende introducir ya esta incluida,
        para sobreescribir usar `new_data = FALSE`'
      )
    }
      historico_updated <- dplyr::bind_rows(historico, month_data)
      saveRDS(
        historico_updated,
        here::here('eem/data/general_files/rds/eem_historico.rds')
      )
      print('Archivo historico actualizado')
  } else if(!new_data) {
    historico_updated <- historico |>
      dplyr::filter(periodo != date_month_data) |>
      dplyr::bind_rows(month_data) |>
      dplyr::arrange(periodo)
    saveRDS(
      historico_updated,
      here::here('eem/data/general_files/rds/eem_historico.rds')
    )
    usethis::ui_done("Archivo historico actualizado")
  }
}
