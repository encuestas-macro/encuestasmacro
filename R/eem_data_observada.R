#' To get the general CPI data
#'
#' You can get the CPI index and the monthly, year over year and
#' throughout the year variations, as well as the 12 month average
get_ipc_general <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/precios/documents/",
    "ipc_base_2019-2020.xls"
  )

  file_path <- tempfile(pattern = "", fileext = ".xls")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  suppressMessages(
    ipc_general <- readxl::read_excel(
      file_path,
      sheet = 1,
      col_names = FALSE,
      skip = 7)
  )

  var_names <- c(
    "year", "mes", "ipc", "ipc_vm", "ipc_vd", "ipc_vi", "ipc_p12")

  ipc_general |>
    janitor::clean_names() |>
    dplyr::select(1:7) |>
    stats::setNames(var_names) |>
    dplyr::filter(!is.na(mes)) |>
    dplyr::mutate(
      fecha = seq(
        lubridate::ymd("1984/01/01"),
        by = "month",
        length.out = dplyr::n()),
      year = lubridate::year(fecha),
      mes = crear_mes(mes)
    ) |>
    dplyr::select(fecha, year, mes, dplyr::everything())
}

#' Exchange rates in the spot market of the Dominican Republic
#'
#' This function returns the average exchange rates of the operations conducted
#' by the banks and exchange operators in the  Dominican Republic
#' based on the specified frequency.
#'
#' @param frecuencia A character string that specifies the frequency of the
#' exchange rates to be downloaded. Valid options are "diaria", "mensual",
#' "trimestral",  or "anual".
#'
#' @param average_or_fp A character string that specifies if the average or the
#' value for the last day of the period is desired. valid options are "average"
#' and "fp"
#'
#' @return A data frame with columns:
#'  compra: for the buying rates
#'  venta: selling rates
#' @export
#'
#' @examples
#' get_tc_spot("mensual", "average")
#' get_tc_spot("mensual", "fp")
#' get_tc_spot("trimestral", "average")
#' get_tc_spot("trimestral", "fp")
get_tc_spot <- function(frecuencia = "mensual", average_or_fp = "average") {
  checkmate::assert_choice(
    frecuencia,
    choices = c("diaria", "mensual", "trimestral", "anual")
  )
  checkmate::assert_choice(
    average_or_fp,
    choices = c("average", "fp")
  )

  sheet <- dplyr::case_when(
    frecuencia == "diaria" ~ "Diaria",

    frecuencia == "mensual"    & average_or_fp == "average" ~ "PromMensual",
    frecuencia == "trimestral" & average_or_fp == "average" ~ "PromTrimestral",
    frecuencia == "anual"      & average_or_fp == "average" ~ "PromAnual",

    frecuencia == "mensual"    & average_or_fp == "fp"      ~ "FPMensual",
    frecuencia == "trimestral" & average_or_fp == "fp"      ~ "FPTrimestral",
    frecuencia == "anual"      & average_or_fp == "fp"      ~ "FPAnual"
  )

  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xlsx"
  )

  path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url, path, mode = "wb", quiet = TRUE)

  tipo_cambio <- readxl::read_excel(path, sheet = sheet, skip = 2) |>
    janitor::clean_names() |>
    dplyr::rename(year = ano)

  if (frecuencia == "diaria") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        mes = crear_mes(mes, "text_to_number"),
        fecha = lubridate::make_date(year, mes, dia)
      ) |>
      dplyr::select(fecha, year, mes, dia, dplyr::everything())
  }

  if (frecuencia == "mensual") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        mes = crear_mes(mes, "text_to_number"),
        fecha = lubridate::make_date(year, mes, "01")
      ) |>
      dplyr::select(fecha, year, mes, dplyr::everything())
  }

  if (frecuencia == "trimestral") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        fecha = seq(
          as.Date("1992/01/01"),
          by = "quarter",
          length.out = dplyr::n()),
        trimestre = lubridate::quarter(fecha)
      ) |>
      dplyr::select(fecha, year, trimestre, dplyr::everything())
  }

  tipo_cambio
}

#' Decarga la tada de política monetaria de república dominicana
#'
#' @export
get_tpm <- function() {
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-monetario-y-financiero/documents/Serie_TPM.xlsx"
  )

  file_path <- tempfile(fileext = ".xlsx")
  utils::download.file(url, file_path, mode = "wb", quiet = TRUE)

  readxl::read_excel(file_path, col_names = FALSE, skip = 6) |>
    stats::setNames(
      c(
        "year",
        "mes",
        "tpm",
        "facilidad_deposito",
        "facilidad_prestamo",
        "facilidad_lombarda"
      )
    ) |>
    tidyr::fill(year) |>
    dplyr::filter(!is.na(mes)) |>
    dplyr::mutate(
      mes = stringr::str_extract(mes, "^..."),
      periodo = lubridate::make_date(year, crear_mes(mes), "01")
    ) |>
    dplyr::select(-c(year, mes)) |>
    dplyr::mutate(dplyr::across(-periodo, \(x) x * 100)) |>
    dplyr::select(periodo, tidyr::everything()) |>
    suppressMessages()
}

#' GDP by the expenditure approach, both real and nominal
#'
#' @param modalidad string indicating if "real" or "nominal"
#' @param acumulado logical, by default FALSE
#' @param homogenea_91 logical, by default FALSE
#'
#' @return a data frame
#' @export
get_pib_gasto <- function(
    modalidad = "nominal",
    acumulado = FALSE,
    homogenea_91 = FALSE
) {
  checkmate::assert_choice(modalidad, c("nominal", "real"))
  checkmate::assert_logical(acumulado, any.missing = FALSE, max.len = 1)
  checkmate::assert_logical(homogenea_91, any.missing = FALSE, max.len = 1)

  temp_file <- tempfile(fileext = ifelse(homogenea_91, ".xlsx", ".xls"))
  start_date <- ifelse(homogenea_91, "1991-01-01", "2007-01-01")

  url <- ifelse(
    homogenea_91,
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_gasto_retro.xlsx", # nolint
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_gasto_2007.xls"    # nolint
  )

  utils::download.file(url, temp_file, mode = "wb", quiet = TRUE)

  sheet <- dplyr::case_when(
    modalidad == "nominal" & acumulado  ~ "PIB$_Trim_Acum",
    modalidad == "nominal" & !acumulado ~ "PIB$_Trim",
    modalidad == "real"    & acumulado  ~ "PIBK_Trim_Acum",
    modalidad == "real"    & !acumulado ~ "PIBK_Trim"
  )

  read_pib_indicator <- function(skip, nmax, indicator_name) {
    suppressMessages(
      pib_gasto <- readxl::read_excel(
        path = temp_file,
        sheet = sheet,
        skip = skip,
        n_max = nmax,
        col_names = FALSE
      )
    )

    quarters <- seq(
      as.Date(start_date),
      by = "quarter",
      length.out = ncol(pib_gasto) - 1
    )

    pib_gasto |>
      stats::setNames(c("partida", quarters)) |>
      dplyr::filter(!is.na(partida)) |>
      tidyr::pivot_longer(
        cols = -partida,
        names_to = "trimestre",
        values_to = indicator_name) |>
      dplyr::mutate(
        trimestre = as.numeric(trimestre),
        fecha = lubridate::as_date(trimestre),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) |>
      dplyr::select(
        partida, fecha, year, trimestre, dplyr::all_of(indicator_name)
      )
  }

  to_read <- list(
    nominal = list(
      list(skip = 9, nmax = 14,  indicator_name = "pib_nominal"),
      list(skip = 28, nmax = 14, indicator_name = "ponderacion")
    ),
    real = list(
      list(skip = 9, nmax = 14,  indicator_name = "indice"),
      list(skip = 28, nmax = 14, indicator_name = "crecimiento_interanual"),
      list(skip = 47, nmax = 14, indicator_name = "incidencia")
    )
  )

  to_read[[modalidad]] |>
    purrr::map(\(args) do.call(read_pib_indicator, args)) |>
    purrr::reduce(
      dplyr::left_join,
      by = c("partida", "fecha", "year", "trimestre")
    )

}



#' Data observada EEM
#'
#' Descagar y preparar las series observadas de las variables de la EEM
#' en los distintos horizonte
#'
#' @export
get_data_obervada_eem <- function() {
  # Data IPC
  data_ipc <- get_ipc_general() |>
    dplyr::select(
      periodo = fecha,
      year,
      mes,
      inflacion_mes_obs = ipc_vm,
      inflacion_interanual_obs = ipc_vi
    ) |>
    dplyr::mutate(
      inflacion_diciembre_obs = dplyr::case_when(
        mes == 12 ~ inflacion_interanual_obs
      )
    ) |>
    tidyr::fill(inflacion_diciembre_obs, .direction = "up")

  # Data tipo de cambio
  data_tc <- get_tc_spot(average_or_fp = "fp") |>
    dplyr::select(periodo = fecha, year, mes, tc_mes_obs = venta) |>
    dplyr::mutate(
      tc_diciembre_obs = dplyr::case_when(
        mes == 12 ~ tc_mes_obs
      )
    ) |>
    tidyr::fill(tc_diciembre_obs, .direction = "up")

  # Data PIB
  pib_diciembre <- get_pib_gasto(modalidad = "real", acumulado = TRUE) |>
    dplyr::filter(
      partida == "Producto Interno Bruto",
      trimestre == 4
    ) |>
    dplyr::mutate(mes = 12) |>
    dplyr::select(year, pib_diciembre_obs = crecimiento_interanual)

  pib_trimestre <- get_pib_gasto(
    modalidad = "real",
    acumulado = FALSE
  ) |>
    dplyr::filter(partida == "Producto Interno Bruto") |>
    dplyr::select(year, trimestre, pib_interanual_obs = crecimiento_interanual)

  data_pib <- pib_trimestre |>
    dplyr::left_join(pib_diciembre, by = "year")

  data_tpm <- get_tpm() |>
    dplyr::select(periodo, tpm_mes_obs = tpm) |>
    dplyr::mutate(
      tpm_diciembre_obs = dplyr::if_else(lubridate::month(periodo) == 12,  tpm_mes_obs, NA),
      tpm_trimestre_obs = dplyr::case_when(
        lubridate::month(periodo) %in% c(3, 6, 9, 12) ~ tpm_mes_obs
      )
    ) |>
    tidyr::fill(tpm_diciembre_obs, tpm_trimestre_obs, .direction = "up")

  data_observada <- data_ipc |>
    dplyr::left_join(data_tc, by = c("periodo", "year", "mes")) |>
    dplyr::left_join(data_tpm, by = "periodo") |>
    dplyr::mutate(trimestre = lubridate::quarter(periodo)) |>
    dplyr::left_join(data_pib, by = c("year", "trimestre")) |>
    dplyr::filter(year > 2008) |>
    dplyr::select(
      periodo, year, mes,
      dplyr::contains("inflacion"),
      dplyr::contains("tc"),
      dplyr::contains("pib"),
      dplyr::contains("tpm")
    )

  data_observada
}
