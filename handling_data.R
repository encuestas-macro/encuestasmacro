# source needed objects
#source(here::here('general_scripts', 'utilitarios.R'))

# Función para importar la data observada

### Recordar actualizar esta función
get_series_observadas <- function(x) {
  readxl::read_excel(here::here('eem', 'data', 'general_files', 'series_observadas.xlsx')) |>
    dplyr::mutate(
      periodo = lubridate::ymd(paste(year, mes, '01', sep = '-'))
    ) |>
    dplyr::select(periodo, year, mes, everything())
}

# Función para importar la data de un mes en particular
eem_month_data <- function(data_year, data_mes) {
  path <- here::here("eem", "data", "original_files", data_year)
  file <- list.files(path, pattern = data_mes)
  
  # Detalles sobre los colaboradores
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
    setNames(headers) |>
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
  
  # La inflación de mayo 2020 se retrasó, salió en junio. Por esta razón
  # el levantamiento de la EEM se realizó luego de que el TC y la TPM del
  # mese eran públicas, quitándole sentido a levantar expectativas para esos
  # horizontes. Por eso esta imputación
  
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

# Función para importar el histórico de la EEM
get_eem_historic <- function(historic_path = NULL){
  
  if(is.null(historic_path)) {
    historic_path <- here::here('eem', 'data', 'general_files', 'rds', 'eem_historico.rds')
    path_exists <- base::file.exists(historic_path)
    
    # En caso de que no se esté usando la función en el ambiente para el que se
    # diseñó
    if(!path_exists) stop("Se debe agregar la ruta del archivo histórico")
    
  } else {
    path_exists <- base::file.exists(historic_path)
    
    # En caso de que no se esté usando la función en el ambiente para el que se
    # diseñó
    if(!path_exists) stop("El archivo señalado no existe")
  }
  
  historico <- readRDS(historic_path)
  historico
}

get_eem_historic_long <- function() {
  get_eem_historic() |>
    tidyr::pivot_longer(
      dplyr::matches("inflacion|tcd|tc|pib|tpm")
    ) |>
    tidyr::separate(name, into = c("variable", "horizonte")) |> 
    dplyr::mutate(
      horizonte = factor(
        horizonte,
        c('mes', 'trimestre', 'diciembre', 'interanual', 'diciembre2', 'interanual2'),
        c("Fin de mes", 'Fin de trimestre', 'Fin de año', '12 meses', 
          'Fin año siguiente', '24 meses')
      ),
      variable_label = dplyr::recode(
        variable, 'inflacion' = 'Inflación',
        'tc' = 'Tipo de cambio',
        'tcd' = 'Depreciación TC',
        'pib' = 'PIB', 'tpm' = 'TPM')
    ) |> 
    dplyr::select(periodo, year, mes, informante, grupo, variable, variable_label, horizonte, value)
}


# Actualiza el histórico en base a la data de un mes particular
update_eem_historic <- function(month_data, new_data = TRUE) {
  historico <- get_eem_historic()
  date_month_data <- max(month_data$periodo)
  date_historic <- max(historico$periodo)
  
  if(new_data) {
    if(date_month_data < date_historic) {
      stop(
        'La data del mes que pretende introducir ya está incluída,
        para sobreescribir usar `new_data = FALSE`'
      )
    }
      historico_updated <- dplyr::bind_rows(historico, month_data)
      saveRDS(
        historico_updated,
        here::here('eem/data/general_files/rds/eem_historico.rds')
      )
      print('Archivo histórico actualizado')
  } else if(!new_data) {
    historico_updated <- historico |>
      dplyr::filter(periodo != date_month_data) |>
      dplyr::bind_rows(month_data) |>
      dplyr::arrange(periodo)
    saveRDS(
      historico_updated,
      here::here('eem/data/general_files/rds/eem_historico.rds')
    )
    usethis::ui_done("Archivo histórico actualizado")
  }
}

# Actualiza el histórico en excel
update_eem_excel_historic <- function() {
  historic <- get_eem_historic()
  xlsx::write.xlsx(
    as.data.frame(historic),
    here::here('eem', 'data', 'general_files', 'eem_historico.xlsx'),
    row.names = FALSE, showNA = FALSE
    )
}

# Genera el archivo de respuestas desagregadas del tipo de cambio
respuestas_desagregadas_tc <- function() {
  
  names <- c("year", "mes", "informante_id", "tc_mes", 
    "tc_diciembre", "tc_12_meses","tc_diciembre_next_year", 'tc_24_meses')
  
  codes <- 
    get_eem_historic() |>
    dplyr::count(informante) |>
    tibble::rowid_to_column(var = 'informante_id') |> 
    dplyr::select(-n) %>%
    as_tibble() %>%
    ungroup()
  
  get_eem_historic() |>
    #ungroup() %>% 
    dplyr::select(year, mes, informante, contains('tc_')) |>
    dplyr::left_join(codes, by = 'informante') |> 
    dplyr::arrange(year, mes, informante_id) |>
    dplyr::select(year, mes, informante_id, contains('tc_')) |>
    setNames(names) |>
    tibble::as_tibble()
}
