#' Create date labels out of dates
#'
#' Take a date object into a 'Month year' format
#'
#' @param date a date vector
#' @param year_in_new_line should the year be in it own line?
#'
#' @return a character string
#' @export
date_label <- function(date = Sys.Date(), year_in_new_line = FALSE) {
  checkmate::assert_date(date)
  checkmate::assert_logical(year_in_new_line)
  paste(
    stringr::str_to_title(lubridate::month(date, label = TRUE)),
    lubridate::year(date),
    sep = ifelse(year_in_new_line, "\n", " ")
  )
}


#' Escala de fechas mateniendo fija la fecha más reciente
#'
#' Esta función es útil para aquellos casos gráficos en los que se requiere
#' que la fecha más reciente esté visible en el eje.
#'
#' @param min_date fecha más lejana
#' @param max_date fecha más reciente
#' @param step distancia entre cada fecha, en meses
#'
#' @return un vector tipo fecha
#' @export
#'
#' @examples my_date_breaks(as.Date("2019-01-01"), Sys.Date())
my_date_breaks <- function(min_date, max_date, step = 4) {
  checkmate::assert_class(min_date, "Date")
  checkmate::assert_class(max_date, "Date")
  checkmate::assert_true(max_date > min_date)

  secuencia <- seq(
    from = min_date,
    to = max_date,
    by = "month"
  )

  selected <- length(secuencia)
  n <- length(secuencia)

  while (n > 1) {
    selected <- c(selected, n - step)
    n <- n - step
  }

  selected <- selected[selected > 0]

  breaks <- secuencia[selected]
  sort(breaks)
}

#' Change month encoding
#'
#' Take month from text to number or form number to text. This work with any
#' month name (Spanish or English) to create a number. From number to text only
#' creates Spanish months
#'
#' @param mes a number or character with the month
#' @param type a character indicating the type of conversion,
#' can be any of these:
#' \code{c("text_to_number", "number_to_text", "number_to_shorttext")}
#'
#' @export
#'
#' @examples
#' crear_mes("Enero", "text_to_number")
crear_mes <- function(mes, type = "text_to_number") {
  # Input validation
  checkmate::assertChoice(
    type, c("text_to_number", "number_to_text", "number_to_shorttext"))

  if (is.character(mes)) {
    checkmate::assert_choice(type, c("text_to_number"))
  } else if (is.numeric(mes)) {
    checkmate::assert(
      checkmate::check_choice(type, c("number_to_text", "number_to_shorttext")),
      all(mes %in% 1:12),
      combine = "and"
    )
  }

  if (type == "number_to_text") {
    new_mes <- dplyr::recode(
      mes,
      `1` = "Enero",
      `2` = "Febrero",
      `3` = "Marzo",
      `4` = "Abril",
      `5` = "Mayo",
      `6` = "Junio",
      `7` = "Julio",
      `8` = "Agosto",
      `9` = "Septiembre",
      `10` = "Octubre",
      `11` = "Noviembre",
      `12` = "Diciembre")
  }

  if (type == "number_to_shorttext") {
    new_mes <- dplyr::recode(
      mes,
      `1` = "Ene",
      `2` = "Feb",
      `3` = "Mar",
      `4` = "Abr",
      `5` = "May",
      `6` = "Jun",
      `7` = "Jul",
      `8` = "Ago",
      `9` = "Sep",
      `10` = "Oct",
      `11` = "Nov",
      `12` = "Dic")
  }

  if (type == "text_to_number") {
    mes  <-  stringr::str_to_title(mes)
    new_mes <- dplyr::recode(
      mes,
      "Jan" = 01,
      "Ene" = 01,
      "Feb" = 02,
      "Mar" = 03,
      "Abr" = 04,
      "Apr" = 04,
      "May" = 05,
      "Jun" = 06,
      "Jul" = 07,
      "Aug" = 08,
      "Ago" = 08,
      "Sep" = 09,
      "Sept" = 09,
      "Oct" = 10,
      "Nov" = 11,
      "Dec" = 12,
      "Dic" = 12,

      "Enero" = 01,
      "Febrero" = 02,
      "Marzo" = 03,
      "Abril" = 04,
      "Mayo" = 05,
      "Junio" = 06,
      "Julio" = 07,
      "Agosto" = 08,
      "Septiembre" = 09,
      "Octubre" = 10,
      "Noviembre" = 11,
      "Diciembre" = 12,

      "January" = 01,
      "February" = 02,
      "March" = 03,
      "April" = 04,
      "May" = 05,
      "June" = 06,
      "July" = 07,
      "August" = 08,
      "September" = 09,
      "October" = 10,
      "November" = 11,
      "December" = 12)
  }

  new_mes
}

#' Crear el directorio para alojar los resultados del procesamiento de la encuesta
#'
#' @param encuesta encuesta a procesar "eem" o "eoe"
#' @param year year value
#' @param mes string con el mes
#' @param path string con la ruta
#'
#' @export
create_month_dirs <- function(encuesta, year, mes, path = ".") {
  mes <- stringr::str_to_lower(mes)

  checkmate::assert_character(mes)
  checkmate::assert_choice(
    mes,
    c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
      "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  )

  mes_in_number <- stringr::str_pad(crear_mes(mes), width = 2, pad = "0")
  folder <- paste0(mes_in_number, ". ", mes)

  dir.create(
    file.path(path, encuesta, "outputs", year, folder),
    recursive = TRUE
  )
  dir.create(
    file.path(path, encuesta, "outputs", year, folder, "graficos"),
    recursive = TRUE
  )
  dir.create(
    file.path(path, encuesta, "outputs", year, folder, "pagina_web"),
    recursive = TRUE
  )
  dir.create(
    file.path(path, encuesta, "outputs", year, folder, "informe"),
    recursive = TRUE
  )
}
