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
    by = 'month'
  )

  selected <- length(secuencia)
  n <- length(secuencia)

  while(n > 1) {
    selected <- c(selected, n - step)
    n <- n - step
  }

  selected <- selected[selected > 0]

  breaks <- secuencia[selected]
  sort(breaks)
}
