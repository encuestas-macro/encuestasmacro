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
