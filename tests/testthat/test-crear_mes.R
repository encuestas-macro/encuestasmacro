months_text <-     c(
  "Enero",
  "Febrero",
  "Marzo",
  "Abril",
  "Mayo",
  "Junio",
  "Julio",
  "Agosto",
  "Septiembre",
  "Octubre",
  "Noviembre",
  "Diciembre"
)

months_shorttext <- stringr::str_sub(months_text, end = 3)

test_that("crear_mes works well", {
  expect_equal(
    crear_mes(1:12, "number_to_text"),
    months_text
  )
  expect_equal(
    crear_mes(months_text, "text_to_number"),
    1:12
  )
  expect_equal(
    crear_mes(1:12, "number_to_shorttext"),
    months_shorttext
  )
  expect_type(
    crear_mes(months_text, "text_to_number"),
    "double"
  )
  expect_type(
    crear_mes(1:12, "number_to_text"),
    "character"
  )
})
