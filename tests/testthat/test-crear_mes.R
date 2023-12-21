
test_that("crear_mes works well", {
  expect_equal(crear_mes("enero"), 1)
  expect_equal(
    crear_mes(1:12, "number_to_text"),
    c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
      "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  )
})
