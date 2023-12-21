path <- file.path(tempdir(), "test")
dir.create(path)

mes <- "enero"
year <- 2023
encuesta <- "eem"

create_month_dirs(encuesta, year, mes, path)

test_that("create_month_dirs works", {
  expect_true(file.exists(file.path(path, "eem/outputs/2023/01. enero")))
  expect_equal(
    list.files(file.path(path, "eem/outputs/2023/01. enero")),
    c("graficos", "informe", "pagina_web")
  )
})
