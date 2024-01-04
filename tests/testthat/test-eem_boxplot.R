data_eem_wide <- get_data_eem(
  data_path = system.file("unnamed_eem.rds", package = "encuestasmacro"),
  format = "wide"
)



test_that("eem_boxplot works", {
  plot1 <- eem_boxplot(data_eem_wide, "inflacion_mes")
  plot2 <- eem_data_diciembre(data_eem_wide) |>
    eem_boxplot("expectativa")

  expect_s3_class(plot1, "ggplot")
  expect_s3_class(plot2, "ggplot")
})
