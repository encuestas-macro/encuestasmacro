data_eem <- get_data_eem(
  data_path = system.file("unnamed_eem.rds", package = "encuestasmacro")
)

test_that("eem_boxplot works", {
  plot1 <- eem_boxplot(data_eem, "inflacion_mes")
  expect_s3_class(plot1, "ggplot")
})
