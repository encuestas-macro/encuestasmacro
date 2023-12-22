data_path <- system.file("unnamed_eem.rds", package = "encuestasmacro")

test_that("multiplication works", {
  expect_error(get_data_eem())
  expect_s3_class(get_data_eem(data_path = data_path), "data.frame")
  expect_s3_class(get_data_eem(format = "long", data_path = data_path), "data.frame")
})
