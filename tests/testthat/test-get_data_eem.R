data_path <- system.file("unnamed_eem.rds", package = "encuestasmacro")

test_that("get_data_eem works", {
  expect_error(get_data_eem())
  expect_s3_class(get_data_eem(data_path = data_path), "data.frame")
  expect_s3_class(get_data_eem(format = "long", data_path = data_path), "data.frame")
})

test_that("get_data_eem works", {
  expect_error(get_data_eem())
  expect_s3_class(example_eem_data("wide"), "data.frame")
  expect_s3_class(example_eem_data(format = "long"), "data.frame")
})
