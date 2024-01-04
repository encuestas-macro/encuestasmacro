data_long <- example_eem_data("long")

test_that("eem_ribbon_plot works", {
  plot <- eem_ribbon_plot(data_long, "inflacion")
  expect_s3_class(plot, "ggplot")
})
