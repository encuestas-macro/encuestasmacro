plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
  ggplot2::geom_point()

temp_path <- tempdir()

saving_plot(plot, w = 3, h = 3, path = temp_path)

test_that("saving_plot works as expected", {
  expect_error(saving_plot(plot(1:10), w = 3, h = 3))
  expect_true(file.exists(file.path(temp_path, "plot.png")))
})
