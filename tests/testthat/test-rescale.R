test_that("rescale works", {
  expect_equal(
    rescale(0:10, 0, 1),
    0:10 / 10
  )

  expect_equal(
    rescale(0:10, 0, 100),
    0:10 * 10
  )
})
