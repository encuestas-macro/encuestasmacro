ipc_data <- get_ipc_general()

test_that("get_ipc_data No empty columns", {
  any_empty_col <- ipc_data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("get_ipc_data no empty rows", {
  any_empty_row <- ipc_data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  expect_false(any_empty_row)
})

test_that("No dates in the future", {
  expect_true(max(ipc_data$fecha) <= lubridate::today())
})
