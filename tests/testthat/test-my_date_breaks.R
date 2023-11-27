min_date <- as.Date("2020-01-01")
max_date <- as.Date("2023-11-01")

test_that("my_date_breaks works as expected", {
  dates <- my_date_breaks(min_date, max_date)

  expect_length(dates, 12)
  expect_error(my_date_breaks(as.character(min_date), max_date))
  expect_error(my_date_breaks(max_date, min_date))
})
