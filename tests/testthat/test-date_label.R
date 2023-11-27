one_date <- as.Date("2023-11-01")
two_dates <- c("2023-11-01", "2023-12-01") |> as.Date()

test_that("date_label works as expected", {
  expect_equal(date_label(one_date), "Nov 2023")
  expect_equal(date_label(two_dates), c("Nov 2023", "Dec 2023"))
})
