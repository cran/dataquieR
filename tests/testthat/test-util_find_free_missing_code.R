test_that("util_find_free_missing_code works", {
  skip_on_cran()
  # Numeric codes
  x <- c(777,888,999)
  expected_next_code <-  1000
  expect_equal(util_find_free_missing_code(x), as.character(expected_next_code))

  # All values cannot be converted
  x <- c("abc", "def", NA, "ghi")
  expect_error(util_find_free_missing_code(x))

  # TODO: date-time values
})
