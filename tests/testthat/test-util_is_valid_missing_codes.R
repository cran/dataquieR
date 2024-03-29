test_that("util_is_valid_missing_codes works", {
  skip_on_cran()
  # Only NA values
  expect_true(util_is_valid_missing_codes(c(NA, NA, NA)))

  # Mixed NA and numeric values
  expect_true(util_is_valid_missing_codes(c(NA,  1,  2)))

  # Character values that are numeric strings
  expect_false(util_is_valid_missing_codes(c("NA", "1", "2")))

  # Character values that cannot be coerced to numeric
  expect_false(util_is_valid_missing_codes(c("NA", "abc", "def")))

  # Date strings that can be converted to datetime
  expect_true(util_is_valid_missing_codes(c("2024-01-01  12:00:00", "2024-01-02  13:00:00")))

  # Date strings that cannot be converted to datetime without warnings
  expect_false(util_is_valid_missing_codes(c("not a date", "2024-13-01  12:00:00")))
})
