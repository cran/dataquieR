test_that("util_ensure_character works", {
  skip_on_cran()
  # Check that the result is a character
  expect_type(util_ensure_character(666), "character")

  # Check that the result is as expected
  expect_equal(util_ensure_character(c(666, "abc", pi)), c("666", "abc", "3.14159265358979"))

  # Check error messages
  ID <- c("111", "222", "333", "444")
  Age <- c(23, 41, NA, "7a")
  df <- data.frame(ID, Age)

  expect_error(util_ensure_character(df, TRUE), ".*was not possible for all of its values.*")
  expect_error(util_ensure_character(df, TRUE, "My error message"), "My error message")
  })
