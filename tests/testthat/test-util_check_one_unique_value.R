test_that("util_check_one_unique_value works", {
  skip_on_cran()
  expect_false(util_check_one_unique_value(1:2))
  expect_false(util_check_one_unique_value(c(1:2, NA)))
  expect_true(util_check_one_unique_value(c(1, NA)))
  expect_false(util_check_one_unique_value(c(NA, NA)))
})
