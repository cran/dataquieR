test_that("util_only_NAs works", {
  expect_equal(util_only_NAs(1), 0)
  expect_equal(util_only_NAs(1:10), 0)
  expect_equal(util_only_NAs(NA), 1)
  expect_equal(util_only_NAs(matrix(nrow = 12, ncol = 17)), 1)
  expect_equal(util_only_NAs(matrix(nrow = 12, ncol = 17, data = 17)), 0)
})
