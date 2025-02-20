test_that("util_condition_from_try_error works", {
  x <- util_condition_from_try_error(try(stop("Test"), silent = TRUE))
  expect_s3_class(x, "simpleError")
  expect_equal(conditionMessage(x), "Test")
})
