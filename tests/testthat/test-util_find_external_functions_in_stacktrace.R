test_that("util_find_external_functions_in_stacktrace works", {
  test1 <- function() {
    util_find_external_functions_in_stacktrace()
  }
  environment(test1) <- asNamespace("dataquieR")
  test2 <- function() {
    test1 <- function() {
      util_find_external_functions_in_stacktrace()
    }
    environment(test1) <- asNamespace("dataquieR")
    test1()
  }
  a <- test2()
  expect_equal(
    a[1:1], 3,
  )
  environment(test2) <- asNamespace("dataquieR")
  b <- test2()
  expect_equal(
    b, a[-1]
  )
})
