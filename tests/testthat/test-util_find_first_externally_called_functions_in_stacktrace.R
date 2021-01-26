test_that("util_find_first_externally_called_functions_in_stacktrace works", {
  test1 <- function() {
    util_find_first_externally_called_functions_in_stacktrace()
  }
  environment(test1) <- asNamespace("dataquieR")
  test2 <- function() {
    test1 <- function() {
      util_find_first_externally_called_functions_in_stacktrace()
    }
    environment(test1) <- asNamespace("dataquieR")
    test1()
  }
  expect(
    ok = test2() == 1,
    info = "before shifting test2 to dataquieR, result is 1",
    failure_message = "external call detection wrong?"
  )
  environment(test2) <- asNamespace("dataquieR")
  expect(
    ok = test2() == 2,
    info = "after shifting test2 to dataquieR, result should now be 2",
    failure_message = "shifted, but external call detection still the old?"
  )
  expect_equal(
    util_find_first_externally_called_functions_in_stacktrace(
      sfs = NULL,
      cls = NULL),
    -1
  )
})
