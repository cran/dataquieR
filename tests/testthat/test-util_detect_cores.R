test_that("util_detect_cores works", {
  skip_on_cran() # not really useful, yet
  skip_if_not_installed("parallelly")
  expect_equal(util_detect_cores(), parallelly::availableCores())
  expect_warning(expect_equal(with_mock(requireNamespace = function(...) {
    return(FALSE)
  },
  util_detect_cores()
  ), 1),
  regexp = "None of the suggested packages.*are found")
})
