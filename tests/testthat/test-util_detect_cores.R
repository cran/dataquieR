test_that("util_detect_cores works", {
  skip_on_cran() # not really useful, yet
  expect_equal(util_detect_cores(), parallel::detectCores())
  expect_warning(expect_equal(with_mock(requireNamespace = function(...) {
    return(FALSE)
  },
  util_detect_cores()
  ), 1),
  regexp = "parallel not found")
})
