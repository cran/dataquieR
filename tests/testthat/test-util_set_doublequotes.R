test_that("util_set_dQuoteString works", {
  skip_on_cran()
  expect_equal(util_set_dQuoteString(
    paste0(LETTERS, letters, letters)
  ), paste0('"', paste0(LETTERS, letters, letters), '"'))
})
