test_that("util_set_dQuoteString works", {
  expect_equal(util_set_dQuoteString(
    paste0(LETTERS, letters, letters)
  ), paste0('"', paste0(LETTERS, letters, letters), '"'))
})
