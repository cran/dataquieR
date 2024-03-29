test_that("util_set_sQuoteString works", {
  skip_on_cran()
  expect_equal(util_set_sQuoteString(
    paste0(LETTERS, letters, letters)
  ), paste0("'", paste0(LETTERS, letters, letters), "'"))
})
