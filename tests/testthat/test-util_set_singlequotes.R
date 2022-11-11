test_that("util_set_sQuoteString works", {
  expect_equal(util_set_sQuoteString(
    paste0(LETTERS, letters, letters)
  ), paste0("'", paste0(LETTERS, letters, letters), "'"))
})
