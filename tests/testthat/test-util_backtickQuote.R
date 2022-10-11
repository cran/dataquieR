test_that("util_backtickQuote works", {
  expect_equal(util_backtickQuote(""), "")
  expect_equivalent(util_backtickQuote(NA), NA_character_)
  nm <- paste(letters, LETTERS)
  t <- paste("c(", paste(util_backtickQuote(nm), collapse = ", "), ")")
  e <- parse(text = t)
  got <- eval(e, envir = as.list(setNames(LETTERS, nm = nm)))
  expect_equivalent(got, LETTERS)
})
