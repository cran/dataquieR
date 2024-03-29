test_that("prep_title_escape works", {
  skip_on_cran()
  # html = TRUE
  expected_resut <- c('Hello' = "Hello", 'world!' = "world!")
  expect_equal(prep_title_escape(c("Hello", "world!"), html = TRUE), expected_resut)

  # html = FALSE
  expect_equal(prep_title_escape("Hello world!", html = FALSE), "`Hello world!`")
  expect_equal(prep_title_escape(c("String1", "String2"), html = FALSE), c("`String1`", "`String2`"))
  expect_equal(prep_title_escape("Test `String`", html = FALSE), "`Test String`")
})
