test_that("util_user_hint works", {
  skip_on_cran()
  test_text <- sprintf(
    paste0("Without the package %s, I cannot decide, if your RStudio is",
           "at least at version 1.3.1056, so I'll activate a work-around",
           "for a known parallel-bug fixed in newer RStudios."),
    dQuote("rstudioapi")
  )

  result <- util_user_hint(test_text)

  # Check that the result is silent
  expect_silent(result)

  # Check that the result is a list
  expect_is(result, "list")
})
