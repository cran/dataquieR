test_that("util_error works", {
  expect_error(util_error("The one and everything is %d (%s).", 42,
                          "Douglas Adams"),
               regexp = "The one and everything is 42 (Douglas Adams).",
               fixed = TRUE)
  x <- function(m) {
    stop(m)
  }
  y <- function(m) {
    x(m)
  }
  s <- function(e) {
    util_error(e)
    invokeRestart()
  }
  expect_error(
    withCallingHandlers(x(""), error = s),
    regexp = "Error",
    fixed = TRUE
  )
  expect_error(
    withCallingHandlers(x("Bye bye"), error = s),
    regexp = "Bye bye",
    fixed = TRUE
  )
  expect_error(
    do.call(y, list(m = "Hello")),
    regexp = "Hello",
    fixed = TRUE
  )
  expect_error(
    do.call(s, list(e = "Hello")),
    regexp = "Hello",
    fixed = TRUE
  )
})
