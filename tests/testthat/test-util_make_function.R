test_that("util_make_function works", {
  skip_on_cran() # used by deprecated dq_report
  acc_test <- function(resp_vars, s, w, m) {
    if (!missing(w)) warning(w)
    if (!missing(m)) message(m)
    if (!missing(s)) stop(s)
    resp_vars
  }
  f  <- util_make_function(acc_test)
  s  <- f(resp_vars = "SEX_0", s = "ErrorMessage")
  w  <- f(resp_vars = "SEX_0", w = "Warning")
  m  <- f(resp_vars = "SEX_0", m = "Message")
  a1 <- f(resp_vars = "SEX_0",
         s = "ErrorMessage",
         m = "Message",
         w = "Warning")
  a2 <- f(resp_vars = "SEX_0",
         m = "Message",
         w = "Warning")
  a3 <- f(resp_vars = "SEX_0",
          w = "Warning")
  capture_output(
    expect_equivalent(a1, list())
  )
  capture_output(
    expect_condition(print(a1))
  )
  errorm <- paste(capture.output(print(s), type = "message"), collapse = "\n")

  expect_match(errorm,
               regexp = "ErrorMessage",
               perl = TRUE)
  capture_output(
    expect_equal(expect_warning(print(w)), "SEX_0")
  )
  capture_output(
    expect_equal(expect_message(print(m)), "SEX_0")
  )
  expect_message(
    expect_warning(print(a2), regexp = "Warning"),
    regexp = "Message"
  )
})
