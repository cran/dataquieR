test_that("reflection works", {
  skip_on_cran() # snot really a problem, if not working. also obvious, if buggy. slow.
  util_load_manual()
  expect("titles" %in% names(..manual),
         failure_message = "load manual did not read titles")
  expect_true(
    any(
      vapply(
        mget(ls(.indicator_or_descriptor),
             envir = .indicator_or_descriptor), identity,
        FUN.VALUE = logical(1))), info = "Any Indicator in Package")
})
