test_that("reflection works", {
  skip_on_cran() # snot really a problem, if not working. also obvious, if buggy. slow.
  util_load_manual()
  expect("titles" %in% names(..manual),
         failure_message = "load manual did not read titles")
})
