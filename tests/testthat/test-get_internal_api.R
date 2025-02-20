test_that(".get_internal_api works", {
  skip_on_cran() # internal use, only
  expect_true(".get_internal_api" %in%
    getNamespaceExports("dataquieR"))
  x <- .get_internal_api("util_html_table", version = "0.0.1", TRUE)
  expect_identical(x, util_html_table)
  expect_error(.get_internal_api("util_html_table", version = "0.0.1", FALSE))
  expect_error(.get_internal_api("util_html_table", version = "10", FALSE))
  expect_error(.get_internal_api("util_html_table", version = "10", TRUE))
})
