test_that("util_html_attr_quote_escape escapes quotes properly", {
  skip_on_cran()
  input_string <- 'Hello "World"!'
  expected_output <- 'Hello &quot;World&quot;!'
  actual_output <- util_html_attr_quote_escape(input_string)
  expect_equal(actual_output, expected_output)
})
