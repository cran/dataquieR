test_that("util_startsWith_prefix._or_equals_prefix works", {
  skip_on_cran()

  # String that should start with the prefix
  expect_true(util_startsWith_prefix._or_equals_prefix("foo.baz", "foo"))

  # String that matches identically
  expect_true(util_startsWith_prefix._or_equals_prefix("foo", "foo"))

  # Strings that should not start with the prefix
  expect_false(util_startsWith_prefix._or_equals_prefix("baz.foo", "foo"))
  expect_false(util_startsWith_prefix._or_equals_prefix("quux", "foo"))
  expect_false(util_startsWith_prefix._or_equals_prefix("", "foo"))

  # Edge case
  expect_error(util_startsWith_prefix._or_equals_prefix(NA, "foo"))
})
