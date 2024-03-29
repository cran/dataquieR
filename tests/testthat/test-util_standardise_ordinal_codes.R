test_that("util_standardise_ordinal_codes works", {
  skip_on_cran()
  # Increasing ordinal codes
  codes <- c(1,  2,  3,  4,  5)
  maxlevel_old <-  5
  maxlevel_new <-  10
  expected_result <- c(1,  3,  5,  8,  10)
  expect_equal(util_standardise_ordinal_codes(codes, maxlevel_old, maxlevel_new), expected_result)

  # Decreasing ordinal codes
  codes <- c(5,  4,  3,  2,  1)
  expected_result <- c(10,  8,  5,  3,  1)
  expect_equal(util_standardise_ordinal_codes(codes, maxlevel_old, maxlevel_new), expected_result)

  # Equal ordinal codes
  codes <- rep(3,  5)
  expected_result <- rep(5,  5)
  expect_equal(util_standardise_ordinal_codes(codes, maxlevel_old, maxlevel_new), expected_result)

  # Invalid input
  expect_error(util_standardise_ordinal_codes(c(1,  2, "three"), maxlevel_old, maxlevel_new))
  expect_error(util_standardise_ordinal_codes(codes,  0, maxlevel_new))
  expect_error(util_standardise_ordinal_codes(codes, -1, maxlevel_new))
})
