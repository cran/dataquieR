context("test-util_count_NA")

test_that("util_count_NA works", {
  skip_on_cran()
  expect_equal(dataquieR:::util_count_NA(c(rep(NA, 10), rnorm(12), 12,
                                           rep(NA, 3))), 13)
})
