test_that("util_looks_like_missing works", {
  skip_on_cran()
  expected <- rep(FALSE, 100)
  expected[c(88, 89, 98, 99)] <- TRUE
  expect_equal(
    util_looks_like_missing(1:100, n_rules = 0),
    expected = expected
  )
  expected <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  expect_equal(
    util_looks_like_missing(c(99999, 99, 8, 888, -999, 9.99, 8.9898),
      n_rules = 0),
    expected = expected)
  expected <- rep(FALSE, 100)
  expect_equal(util_looks_like_missing(1:100),
               expected = expected)
  expected <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(
    util_looks_like_missing(c(99999, 99, 8, 888, -999, 9.99, 8.9898)),
    expected = expected
  )
  expected <- c(TRUE, FALSE)
  expect_equal(
    util_looks_like_missing(c(-Inf, 9)),
    expected = expected
  )
  expect_equal(
    util_looks_like_missing(NA_integer_),
    TRUE
  )
  expect_equal(
    util_looks_like_missing(c(Inf, -Inf, NA, NaN)),
    rep(TRUE, 4)
  )

})
