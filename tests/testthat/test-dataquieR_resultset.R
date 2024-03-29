test_that("dataquieR_resultset works", {
  skip_on_cran()
  expect_error(dataquieR_resultset(list(a =  1, b =  2)))
})
