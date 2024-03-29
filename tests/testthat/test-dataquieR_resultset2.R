test_that("dataquieR_resultset2 works", {
  skip_on_cran()
  # Call the function
  result <- dataquieR_resultset2(a = 1, b = 2, c = 3)

  # Check that the result is a dataquieR_resultset_class2
  expect_is(result, "dataquieR_resultset2")
})
