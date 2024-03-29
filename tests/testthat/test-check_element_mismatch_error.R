test_that("check_element_mismatch_error works", {
  skip_on_cran()
  d1 <- data.frame(id = c(1,  2,  3), value1 = c("a", "b", "c"))
  d2 <- data.frame(id = c(2,  3,  4), value2 = c("x", "y", "z"))

  result1 <- check_element_mismatch_error(d1, d1, "id")
  expect_equal(result1[[1]], list(NULL))
  expect_equal(result1[[2]], list(NULL))

  result2 <- check_element_mismatch_error(d1, d2, "id")
  expect_equal(result2[[1]], list("value1")) # 'value1' column to be present only in d1
  expect_equal(result2[[2]], list("value2")) # 'value2' column to be present only in d2
})
