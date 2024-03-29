test_that("prep_dq_data_type_of works", {
  skip_on_cran()
  expect_equal(prep_dq_data_type_of(1:10), "integer")
  expect_null(prep_dq_data_type_of(cars))
})
