test_that("util_remove_empty_rows works", {
  skip_on_cran()

  result <- util_remove_empty_rows(cars)

  # Check that the result has the correct structure
  expect_is(result, "data.frame")

  # Check that the result has the correct values when there are no empty rows
  expect_identical(result, cars)

  # Check that the result has the correct number of rows when there are empty rows
  # Set a seed for reproducibility
  set.seed(123)

  # Generate random indices to replace with empty string
  n_empty_strings <- 10
  indices_to_replace1 <- sample(1:length(cars$dist), size = n_empty_strings)

  # Replace values at selected indices with empty string
  cars_na <- cars
  cars_na$dist[indices_to_replace1] <- ""
  cars_na$speed[indices_to_replace1] <- ""

  result_na <- util_remove_empty_rows(cars_na)

  expect_equal(nrow(result_na), nrow(cars)-n_empty_strings)

})
