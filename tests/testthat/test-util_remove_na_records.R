test_that("util_remove_na_records works", {
  skip_on_cran()

  result <- util_remove_na_records(cars)

  # Check that the result has the correct structure
  expect_is(result, "data.frame")

  # Check that the result has the correct values when there are no NAs
  expect_identical(result, cars)

  # Check that the result shows a message when NAs where removed
  # Set a seed for reproducibility
  set.seed(123)

  # Generate random indices to replace with NA
  indices_to_replace1 <- sample(1:length(cars$dist), size = 10)
  indices_to_replace2 <- sample(1:length(cars$dist), size = 10)

  # Replace values at selected indices with NA
  cars_na <- cars
  cars_na$dist[indices_to_replace1] <- NA
  cars_na$speed[indices_to_replace2] <- NA

  # TODO: improve the message

  # Find how many NAs are left
  # nas <- sum(rowSums(is.na(cars_na[, colnames(cars_na), FALSE])) > 0)

  expect_message(result_remove_na <- util_remove_na_records(cars_na),
                 regexp = ". observations because of NAs in some of the following columns.",
                 perl = TRUE)

})
