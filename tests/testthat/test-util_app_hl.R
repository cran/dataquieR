test_that("util_app_hl works", {
  skip_on_cran()
  # 0. Non-Matching data types
  #  - return 1: data type mismatches but applicable
  #  - return 4: not applicable because of not suitable data type

  # Create a test data set
  study_data <- cars
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$STRING),
    MISSING_LIST = "",
    HARD_LIMITS = c("[5.5;10.25]", "")
  )

  result0 <- util_app_hl(meta_data, dta = c(0, 0))

  # Check that the result is a factor
  expect_s3_class(result0, "factor")

  # Check that the result is as expected
  expect_equal(result0, as.factor(c(1, 4)))

  # 1. Matching data types but no hard limits defined
  #  - return 2: data type matches but not applicable

  # Create a test data set
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = ""
  )

  result1 <- util_app_hl(meta_data, dta = c(1,1))

  # Check that the result is a factor
  expect_s3_class(result1, "factor")

  # Check that the result is as expected
  expect_equal(result1, as.factor(c(2, 2)))

  # 2. Matching data types and hard limits defined
  #  - return 3: data type matches and applicable

  # Create a test data set
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = "",
    HARD_LIMITS = c("[5;25]", "[1;100]")
  )

  result2 <- util_app_hl(meta_data, dta = c(1,1))

  # Check that the result is as expected
  expect_equal(result2, as.factor(c(3, 3)))

  # 3. Non-Matching data types and hard limits defined
  #  - return 3: data type matches and applicable
  #  - return 4: not applicable because of not suitable data type

  # Create a test data set
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    MISSING_LIST = "",
    HARD_LIMITS = c("[5;25]", "[1;100]")
  )

  result3 <- util_app_hl(meta_data, dta = c(1,1))

  # Check that the result is as expected
  expect_equal(result3, as.factor(c(3, 4)))

})
