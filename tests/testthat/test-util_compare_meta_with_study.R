test_that("util_compare_meta_with_study works", {#####
  skip_on_cran()
  study_data <- cars
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = ""
  )

  expect_equal(util_compare_meta_with_study(study_data, meta_data,
                                            label_col = VAR_NAMES),
               c(speed = 1, dist = 1))

  study_data$speed <- study_data$speed + rnorm(nrow(study_data))
  expect_equal(util_compare_meta_with_study(study_data, meta_data,
                                            label_col = VAR_NAMES),
               c(speed = 0, dist = 1))

  study_data <- cars
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$FLOAT),
    MISSING_LIST = ""
  )
  expect_equal(util_compare_meta_with_study(study_data, meta_data,
                                            label_col = VAR_NAMES),
               c(speed = 1, dist = 1))
  study_data$speed <- study_data$speed + rnorm(nrow(study_data))
  expect_equal(util_compare_meta_with_study(study_data, meta_data,
                                            label_col = VAR_NAMES),
               c(speed = 1, dist = 1))

  # note, that other data types are tested with the function
  # util_check_data_type, that util_compare_meta_with_study bases on

})
