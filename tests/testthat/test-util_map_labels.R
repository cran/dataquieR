test_that("util_map_labels works", {
  meta_data <- prep_create_meta(
    VAR_NAMES = c("ID", "SEX", "AGE", "DOE"),
    LABEL = c("Pseudo-ID", "Gender", "Age", "Examination Date"),
    MISSING_LIST = "",
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$INTEGER,
                  DATA_TYPES$DATETIME)
  )
  expect_equal(util_map_labels(c("AGE", "DOE"), meta_data),
               c(AGE = "Age", DOE = "Examination Date"))
  expect_error(util_map_labels(c("NOT_AVAIL", "AGE"), meta_data),
               regexp = "value for .+NOT_AVAIL.+ not found", perl = TRUE)
  expect_equal(util_map_labels(c("AGE", "NOT_AVAIL"), meta_data,
                               ifnotfound = NA_character_),
               c(AGE = "Age", NOT_AVAIL = NA_character_))
  expect_equal(util_map_labels(c("AGE", "NOT_AVAIL"), meta_data,
                               ifnotfound = 42),
               c(AGE = "Age", NOT_AVAIL = 42))
})
