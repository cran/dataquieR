test_that("prep_map_labels works", {
  skip_on_cran()
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_create_meta(
    VAR_NAMES = c("ID", "SEX", "AGE", "DOE"),
    LABEL = c("Pseudo-ID", "Gender", "Age", "Examination Date"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$INTEGER,
                  DATA_TYPES$DATETIME),
    MISSING_LIST = ""
  )
  expect_equal(prep_map_labels(c("AGE", "DOE"), meta_data),
                   c(AGE = "Age", DOE = "Examination Date"))
  expect_error(prep_map_labels(c("NOT_AVAIL", "AGE"), meta_data),
              regexp = "value for .+NOT_AVAIL.+ not found", perl = TRUE)
  expect_equal(prep_map_labels(c("AGE", "NOT_AVAIL"), meta_data,
                               ifnotfound = NA_character_),
               c(AGE = "Age", NOT_AVAIL = NA_character_))
  expect_equal(prep_map_labels(c("AGE", "NOT_AVAIL"), meta_data,
                               ifnotfound = 42),
               c(AGE = "Age", NOT_AVAIL = 42))
  expect_error(prep_map_labels(meta_data = 42),
               regexp =
                 paste(".+meta_data.+ is not a data frame")
  )
  expect_error(prep_map_labels(x = function(){},
                               meta_data = meta_data),
               regexp =
                 paste("all variable source names must be characters")
  )
  expect_error(prep_map_labels(x = meta_data$VAR_NAMES,
                               meta_data = meta_data,
                               to = 42),
               regexp =
                 paste("Need exactly one existing variable",
                       "attribute name to map variables to")
  )
  expect_error(prep_map_labels(x = meta_data$VAR_NAMES,
                               meta_data = meta_data,
                               to = letters[1:5]),
               regexp =
                 paste("Need exactly one existing variable",
                       "attribute name to map variables to")
  )
  expect_error(prep_map_labels(x = meta_data$VAR_NAMES,
                               meta_data = meta_data,
                               to = "vegetable soup"),
               regexp =
                 paste("Need exactly one existing variable",
                       "attribute name to map variables to")
  )

  expect_error(prep_map_labels(x = meta_data$VAR_NAMES,
                               meta_data = meta_data,
                               from = 42),
               regexp =
                 paste("Need exactly one variable",
                       "attribute name to use as variable name on mapping")
  )
  expect_error(prep_map_labels(x = meta_data$VAR_NAMES,
                               meta_data = meta_data,
                               from = letters[1:5]),
               regexp =
                 paste("Need exactly one variable",
                       "attribute name to use as variable name on mapping")
  )
  expect_error(prep_map_labels(x = meta_data$VAR_NAMES,
                               meta_data = meta_data,
                               from = "vegetable soup"),
               regexp =
                 paste("Need exactly one variable",
                       "attribute name to use as variable name on mapping")
  )


})
