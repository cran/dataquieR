test_that("util_app_dl works", {
  skip_on_cran() # deprecated

  # Create a test metadata set
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )

  md_no_names <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = ""
  )

  result1 <- util_app_dl(md, as.factor(rep(1, nrow(md))))
  result2 <- util_app_dl(md_no_names, as.factor(rep(1, nrow(md_no_names))))

  # Check that the result is a factor
  expect_s3_class(result1, "factor")

  # Check that the result is as expected
  expect_equal(result1,
               as.factor(c(rep(3, 10), rep(2, 6), rep(3, 7), rep(4, 3))))

  expect_equal(result2,
               as.factor(c(rep(2, 23), rep(4, 3))))
})
