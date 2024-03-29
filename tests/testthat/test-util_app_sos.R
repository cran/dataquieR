test_that("util_app_sos works", {
  skip_on_cran() # deprecated

  # Create a test metadata set
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    GROUP_VAR_OBSERVER = "a",
    DISTRIBUTION = c(rep(DISTRIBUTIONS$NORMAL, 10),
                     DISTRIBUTIONS$GAMMA,
                     DISTRIBUTIONS$UNIFORM,
                     rep(NA, 14)),
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )

  result1 <- util_app_sos(md, as.factor(rep(1, nrow(md))))

  # Check that the result is a factor
  expect_s3_class(result1, "factor")

  # check result
  expect_equal(result1,
               as.factor(c(rep(3, 12), rep(2, 11), rep(4, 3))))

  md0 <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    GROUP_VAR_OBSERVER = "a",
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )

  result2 <- util_app_sos(md0, as.factor(rep(1, nrow(md))))

  # check result
  expect_equal(result2,
               as.factor(c(rep(2, 23), rep(4, 3))))
})
