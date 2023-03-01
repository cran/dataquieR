test_that("util_app_iac works", {
  skip_on_cran() # deprecated
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  expect_equal(util_app_iac(md, as.factor(rep(1, nrow(md)))),
               as.factor(c(rep(4, 13), rep(2, 11), 4, 2)))
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    VALUE_LABELS = c(rep(c(NA_character_, "", "12 = x | 14 = z", "|"), 26)),
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  testthat::local_edition(3)
  expect_snapshot_value(style = "deparse",
    util_app_iac(md, as.factor(c(rep(rep(1, nrow(md)), 52),
    rep(rep(0, nrow(md)), 52)))))
})
