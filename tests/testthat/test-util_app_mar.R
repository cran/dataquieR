test_that("util_app_mar works", {
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    KEY_OBSERVER = c("a", NA),
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  expect_equal(util_app_mar(md, as.factor(rep(1, nrow(md)))),
               as.factor(c(rep(c(3, 2), 11), 3, rep(4, 3))))
})
