test_that("util_app_mol works", {
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    KEY_OBSERVER = "a",
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  expect_equal(util_app_mol(md, as.factor(rep(1, nrow(md)))),
               as.factor(c(rep(3, 23), rep(4, 3))))
})
