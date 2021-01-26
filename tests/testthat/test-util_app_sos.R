test_that("util_app_sos works", {
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
                  DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    MISSING_LIST = "",
    KEY_OBSERVER = "a",
    DISTRIBUTION = c(rep(DISTRIBUTIONS$NORMAL, 10),
                     DISTRIBUTIONS$GAMMA,
                     DISTRIBUTIONS$UNIFORM,
                     rep(NA, 14)),
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  expect_equal(util_app_sos(md, as.factor(rep(1, nrow(md)))),
               as.factor(c(rep(3, 12), rep(2, 11), rep(4, 3))))
})
