test_that("util_app_sm works", {
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = DATA_TYPES$FLOAT,
    MISSING_LIST = "",
    KEY_STUDY_SEGMENT =
      c(rep(1, 10), rep(NA, 6), rep(3, 10))
  )
  expect_equal(util_app_sm(md, as.factor(rep(1, nrow(md)))),
               as.factor(c(rep(3, 10), rep(2, 6), rep(3, 10))))
})
