test_that("util_app_im works", {
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = DATA_TYPES$FLOAT,
    MISSING_LIST =
      c(rep("1|9", 10), rep(NA, 3), rep("1|9", 13)),
    JUMP_LIST =
      c(rep("1|9", 12), rep(NA, 3), "", rep("1|9", 10))
  )
  expect_equal(util_app_im(md, as.factor(rep(1, nrow(md)))),
               as.factor(c(rep(3, 10), rep(2, 5), rep(3, 11))))
  md$JUMP_LIST <- NULL
  md$MISSING_LIST <- NULL
  expect_equal(util_app_im(md, as.factor(rep(1, nrow(md)))),
               as.factor(rep(2, 26)))
})
