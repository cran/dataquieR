test_that("util_app_im works", {
  skip_on_cran() # deprecated
  expect_warning(
    md <- prep_create_meta(
      VAR_NAMES = letters,
      DATA_TYPE = DATA_TYPES$FLOAT,
      MISSING_LIST =
        c(rep("1 = NA | 9 = -", 10), rep(NA, 3), rep("1 = NA | 9 = -", 13)),
      JUMP_LIST =
        c(rep("1 = Not planned | 9 = exp. missing", 12), rep(NA, 3), "",
          rep("1 = Not planned | 9 = exp. missing", 10))
    ),
    regexp = paste("Found at least one missing code with more than one meaning",
                   "for .a., .b., .c., .d., .e., .f., .g., .h., .i., .j.,",
                   ".q., .r., .s., .t., .u., .v., .w., .x., .y., .z..")
  )
  expect_equal(util_app_im(md, as.factor(rep(1, nrow(md)))),
               as.factor(c(rep(3, 12), rep(2, 1), rep(3, 13))))
  md$JUMP_LIST <- NULL
  md$MISSING_LIST <- NULL
  expect_equal(util_app_im(md, as.factor(rep(1, nrow(md)))),
               as.factor(rep(2, 26)))
})
