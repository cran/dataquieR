test_that("util_ds1_eval_env works", {
  md <- prep_get_data_frame("meta_data")
  sd <- prep_get_data_frame("study_data")
  md <- md[md$VAR_NAMES %in% colnames(sd), , FALSE]
  sd <- sd[, intersect(md$VAR_NAMES, colnames(sd)), FALSE]
  md$xx <- abbreviate(md$LABEL)
  e <- util_ds1_eval_env(sd, md,
                         label_col = "xx")
  expect_equal(
    sort(ls(e)),
    sort(unique(c(
      md$VAR_NAMES,
      md$LABEL,
      md$LONG_LABEL,
      md$xx)))
  )
})
