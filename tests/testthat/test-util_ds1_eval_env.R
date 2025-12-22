test_that("util_ds1_eval_env works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  md <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  sd <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
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
