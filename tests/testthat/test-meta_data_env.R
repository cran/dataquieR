test_that("util_meta_data_env works", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  b <- util_meta_data_env(meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx",
                          study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS")
  xx <- b$call(acc_margins(resp_vars = "sbp1",
                     group_vars = "DEV_BP_0"))
  prep_purge_data_frame_cache()
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx")
  yy <- acc_margins(resp_vars = "sbp1",
                    group_vars = "DEV_BP_0",
                    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS",
                    meta_data = "item_level",
                    co_vars = c("SEX_0", "AGE_0"),
                    label_col = LABEL)
  expect_equal(xx$ResultData, yy$ResultData, ignore_attr = TRUE)
  expect_equal(xx$SummaryPlot, yy$SummaryPlot, ignore_attr = TRUE)
  expect_equal(xx$SummaryTable[, -1, FALSE],
               yy$SummaryTable[, -1, FALSE],
               ignore_attr = TRUE)
})
