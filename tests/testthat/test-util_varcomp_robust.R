test_that("util_varcomp_robust works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  r <- util_varcomp_robust(resp_vars = "SBP_0",
                           group_vars = "USR_BP_0",
                           study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                           meta_data = "item_level")
  expect_snapshot(r)
})
