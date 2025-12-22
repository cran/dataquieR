test_that("prep_compare_meta_with_study works", {
  skip_on_cran() # online, fragile
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  sd0 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)

  my_warnings <- capture_warnings({
    res <- prep_compare_meta_with_study(sd0)
  })
  expect_snapshot_value(res, style = "deparse")
  expect_snapshot_value(my_warnings, style = "deparse")
})
