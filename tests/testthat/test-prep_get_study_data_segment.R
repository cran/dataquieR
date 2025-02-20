test_that("prep_get_study_data_segment works", {
  skip_on_cran() # needs online access
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  sds <-
    prep_get_study_data_segment("LAB", "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                                meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  expect_snapshot_value(sds, style = "json2")
})
