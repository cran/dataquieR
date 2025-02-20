test_that("prep_load_folder_with_metadata works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran()
  prep_purge_data_frame_cache()
  expect_equal(length(prep_list_dataframes()), 0)
  suppressWarnings(prep_load_folder_with_metadata("https://dataquality.qihs.uni-greifswald.de/extdata/fortests"))
  expect_gt(length(prep_list_dataframes()), 10)
})
