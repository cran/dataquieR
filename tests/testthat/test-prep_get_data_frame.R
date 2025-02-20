test_that("prep_get_data_frame works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran()
  prep_purge_data_frame_cache()
  expect_equal(length(prep_list_dataframes()), 0)
  expect_error(prep_get_data_frame("", .data_frame_list = cars),
               "must be an environment")
  expect_error(prep_get_data_frame("ship"),
               "shortcuts in test")
  expect_error(prep_get_data_frame("ship_subset1"),
               "shortcuts in test")
  expect_error(prep_get_data_frame("ship_meta_v2"),
               "shortcuts in test")
  expect_error(prep_get_data_frame("ship_meta_dataframe"),
               "shortcuts in test")
  expect_error(prep_get_data_frame("ship_meta"),
               "shortcuts in test")
  expect_error(prep_get_data_frame("study_data|study_data"),
               "shortcuts in test")
  expect_error(prep_get_data_frame("meta_data_v2"),
               "shortcuts in test")
  expect_error(prep_get_data_frame("meta_data"),
               "shortcuts in test")


})
