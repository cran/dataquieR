test_that("nres works", {
  skip_on_cran() # slow, errors unlikely
  skip_if_not_installed("stringdist")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  withr::defer(prep_purge_data_frame_cache())
  # item_level <- prep_get_data_frame("item_level")
  # item_level$MISSING_LIST_TABLE <- NULL
  # prep_add_data_frames(item_level)
  report <-
    dq_report2("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", dimensions = c("int"), label_col = "LABEL",
               cores = NULL, filter_result_slots = NULL);
  expect_equal(nres(report), 13)
})
