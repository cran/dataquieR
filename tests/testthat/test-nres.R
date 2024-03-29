test_that("nres works", {
  skip_on_cran() # slow, errors unlikely

  prep_load_workbook_like_file("meta_data_v2")
  withr::defer(prep_purge_data_frame_cache())
  # item_level <- prep_get_data_frame("item_level")
  # item_level$MISSING_LIST_TABLE <- NULL
  # prep_add_data_frames(item_level)
  report <-
    dq_report2("study_data", dimensions = c("int"), label_col = "LABEL",
               cores = 1, filter_result_slots = NULL);
  expect_equal(nres(report), 12)
})
