test_that("nres works", {
  skip_on_cran() # slow, errors unlikely

  prep_load_workbook_like_file("meta_data_v2")
  withr::defer(prep_purge_data_frame_cache())
  report <-
    dq_report2("study_data", dimensions = c("int"), label_col = "LABEL",
               cores = 1);
  expect_equal(nres(report), 10)
})
