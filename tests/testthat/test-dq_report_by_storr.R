test_that("dq_report_by error_arguments_test", {
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("storr")

  target <- withr::local_tempdir("testdqareporstorr")
  storr_dir <- withr::local_tempdir("testdqareporstorr_dir")

  sd0 <- head(prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    20)
  sd0 <- sd0[, head(colnames(sd0), 10)]

  md0 <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level")

  md0 <- md0[md0$VAR_NAMES %in% colnames(sd0), ]
  # fewer segments to speed up things.
  # can be solved in a more elegant way after
  # https://gitlab.com/libreumg/dataquier/-/issues/481

  md0$STUDY_SEGMENT <- "STUDY"


  expect_message2(dq_report_by(sd0,
               meta_data = md0,
               dimensions = "int",
               strata_column = "SEX_0",
               cores = NULL,
               storr_factory = prep_create_storr_factory(storr_dir, "TestReport"),
               meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
               output_dir = file.path(target, "sm5"),
               also_print = TRUE))

  expect_true(
    dir.exists(file.path(target, "sm5", "report_sd0_SEX_0_0_all_variables"))
  )
  expect_true(
    dir.exists(file.path(target, "sm5", "report_sd0_SEX_0_1_all_variables"))
  )

})
