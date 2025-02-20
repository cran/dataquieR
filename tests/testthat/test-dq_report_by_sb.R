test_that("dq_report_by works with subgroup", {
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # TODO: test with all sorts of _by calls and on windows
  target <- withr::local_tempdir("testdqareportby")

  sd1 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
              20)

  expect_message(dq_report_by(meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              study_data = sd1,
                              output_dir = !!file.path(target, "sb"),
                              also_print = TRUE,
                              cores = NULL,
                              label_col = "LABEL",
                              segment_column = "STUDY_SEGMENT",
                              segment_select = c("LAB"),
                              strata_column = "SEX_0",
                              strata_exclude = "0",
                              selection_type = "value",
                              subgroup = "[v00003] == 56",
                              dimensions = "Integrity"))

  expect_message(dq_report_by(meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              study_data = sd1,
                              also_print = FALSE,
                              cores = NULL,
                              label_col = "LABEL",
                              segment_column = "STUDY_SEGMENT",
                              segment_select = c("LAB"),
                              strata_column = "SEX_0",
                              strata_exclude = "0",
                              selection_type = "value",
                              subgroup = "[AGE_0] == 56",
                              dimensions = "Integrity"))

  expect_error(dq_report_by(meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              study_data = sd1,
                              also_print = FALSE,
                              cores = NULL,
                              label_col = "LABEL",
                              segment_column = "STUDY_SEGMENT",
                              segment_select = c("LAB"),
                              strata_column = "SEX_0",
                              strata_exclude = "0",
                              selection_type = "value",
                              subgroup = "[AGGGE_0] == 56",
                              dimensions = "Integrity"),
               regexp = "The subgroup rule.+not acceptable.")
})
