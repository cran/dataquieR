test_that("dq_report_by works with pipe list in argum", {
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  target <- withr::local_tempdir("testdqareportby_pipe")

  study_data <- head(
    prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    20)

  expect_message(dq_report_by(study_data = study_data,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              segment_select ="STUDY | LAB",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m1"),
                              also_print = TRUE))

  expect_message(dq_report_by(study_data = study_data,
                              dimensions = "int",
                              cores = NULL,
                              segment_column = STUDY_SEGMENT,
                              segment_exclude ="STUDY | LAB",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m2"),
                              also_print = TRUE))

  expect_message(dq_report_by(study_data = study_data,
                              dimensions = "int",
                              cores = NULL,
                              strata_column = "SEX_0",
                              strata_select ="0|1",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m3"),
                              also_print = TRUE))

  study_data[is.na(study_data[, "v00008"]), "v00008"] <- "B"
  expect_message(dq_report_by(study_data = study_data,
                              dimensions = "int",
                              cores = NULL,
                              strata_column = "VO2_CAPCAT_0",
                              strata_exclude ="B | D |E",
                              selection_type = "value",
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m4"),
                              also_print = TRUE))

  expect_message(dq_report_by(study_data = c("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS",
                                             "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
                              meta_data_v2 ="https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "m5"),
                              cores = NULL,
                              also_print=TRUE,
                              segment_column = STUDY_SEGMENT,
                              segment_exclude = c("PHYS_EXAM", "LAB",
                                                  "INTERVIEW", "QUESTIONNAIRE"),
                              strata_column = "v00002",
                              strata_select = "females",
                              dimensions = "Integrity",
                              selection_type = "v_label",
                              subgroup = "[v00003]<40",
                              id_vars = "v00001|v00000"))

})
