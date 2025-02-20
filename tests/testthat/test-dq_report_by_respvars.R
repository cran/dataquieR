test_that("dq_report_by error_respvars", {
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  # TODO: test with all sorts of _by calls and on windows
  target <- withr::local_tempdir("testdqareportbyrespv")


  study_data <-
    head(prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
      20)



  expect_message(dq_report_by(
    resp_vars = c("CENTER_0", "PSEUDO_ID",
                  "AGE_0", "SEX_0",
                  "ARM_CUFF_0",
                  "SBP_0",
                  "DBP_0", "CRP_0", "BSG_0" ),
    study_data = study_data,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
    cores = NULL,
    also_print = FALSE,
    dimensions = "Integrity",
    segment_column = "STUDY_SEGMENT"))

  expect_warning(dq_report_by(
    resp_vars = c("v00000", "v00001", "v00002", "v00003",
                  "v00010",
                  "v00004", "v00005",
                  "v00020", "v00021"),
    cores = NULL,
    study_data =study_data,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
    output_dir = file.path(target, "resp2"),
    also_print=TRUE,
    dimensions = NULL,
    segment_column = "STUDY_SEGMENT"),
    regexp = "Entry VARIABLE_LIST for check.+not specified correctly.+")

})
