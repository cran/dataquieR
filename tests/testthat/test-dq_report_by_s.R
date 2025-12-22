test_that("dq_report_by works with content s", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
    skip_on_cran() # slow test
    # TODO: test with all sorts of _by calls and on windows
    target <- withr::local_tempdir("testdqareportby")

    sd0 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100)

    expect_message2(dq_report_by(sd0,
                 dimensions = "int",
                 cores = NULL,
                 strata_column = "SEX_0",
                 strata_select = "0", selection_type = "value",
                 meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                 output_dir = !!file.path(target, "s"),
                 also_print = TRUE))

    # TODO: check target path for all expected artifacts, namely the dq2 files, the .output_dirs containing the HTML reports and the summary RDS files

})


test_that("dq_report_by works with complex limits", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  # TODO: test with all sorts of _by calls and on windows
  target <- withr::local_tempdir("testdqareportby")

  sd0 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 50)

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
  md <- prep_get_data_frame("item_level")
  cil <- prep_get_data_frame("cross-item_level")
  cil <- rbind(cil, c("SBP_0", rep(NA, 12) ))
  cil$HARD_LIMITS <- c(rep(NA, 12), "[SBP_0] > 80 and [SBP_0] < 180")

  expect_message2(dq_report_by(sd0,
                              dimensions = "con",
                              cores = NULL,
                              segment_select = "PHYS_EXAM",
                              item_level = md,
                              meta_data_cross_item = cil,
                              output_dir = !!file.path(target, "c"),
                              also_print = TRUE))

})

