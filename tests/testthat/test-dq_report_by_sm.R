test_that("dq_report_by works with content sm", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # TODO: test with all sorts of _by calls and on windows
  target <- withr::local_tempdir("testdqareportby")

  sd0 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 20)
  md0 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level")

  # fewer segments to speed up things.
  # can be solved in a more elegant way after
  # https://gitlab.com/libreumg/dataquier/-/issues/481

  md0$STUDY_SEGMENT <- "STUDY"

  dq_report_by( sd0,
                meta_data = md0,
                dimensions = "int",
                cores = NULL,
                segment_column = STUDY_SEGMENT,
                strata_column = "SEX_0",
                meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                output_dir = file.path(target, "sm"),
                also_print = TRUE)

  rm(md0)

  sd0[2, "v00002"] <- 2
  expect_warning(dq_report_by(study_data = sd0,
                              meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                              output_dir = !!file.path(target, "sm2"),
                              cores = NULL,
                              also_print=TRUE,
                              strata_column ="v00002",
                              segment_column = "STUDY_SEGMENT",
                              strata_select = "1",
                              segment_select = "STUDY",
                              dimensions = "Integrity"),
                 "The stratum/strata.+not present in the metadata")

  # TODO: check target path for all expected artifacts, namely the dq2 files, the .output_dirs containing the HTML reports and the summary RDS files

})
