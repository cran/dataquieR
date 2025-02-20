test_that("dq_report_by works with input and output dir", {
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran()
  target <- withr::local_tempdir("testdqareportby_v2")
  target <- gsub('\\\\','/', target)
  sd1 <- head(
    prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    n = 10)

  library(rio)
  export(sd1,file.path(target, "data_origin.RData") )


  try(utils::download.file(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
    destfile = file.path(target, "meta_data_v2.xlsx"),
    quiet = TRUE, mode = "wb"), silent = TRUE)

  target1 <- file.path(target, "test01")

  expect_message(dq_report_by(study_data = "data_origin.RData",
                              input_dir = target,
                              dimensions = "int",
                              cores = NULL,
                              strata_column = "v00002",
                              strata_select = "0",
                              meta_data_v2 = "meta_data_v2.xlsx",
                              output_dir = target1,
                              also_print = TRUE))


  expect_error(dq_report_by(study_data = "data_origin.RData",
                              input_dir = target,
                              dimensions = "int",
                              cores = NULL,
                              strata_column = "v00002",
                              strata_select = "0",
                              meta_data_v2 = "meta_data_v2.xlsx",
                              output_dir = target1,
                              also_print = TRUE),
               regexp = ".+already exists. Remove the.+output_dir.+first")


  target_v3 <- withr::local_tempdir("testdqareportby_v3")
  target_v3 <- gsub('\\\\','/', target_v3)

  try(utils::download.file(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
    destfile = file.path(target_v3, "meta_data_v2.xlsx"),
    quiet = TRUE, mode = "wb"), silent = TRUE)

  sd1 <- head(
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    n = 10)
  export(sd1, file.path(target_v3, "data_origin.RData"))

  sd2 <- tail(
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    n = 10)
  export(sd2, file.path(target_v3, "data_origin_2.RData") )

  target2 <- file.path(target_v3, "test02")

  expect_warning(dq_report_by(study_data = c("data_origin.RData",
                                             "data_origin_2.RData"),
                              input_dir = target_v3,
                              dimensions = "int",
                              cores = NULL,
                              strata_column = "v00002",
                              strata_select = "0",
                              meta_data_v2 = "meta_data_v2.xlsx",
                              output_dir = target2,
                              also_print = TRUE),
                 regexp = "Because no id.+created duplicated rows.")

})
