test_that("util_parallel_futures works", {
  # testthat::skip_if(identical(Sys.getenv("R_COVR"), "true"),
  #                   message = "Crashes, if instrumented")
  skip_on_cran() # slow, parallel, ...
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("future")
  skip_if_not_installed("stringdist")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100)
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| missing_table")

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL

  # md0$MISSING_LIST_TABLE <- NULL

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report2
  # here in the first place
  expect_warning({
    expect_warning({
      report <- dq_report2(sd0, md0,
                           resp_vars = c("v00000", "v00001", "v00002",
                                         "v00003", "v00004", "v00012"),
                           filter_indicator_functions =
                             c("^com_item_missingness$",
                               "^acc_varcomp$"),
                           filter_result_slots =
                             c("^SummaryTable$"),
                           cores = list(mode = "socket", cores = 1),
                           mode = "futures",
                           dimensions = # for speed, omit Accuracy
                             c("Integrity",
                               "Completeness",
                               "Consistency",
                               "Accuracy"))
    }, regexp =
      ".*context*",
    perl = TRUE)}, regexp =
      ".*context*",
    perl = TRUE)

  if (nres(report) == 0) {
    if (identical(Sys.getenv("CI_PROJECT_ID"), "10015470")) { # only in our gitlab CI pipeline
      p <- file.path(path.expand("~"), "addtional_output")
      if (!dir.exists(p)) {
        dir.create(p, recursive = TRUE)
      }
      if (dir.exists(p)) {
        save(report, file = file.path(p, "util_parallel_futures_report.RData"))
      }
    }
  }

  expect_equal(dim(report), c(6, 3, 1))
  expect_snapshot(summary(report))

})
