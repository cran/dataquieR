test_that("prep_save_report correctly saves a report with correct parameters", {
  skip_on_cran() # slow, parallel, ...

  # Define a temporary file path for the test
  tmp_file <- tempfile(fileext = ".gz")

  # Not a dq_report2 object
  df <- data.frame(a =  1:5, b = letters[1:5])
  expect_error(prep_save_report(df, tmp_file))

  # Create example report
  prep_load_workbook_like_file("meta_data_v2")

  study_data <- prep_get_data_frame("study_data")
  meta_data <- prep_get_data_frame("item_level")

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report2
  # here in the first place
  report <- dq_report2(sd0, md0,
                       resp_vars = c("v00000", "v00001", "v00002",
                                     "v00003", "v00004", "v00012"),
                       filter_indicator_functions =
                         c("^com_item_missingness$",
                           "^acc_varcomp$"),
                       filter_result_slots =
                         c("^SummaryTable$"),
                       cores = 1,
                       dimensions = # for speed, omit Accuracy
                         c("Integrity",
                           "Completeness",
                           "Consistency",
                           "Accuracy"))

  prep_save_report(report, tmp_file)

  # Check that the file was created
  expect_true(file.exists(tmp_file))

  # Check the file size
  expect_gt(file.info(tmp_file)$size,  0)

  # Check for invalid compression_level
  expect_error(prep_save_report(report, tmp_file, compression_level =  10))

  # Clean up
  on.exit(unlink(tmp_file))
})
