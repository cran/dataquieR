test_that("dq_report_by works", {
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  reports <- suppressWarnings(dq_report_by(v1.0 = TRUE,
                                           meta_data_split = STUDY_SEGMENT,
                                           cores = 1,
                                           study_data_split = "CENTER_0",
                                       subset(study_data, # speed: CENTER_0 1:2
                                              v00000 %in% 1:2),
                                       meta_data,
                                       label_col = LABEL,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness")
  ))
  expect_equal(sum(
    reports$`CENTER_0 = 1`$PART_STUDY$long_format$com_item_missingness$results[[
      1]]$SummaryTable$GRADING, na.rm = TRUE), 0)

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  reports <- suppressWarnings(dq_report_by(
                                       v1.0 = TRUE,
                                       cores = 1,
                                       subset(study_data, # speed: CENTER_0 1:2
                                              v00000 %in% 1:2),
                                       meta_data,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness")
  ))
  expect_equal(sum(
    reports$all_observations$v10000$long_format$com_item_missingness$results[[
      1]]$SummaryTable$GRADING, na.rm = TRUE), 0)


  expect_error(
    reports <- suppressWarnings(dq_report_by(
      v1.0 = TRUE,
      cores = 1,
      subset(study_data, # speed: CENTER_0 1:2
             v00000 %in% 1:2),
      meta_data,
      meta_data_split = GROUP_VAR_DEVICE,
      dimensions = # for speed, omit Accuracy
        c("Completeness")
    )),
    regexp =
      "Only STUDY_SEGMENT is supported for meta_data_split up to now.",
    fixed = TRUE
  )


  md0 <- meta_data
  md0$STUDY_SEGMENT <- NULL
  md0$KEY_STUDY_SEGMENT <- NULL
  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  reports <- suppressWarnings(dq_report_by(
    v1.0 = TRUE,
    cores = 1,
    subset(study_data, # speed: CENTER_0 1:2
           v00000 %in% 1:2),
    md0,
    dimensions = # for speed, omit Accuracy
      c("Completeness")
  ))
  expect_equal(sum(
    reports$all_observations$all_variables$long_format$
      com_item_missingness$results[[1]]$SummaryTable$GRADING,
    na.rm = TRUE), 34)

})
