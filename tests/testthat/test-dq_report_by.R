test_that("dq_report_by works", {
  skip_on_cran() # slow test
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  reports <- suppressWarnings(dq_report_by(meta_data_split = KEY_STUDY_SEGMENT,
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
      cores = 1,
      subset(study_data, # speed: CENTER_0 1:2
             v00000 %in% 1:2),
      meta_data,
      meta_data_split = KEY_DEVICE,
      dimensions = # for speed, omit Accuracy
        c("Completeness")
    )),
    regexp =
      "Only KEY_STUDY_SEGMENT is supported for meta_data_split up to now.",
    fixed = TRUE
  )


  md0 <- meta_data
  md0$KEY_STUDY_SEGMENT <- NULL
  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  reports <- suppressWarnings(dq_report_by(
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
