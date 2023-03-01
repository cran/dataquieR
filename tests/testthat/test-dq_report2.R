test_that("dq_report2 works", {
  skip_on_cran() # slow, parallel, ...
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)

  prep_load_workbook_like_file("meta_data_v2")

  study_data <- prep_get_data_frame("study_data")
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("meta_data_v2.xlsx| missing_table")

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

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

  sts <- report[, "com_item_missingness", "SummaryTable"]
  sts <- lapply(sts, `[[`, "SummaryTable")
  sts <- vapply(sts, `[[`, "GRADING", FUN.VALUE = numeric(1))

  expect_equal(sum(sts), 1)

  expect_silent(summary(report))

  r <- report
  r$acc_varcomp_observer.SBP_0$SummaryTable <- NULL
  expect_silent(summary(r))

  r <- report
  r$acc_varcomp_observer.SBP_0$SummaryTable <-
    r$acc_varcomp_observer.SBP_0$SummaryTable[FALSE, , FALSE]
  expect_silent(summary(r))

  expect_error(
    report <-
      suppressWarnings(dq_report2(sd0, md0,
                                 cores = 1,
                                 dimensions = 42)
      ),
    regexp =
      ".+dimensions.+must match the predicate.+is.character",
    perl = TRUE
  )


  expect_warning(
    report <-
      (dq_report2(sd0, md0,
                  resp_vars = c("v00000", "v00001", "v00002",
                                "v00003", "v00004", "v00012"),
                  filter_indicator_functions =
                    c("^com_item_missingness$",
                      "^acc_varcomp$"),
                  filter_result_slots =
                    c("^SummaryTable$"),
                  cores = 1,
                   dimensions = c("invalid"),
      )),
    regexp =
      paste("(?ms)Missing",
            ".+invalid.+from.+Completeness.+Consis.+Accuracy.+Integrity.+did",
            "you mean.+Integrity.+"),
    perl = TRUE
  )

  expect_silent(
    report <-
      suppressMessages(dq_report2(sd0, md0,
                                  resp_vars = c("v00000", "v00001", "v00002",
                                                "v00003", "v00004", "v00012"),
                                  filter_indicator_functions =
                                    c("^com_item_missingness$",
                                      "^acc_varcomp$"),
                                  filter_result_slots =
                                    c("^SummaryTable$"),
                                  cores = 1,
                                 strata_attribute = "GROUP_VAR_XXX",
                                 dimensions = c("Completeness"),
      )
      )
  )

  report <- suppressWarnings(dq_report2(sd0, md0,
                                       cores = 1,
                                       label_col = LABEL,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency",
                                           "Accuracy"),
                                       resp_vars = c("SBP_0", "SEX_0"),
                                       filter_indicator_functions =
                                         c("^com_item_missingness$",
                                           "^acc_distributions_loc$",
                                           "^acc_margins$"),
                                       filter_result_slots =
                                         c("^SummaryTable$"),
                                       specific_args = list(
                                         acc_margins =
                                           list(min_obs_in_subgroup = 40),
                                         acc_distributions_loc =
                                           list(flip_mode = "flip"),
                                         com_item_missingness = list(
                                           label_col = LONG_LABEL
                                         ))
  ))

  expect_equal(
    attr(report$acc_margins_observer.SBP_0, "call")[["min_obs_in_subgroup"]],
    40)

  expect_equal(
    attr(report$acc_distributions_loc.SBP_0, "call")[["flip_mode"]],
    "flip")

  expect_null(
    attr(report$acc_distributions_loc_ecdf_observer.SBP_0,
         "call")[["flip_mode"]])

  expect_equal(
    attr(report$com_item_missingness.SBP_0, "call")[["label_col"]],
    LABEL) # this should not be overwritable

  report <- suppressWarnings(dq_report2(sd0, md0,
                                       resp_vars = c("SBP_0", "SEX_0"),
                                       filter_indicator_functions =
                                         c("^acc_distributions_loc_ecdf$",
                                           "^acc_distributions_loc$"),
                                       cores = 1,
                                       flip_mode = "flip",
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency",
                                           "Accuracy"))
  )

  expect_equal(attr(report$acc_distributions_loc.SEX_0, "call")$resp_vars,
               "SEX_0") # resp_vars cannot be overwritten

  expect_equal(attr(report$acc_distributions_loc.SBP_0, "call")$flip_mode,
               "flip")

  expect_equal(attr(report$acc_distributions_loc_ecdf_observer.SBP_0, "call")[[
    "flip_mode"]], "flip")

})
