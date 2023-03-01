test_that("as.list.dataquieR_resultset works", {
  skip_on_cran() # deprecated
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  report <- suppressWarnings(dq_report(study_data, meta_data,
                                       label_col = LABEL,
                                       cores = 1,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency"),
                                       check_table =
                                         read.csv(system.file(
                                           "extdata",
                                           "contradiction_checks.csv",
                                           package = "dataquieR"
                                       ), header = TRUE, sep = "#"),
                                       show_causes = TRUE,
                                       cause_label_df = prep_get_data_frame(
                                         "meta_data_v2|missing_table")
  ))

  x <- as.list(report)

  expect_equal(names(x),
               c("int_all_datastructure_dataframe",
                 "int_all_datastructure_segment", "int_datatype_matrix",
                 "com_unit_missingness", "com_segment_missingness",
                 "com_item_missingness",  "con_hard_limits",
                 "con_soft_limits", "con_detection_limits",
                 "con_inadmissible_categorical",
                 "con_contradictions", "con_contradictions_redcap"))
  expect_type(x, "list")
  expect_false(is.data.frame(x))

  report2 <- report
  report2$long_format$com_unit_missingness <- 42
  y <- as.list(report2)
  expect_equal(y$com_unit_missingness, 42)
})
