test_that("as.data.frame.dataquieR_resultset works", {
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
  report <- suppressWarnings(dq_report(study_data, meta_data, cores = 1,
    label_col = LABEL, dimensions =
      c("Completeness", "Consistency"), # for sake of speed, omit Accuracy here
    check_table = read.csv(system.file("extdata",
      "contradiction_checks.csv",
      package = "dataquieR"
    ), header = TRUE, sep = "#"),
    show_causes = TRUE,
    cause_label_df = prep_get_data_frame("meta_data_v2|missing_table")
  ))

  x <- as.data.frame(report)

  # x$meta_data_dataframe <- NULL
  # x$meta_data_segment <- NULL

  expect_equal(dim(x), c(12, 7)) # NOTE: This must be changed, if more indicators are added to dq_report
  expect_equal(
    colnames(x),
    c(
      "implementationform",
      "results",
      "resp_vars",
      "show_causes",
      "cause_label_df",
      "include_sysmiss",
      "check_table"
    )
  )
  expect_equal(
    x$implementationform,
    c(
      "int_all_datastructure_dataframe",
      "int_all_datastructure_segment",
      "int_datatype_matrix",
      "com_unit_missingness",
      "com_segment_missingness",
      "com_item_missingness",
      "con_hard_limits",
      "con_soft_limits",
      "con_detection_limits",
      "con_inadmissible_categorical",
      "con_contradictions",
      "con_contradictions_redcap"
    )
  )
  expect_equal(vapply(x$results, length, FUN.VALUE = integer(1)),
               c(1L, 1L, 5L, 2L, 3L, 4L, 6L, 6L, 6L, 4L, 4L, 1L))
  expect_equal(lapply(setNames(x$results, nm = x$implementationform), names),
               list(
                 int_all_datastructure_dataframe = c(),
                 int_all_datastructure_segment = c(),
                 int_datatype_matrix = c(
                   "SummaryPlot",
                   "DataTypePlotList",
                   "SummaryTable",
                   "SummaryData",
                   "ReportSummaryTable"
                 ),
                 com_unit_missingness = c("FlaggedStudyData", "SummaryData"),
                 com_segment_missingness =
                   c("SummaryData", "ReportSummaryTable", "SummaryPlot"),
                 com_item_missingness = c(
                   "SummaryTable",
                   "SummaryData",
                   "SummaryPlot",
                   "ReportSummaryTable"
                 ),
                 con_hard_limits = c(
                   "FlaggedStudyData",
                   "SummaryTable",
                   "SummaryData",
                   "ReportSummaryTable",
                   "SummaryPlotList",
                   "ModifiedStudyData"
                 ),
                 con_soft_limits = c(
                   "FlaggedStudyData",
                   "SummaryTable",
                   "SummaryData",
                   "ReportSummaryTable",
                   "SummaryPlotList",
                   "ModifiedStudyData"
                 ),
                 con_detection_limits = c(
                   "FlaggedStudyData",
                   "SummaryTable",
                   "SummaryData",
                   "ReportSummaryTable",
                   "SummaryPlotList",
                   "ModifiedStudyData"
                 ),
                 con_inadmissible_categorical =
                   c("SummaryData",
                     "SummaryTable", "ModifiedStudyData",  "FlaggedStudyData"),
                 con_contradictions = c(
                   "FlaggedStudyData",
                   "SummaryTable",
                   "SummaryData",
                   "SummaryPlot"
                 ),
                 con_contradictions_redcap = c()
               ))
})
