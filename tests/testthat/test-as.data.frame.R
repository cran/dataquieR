test_that("as.data.frame.dataquieR_resultset works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

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
    cause_label_df = read.csv(
      system.file("extdata", "Missing-Codes-2020.csv", package = "dataquieR"),
      header = TRUE, sep = ";"
    )
  ))

  x <- as.data.frame(report)

  expect_equal(dim(x), c(7, 6))
  expect_equal(colnames(x),
               c("implementationform", "results",
                 "show_causes", "cause_label_df", "resp_vars", "check_table"))
  expect_equal(x$implementationform,
               c("com_unit_missingness", "com_segment_missingness",
                 "com_item_missingness",  "con_limit_deviations",
                 "con_inadmissible_categorical",
                 "con_contradictions",  "con_detection_limits"))
  expect_equal(vapply(x$results, length, FUN.VALUE = integer(1)),
               c(2L, 2L, 2L, 4L, 3L, 4L, 4L))
  expect_equal(lapply(x$results, names),
               list(c("FlaggedStudyData", "SummaryData"),
                    c("SummaryData", "SummaryPlot" ),
                    c("SummaryTable", "SummaryPlot"),
                    c("FlaggedStudyData", "SummaryTable",
                      "SummaryPlotList", "ModifiedStudyData"),
                    c("SummaryTable", "ModifiedStudyData",  "FlaggedStudyData"),
                    c("FlaggedStudyData", "SummaryTable",
                      "SummaryData",  "SummaryPlot"),
                    c("FlaggedStudyData", "SummaryTable",
                      "SummaryPlotList",  "ModifiedStudyData")))
})
