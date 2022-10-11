test_that("as.list.dataquieR_resultset works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

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
                                       cause_label_df = read.csv(
                                         system.file(
                                           "extdata",
                                           "Missing-Codes-2020.csv",
                                           package = "dataquieR"),
                                         header = TRUE, sep = ";"
                                       )
  ))

  x <- as.list(report)

  expect_equal(length(x), 7)
  expect_equal(names(x), c("com_unit_missingness", "com_segment_missingness",
                           "com_item_missingness",  "con_limit_deviations",
                           "con_inadmissible_categorical", "con_contradictions",
                           "con_detection_limits"))
  expect_type(x, "list")
  expect_false(is.data.frame(x))

  report2 <- report
  report2$long_format$com_unit_missingness <- 42
  y <- as.list(report2)
  expect_equal(y$com_unit_missingness, 42)
})
