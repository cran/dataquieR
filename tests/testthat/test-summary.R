test_that("summary.dataquieR_resultset works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  report <- suppressWarnings(dq_report(study_data, meta_data,
                                       cores = 1,
                                       label_col = LABEL, dimensions =
                                     c( # for sake of speed, omit Accuracy here
                                           "Completeness",
                                           "Consistency"),
                                       check_table =
                                         read.csv(system.file("extdata",
                                                     "contradiction_checks.csv",
                                                      package = "dataquieR"
                                       ), header = TRUE, sep = "#"),
                                       show_causes = TRUE,
                                       cause_label_df = read.csv(
                                         system.file("extdata",
                                                     "Missing-Codes-2020.csv",
                                                     package = "dataquieR"),
                                         header = TRUE, sep = ";"
                                       )
  ))

  x <- summary(report)

  expect_equal(dim(x), c(53, 8))
  expect_equal(
    colnames(x),
    c(
      "StudyVariable",
      "com_item_missingness",
      "con_contradictions",
      "con_detection_limits",
      "con_inadmissible_categorical",
      "con_limit_deviations",
      "KEY_STUDY_SEGMENT",
      "AnyProblem"
    )
  )
  expect_equal(x$AnyProblem,
               c(
                 FALSE,
                 TRUE,
                 FALSE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 FALSE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 FALSE,
                 TRUE,
                 TRUE,
                 TRUE,
                 FALSE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 FALSE,
                 FALSE,
                 FALSE,
                 FALSE,
                 FALSE,
                 TRUE,
                 FALSE,
                 FALSE,
                 TRUE,
                 FALSE,
                 TRUE,
                 TRUE,
                 TRUE,
                 FALSE,
                 TRUE,
                 FALSE,
                 TRUE
               ))
  expect_true(is.data.frame(x))
})

test_that("empty report summary works", {
  empty_report <- as.data.frame.dataquieR_resultset(list(long_format = list()))
  class(empty_report) <- "dataquieR_resultset"
  expect_warning(sr <- summary(empty_report),
                 regexp = paste("No summary available for this report. None",
                           "of the called implementation forms returned any",
                           "GRADING column."))
  expect_equal(nrow(sr), 0)
})

test_that("summary for more than one SummaryTable per indicator", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"),
       envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"),
       envir =
         environment())
  check_table <- read.csv(
    system.file("extdata",
                "contradiction_checks.csv",
                package = "dataquieR"),
    header = TRUE,
    sep = "#"
  )
  check_table[1, "tag"] <- "Logical"
  check_table[1, "Label"] <- "Becomes younger"
  check_table[2, "tag"] <- "Empirical"
  check_table[2, "Label"] <- "sex transformation"
  check_table[3, "tag"] <- "Empirical"
  check_table[3, "Label"] <- "looses academic degree"
  check_table[4, "tag"] <- "Logical"
  check_table[4, "Label"] <- "vegetarian eats meat"
  check_table[5, "tag"] <- "Logical"
  check_table[5, "Label"] <- "vegan eats meat"
  check_table[6, "tag"] <- "Empirical"
  check_table[6, "Label"] <- "non-veg* eats meat"
  check_table[7, "tag"] <- "Empirical"
  check_table[7, "Label"] <- "Non-smoker buys cigarettes"
  check_table[8, "tag"] <- "Empirical"
  check_table[8, "Label"] <- "Smoker always scrounges"
  check_table[9, "tag"] <- "Logical"
  check_table[9, "Label"] <- "Cuff didn't fit arm"
  check_table[10, "tag"] <- "Empirical"
  check_table[10, "Label"] <- "Very mature pregnant woman"
  suppressWarnings(
    report1 <-
      dq_report(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL,
        summarize_categories = TRUE,
        check_table = check_table,
        dimensions = "Consistency",
        cores = 1
      )
  )
  s1 <- summary(report1)
  suppressWarnings(
    report2 <-
      dq_report(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL,
        summarize_categories = FALSE,
        check_table = check_table,
        dimensions = "Consistency",
        cores = 1
      )
  )
  s2 <- summary(report2)
  expect_equal(s1$con_contradictions,
               s2$con_contradictions,
               label = paste("Contradictions with tag cause more than",
               "one SummaryTable, verify its consistency"))
})
