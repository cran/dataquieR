test_that("dq_report works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  report <- suppressWarnings(dq_report(sd0, md0,
                                       cores = 1,
                                       label_col = LABEL,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency",
                                           "Accuracy"),
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
  expect_equal(sum(report$long_format$com_item_missingness$results[[
    1]]$SummaryTable$GRADING, na.rm = TRUE), 1)

  expect_silent(summary(report))

  r <- report
  r$long_format$acc_varcomp$results[[1]]$SummaryTable <- NULL
  expect_silent(summary(r))

  r <- report
  r$long_format$acc_varcomp$results[[1]]$SummaryTable <-
    r$long_format$acc_varcomp$results[[1]]$SummaryTable[FALSE, , FALSE]
  expect_silent(summary(r))

  expect_error(
    report <-
      suppressWarnings(dq_report(sd0, md0,
                                 cores = 1,
                                 label_col = LABEL,
                                 dimensions = 42,
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
                                 )
                       ),
                       regexp =
                         ".+dimensions.+ needs to be a character vector",
                       perl = TRUE
    )


  expect_error(
    report <-
      suppressWarnings(dq_report(sd0, md0,
                                 cores = 1,
                                 label_col = LABEL,
                                 dimensions = c("invalid"),
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
      )
      ),
    regexp =
      paste(
        "(?ms).+dimensions.+ need to",
        "be in .+Completeness.+Consistency.+Accuracy+"
        ),
    perl = TRUE
  )

  expect_error(
    report <-
      suppressWarnings(dq_report(sd0, md0,
                                 cores = 1,
                                 label_col = LABEL,
                                 strata_attribute = "KEY_XXX",
                                 dimensions = c("Completeness"),
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
      )
      ),
    regexp =
      paste(
        "(?ms).segment attributes other than",
        "KEY_STUDY_SEGMENT are unsupported yet"
      ),
    perl = TRUE
  )

  expect_error(
    report <-
      suppressWarnings(dq_report(sd0, md0,
                                 cores = 1,
                                 label_col = LABEL,
                                 strata_vars = NA,
                                 dimensions = c("Completeness"),
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
      )
      ),
    regexp =
      paste(
        "(?ms)The report generated by dq_report.+ cannot be stratified yet."
      ),
    perl = TRUE
  )

  md0$KEY_STUDY_SEGMENT <- NULL
  report <- NULL
  report <-
    suppressWarnings(dq_report(sd0, md0,
                               cores = 1,
                               label_col = LABEL,
                               dimensions = c("Completeness"),
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
    )
    )

  sd0 <- study_data[, 1:5]
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  report <- suppressWarnings(dq_report(sd0, md0,
                                       cores = 1,
                                       label_col = LABEL,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency",
                                           "Accuracy"),
                                       specific_args = list(
                                        acc_margins =
                                          list(min_obs_in_subgroup = 40),
                                        com_item_missingness = list(
                                          show_causes = TRUE,
                                          cause_label_df = read.csv(
                                            system.file(
                                              "extdata",
                                              "Missing-Codes-2020.csv",
                                              package = "dataquieR"),
                                            header = TRUE, sep = ";"
                                          )
                                        )
                                       )
  ))

})
