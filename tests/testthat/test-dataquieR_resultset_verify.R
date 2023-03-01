test_that("dataquieR_resultset_verify works", {
  skip_on_cran() # deprecated
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  report <- suppressWarnings(dq_report(study_data, meta_data,
                                       cores = 1,
                                       label_col = LABEL,
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
  expect_silent(dataquieR:::dataquieR_resultset_verify(report))
  expect_error(dataquieR:::dataquieR_resultset_verify("Nothing"),
               regexp =
                 paste("Tried to bless something else than a list in",
                       "class .+dataquieR_resultset.+. Internal error."),
               perl = TRUE
  )

  a <- report
  a[["long_format"]] <- NULL
  b <- report
  b[["app_mat"]] <- NULL
  c <- report
  c[["long_format"]] <- NULL
  c[["app_mat"]] <- NULL

  expect_error(dataquieR:::dataquieR_resultset_verify(a),
               regexp =
                 paste("Not a list .+long_format.+. Internal error."),
               perl = TRUE
  )
  a[["long_format"]] <- 1:10
  expect_error(dataquieR:::dataquieR_resultset_verify(a),
               regexp =
                 paste("Not a list .+long_format.+. Internal error."),
               perl = TRUE
  )
  b[["app_mat."]] <- 1:10
  expect_error(dataquieR:::dataquieR_resultset_verify(b),
               regexp =
                 paste("Not a list .+app_mat.+. Internal error."),
               perl = TRUE
  )
  expect_error(dataquieR:::dataquieR_resultset_verify(c),
               regexp =
                 paste("Not a list .+long_format.+. Internal error."),
               perl = TRUE
  )
  d <- report
  d$long_format$com_unit_missingness <- 42
  expect_error(dataquieR:::dataquieR_resultset_verify(d),
               regexp =
                 paste("Some outputs are neither a data frame nor",
                       "an empty list",
                       ".+long_format.+:.+com_unit_missingness.+.",
                       "Internal error."),
               perl = TRUE
  )
  d$long_format$com_segment_missingness <- 42
  expect_error(dataquieR:::dataquieR_resultset_verify(d),
               regexp =
                 paste("Some outputs are neither a data frame nor",
                       "an empty list",
                       ".+com_unit_missingness.+.com_segment_missingness.+",
                       "Internal error."),
               perl = TRUE
  )
  e <- report
  e$app_mat$SummaryTable <- NULL
  expect_error(dataquieR:::dataquieR_resultset_verify(e),
               regexp =
                 paste("Not a data frame in .+this.app_mat.SummaryTable.+.",
                       "Internal error."),
               perl = TRUE
  )
  e$app_mat$SummaryTable <- 42
  expect_error(dataquieR:::dataquieR_resultset_verify(e),
               regexp =
                 paste("Not a data frame in .+this.app_mat.SummaryTable.+.",
                       "Internal error."),
               perl = TRUE
  )
  f <- report
  f$study_data <- NULL
  expect_error(dataquieR:::dataquieR_resultset_verify(f),
               regexp =
                 paste("Not a data frame .+study_data.+",
                       "Internal error."),
               perl = TRUE
  )
  f$study_data <- 42
  expect_error(dataquieR:::dataquieR_resultset_verify(f),
               regexp =
                 paste("Not a data frame .+study_data.+",
                       "Internal error."),
               perl = TRUE
  )
  f <- report
  f$meta_data <- 42
  expect_error(dataquieR:::dataquieR_resultset_verify(f),
               regexp =
                 paste("Not a data frame .+meta_data.+",
                       "Internal error."),
               perl = TRUE
  )
  g <- report
  g$strata_attribute <- "xxx"
  expect_error(dataquieR:::dataquieR_resultset_verify(g),
               regexp =
                 paste("Not a supported .+strata_attribute.+: .+xxx.+.",
                       "Internal error."),
               perl = TRUE
  )
  h <- report
  h$strata_attribute <- "STUDY_SEGMENT"
  expect_silent(dataquieR:::dataquieR_resultset_verify(h))
  i <- report
  i$strata_attribute <- NA
  expect_silent(dataquieR:::dataquieR_resultset_verify(i))
  j <- report
  j$strata_vars <- NULL
  expect_silent(dataquieR:::dataquieR_resultset_verify(j))
  k <- report
  k$strata_vars <- c("SEX_0", "AGE_0")
  expect_silent(dataquieR:::dataquieR_resultset_verify(k))
  l <- report
  l$strata_vars <- c("SEX_0", "AGE_0x", "y")
  expect_error(dataquieR:::dataquieR_resultset_verify(l),
                regexp =
                  paste("All .+strata_vars.+ should name variables:",
                        ".+AGE_0x.+, .+y.+ don't. Internal error."),
                perl = TRUE
  )
  m <- report
  m$label_col <- letters[1:2]
  expect_error(dataquieR:::dataquieR_resultset_verify(m),
               regexp =
                 paste("Invalid .label_col. set."),
               perl = TRUE
  )
  m$label_col <- 1
  expect_error(dataquieR:::dataquieR_resultset_verify(m),
               regexp =
                 paste("Invalid .label_col. set."),
               perl = TRUE
  )
  m$label_col <- "xxx"
  expect_error(dataquieR:::dataquieR_resultset_verify(m),
               regexp =
                 paste("Invalid .label_col. set."),
               perl = TRUE
  )

})
