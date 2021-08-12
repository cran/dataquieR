test_that("acc_univariate_outlier works without label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  sd1 <- study_data # no outliers at all
  set.seed(234325)
  sd1$v00014 <- rnorm(nrow(sd1), sd = 0.01)
  sd1 <- sd1[sd1$v00014 < 0.02 & sd1$v00014 > -0.02, ]
  expect_warning(
    no_ol_det <- acc_univariate_outlier(resp_vars = "v00014", study_data = sd1,
                           meta_data = meta_data, max_non_outliers_plot = 0),
    regexp =
      paste("For .+v00014.+, 0 from 2851 non-outlier data values",
      "were sampled to avoid large plots."))

  expect_match(
    ggplot2::ggplot_build(no_ol_det$SummaryPlotList$v00014)$data[[1]]$label,
    "No outliers detected for .+v00014.+",
    perl = TRUE
  )

  expect_warning(invisible(
    acc_univariate_outlier(resp_vars = "v00014", study_data = study_data,
                           meta_data = meta_data, n_rules = cars)),
    regexp =
      "The formal n_rules is not an integer of 1 to 4, default .4. is used.")

  expect_warning(invisible(
    acc_univariate_outlier(resp_vars = "v00014", study_data = study_data,
                           meta_data = meta_data,
                           max_non_outliers_plot = 100)),
    regexp =
      paste("For .+v00014.+, 100 from 2633 non-outlier data values",
            "were sampled to avoid large plots."))

  expect_warning(invisible(
    acc_univariate_outlier(resp_vars = "v00014", study_data = study_data,
                           meta_data = meta_data,
                           max_non_outliers_plot = -42)),
    regexp =
      paste("The formal max_non_outliers_plot is not an",
            "integer >= 0, default .10000. is used."))

  sd1 <- study_data[, c("v00103", "v00001", "v00013")]
  md1 <- subset(meta_data, VAR_NAMES %in% colnames(sd1))
  md1$JUMP_LIST <- "|"
  md1$MISSING_LIST <- "|"

  expect_warning(
    expect_error(invisible(
      acc_univariate_outlier(study_data = sd1,
                             meta_data = md1)),
      regexp =
        paste("No variables suitable data type defined.")),
    regexp = "The following variables:  were selected.",
    all = TRUE,
    fixed = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE <- NULL
  expect_warning(invisible(
    acc_univariate_outlier(study_data = study_data,
                           meta_data = md0, n_rules = 4)),
    regexp = sprintf("(%s|%s|%s|%s)",
      paste(
        "No .+DATA_TYPE.+ for all or some variables defined in the metadata.",
        "I guessed them based on data"),
      paste("The following variables: v00000, v00002, v00003, v01003, v01002,",
          "v10000, v00004, v00005, v00006, v00007, v00009, v00109, v00010,.+",
          "v50000 were selected."),
      paste("Variables: .+v00000.+, .+v00002.+, .+v50000.+",
            "show integer values only, but will be nonetheless considered."),
      paste("The variables .+v00000.+v00002.+v01002.+v10000.+v00007.+,",
            ".+v00109.+v00010.+v20000.+v30000.+v00018.+v01018.+v00019.+,",
            ".+v00020.+v00022.+v00023.+v00024.+v00025.+v00028.+v00029.+,",
            ".+v00030.+v40000.+v50000.+ are neither float nor integer",
            "without VALUE_LABELS. Ignoring those")
    ),
    all = TRUE,
    perl = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE[[1]] <- NA
  expect_warning(
    invisible(
      acc_univariate_outlier(study_data = study_data,
                           meta_data = md0, n_rules = 4))
    ,
    regexp = sprintf("(%s|%s|%s|%s)",
     paste(
       "No .+DATA_TYPE.+ for all or some variables defined in the metadata.",
       "I guessed them based on data"),
     paste("The following variables: v00000, v00002, v00003, v01003, v01002,",
           "v10000, v00004, v00005, v00006, v00007, v00009, v00109, v00010,.+",
           "v50000 were selected."),
     paste("Variables: .+v00000.+, .+v00002.+, .+v50000.+",
           "show integer values only, but will be nonetheless considered."),
     paste("The variables .+v00000.+v00002.+v01002.+v10000.+v00007.+,",
           ".+v00109.+v00010.+v20000.+v30000.+v00018.+v01018.+v00019.+,",
           ".+v00020.+v00022.+v00023.+v00024.+v00025.+v00028.+v00029.+,",
           ".+v00030.+v40000.+v50000.+ are neither float nor integer",
           "without VALUE_LABELS. Ignoring those")
    ),
    all = TRUE,
    perl = TRUE
  )

  res1 <-
    acc_univariate_outlier(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data)

  expect_true(all(
    c("SummaryTable",
      "SummaryPlotList"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 112.45)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryPlotList$v00014$data)),
      na.rm = TRUE) - 8005.172)), 0
  )

  expect_identical(colnames(res1$SummaryTable),
                   c("Variables", "Mean", "SD", "Median", "Skewness",
                     "Tukey (N)",  "6-Sigma (N)", "Hubert (N)",
                     "Sigma-gap (N)", "Most likely (N)",  "To low (N)",
                     "To high (N)", "Grading"))
})

test_that("acc_univariate_outlier works with label_col", {
  skip_if_translated()
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  md0 <- meta_data
  md0$DATA_TYPE <- NULL
  expect_warning(
    res1 <-
      acc_univariate_outlier(resp_vars = c("USR_SOCDEM_0", "CRP_0"),
                             study_data = study_data,
                             meta_data = md0, label_col = LABEL),
    regexp = sprintf("(%s|%s|%s)",
                     paste(
                       "No .+DATA_TYPE.+ for all or some variables defined",
                       "in the metadata.",
                       "I guessed them based on data"),
                     paste("In .+resp_vars.+, variables",
                           "with types matching .+integer . float.+ should",
                           "be specified, but not all variables have a type",
                           "assigned in the meta data. I have 2 variables but",
                           "only 0 types."),
                     paste("Only: CRP_0 are defined to be of type float or",
                           "integer.")
    ),
    all = TRUE,
    perl = TRUE
  )
  expect_warning(
    res1 <-
      acc_univariate_outlier(resp_vars = c("USR_SOCDEM_0", "CRP_0"),
                             study_data = study_data,
                             meta_data = md0, label_col = LABEL,
                             exclude_roles = "XXX"),
    regexp = sprintf("(%s|%s|%s|%s)",
                     paste(
                       "No .+DATA_TYPE.+ for all or some variables defined",
                       "in the metadata.",
                       "I guessed them based on data"),
                     paste("In .+resp_vars.+, variables",
                           "with types matching .+integer . float.+ should",
                           "be specified, but not all variables have a type",
                           "assigned in the meta data. I have 2 variables but",
                           "only 0 types."),
                     paste("Only: CRP_0 are defined to be of type float or",
                           "integer."),
                     paste("Specified VARIABLE_ROLE not in meta_data.",
                           "No exclusion applied.")
    ),
    all = TRUE,
    perl = TRUE
  )

  expect_warning(
    res1 <-
      acc_univariate_outlier(resp_vars = c("USR_SOCDEM_0", "CRP_0",
                                           "CENTER_0", "SEX_0", "AGE_0"),
                             study_data = study_data,
                             meta_data = md0, label_col = LABEL,
                             exclude_roles = c("intro", "process")),
    regexp = sprintf("(%s|%s|%s|%s)",
                     paste(
                       "No .+DATA_TYPE.+ for all or some variables defined",
                       "in the metadata.",
                       "I guessed them based on data"),
                     paste("In .+resp_vars.+, variables",
                           "with types matching .+integer . float.+ should",
                           "be specified, but not all variables have a type",
                           "assigned in the meta data. I have 5 variables but",
                           "only 0 types."),
                     paste("Only: CENTER_0, SEX_0, AGE_0, CRP_0 are defined",
                           "to be of type float or integer."),
                     paste("Study variables: .+CENTER_0.+, .+SEX_0.+,",
                           ".+AGE_0.+ have been excluded.")
    ),
    all = TRUE,
    perl = TRUE
  )

  sd0 <- study_data
  sd0$v00014 <- NA_integer_
  expect_warning(
    expect_error(
      res1 <-
        acc_univariate_outlier(resp_vars = c("CRP_0", "AGE_0"),
                               study_data = sd0,
                               meta_data = meta_data, label_col = LABEL,
                               exclude_roles = c("intro", "process")),
      regexp = "No data left, aborting.",
      perl = TRUE
    ),
    regexp = sprintf("(%s|%s)",
                     paste(".+CRP_0.+ show integer values only,",
                           "but will be nonetheless considered."),
                     paste("Study variables:",
                           ".+AGE_0.+ have been excluded.")
    ),
    all = TRUE,
    perl = TRUE
  )

  sd0 <- study_data
  sd0$v00014 <- NA
  expect_warning(
    expect_error(
      res1 <-
        acc_univariate_outlier(resp_vars = c("CRP_0", "AGE_0"),
                               study_data = sd0,
                               meta_data = meta_data, label_col = LABEL,
                               exclude_roles = c("intro", "process")),
      regexp = "No suitable response variables left.",
      perl = TRUE
    ),
    regexp = sprintf("(%s|%s)",
                     paste("Variables .+CRP_0.+ are not of type float or",
                           "integer and will be removed from univariate",
                           "outlier analysis."),
                     paste("Study variables:",
                           ".+AGE_0.+ have been excluded.")
    ),
    all = TRUE,
    perl = TRUE
  )

  res1 <-
    acc_univariate_outlier(resp_vars = "CRP_0", study_data = study_data,
                           meta_data = meta_data, label_col = LABEL,
                           n_rules = 1)
  expect_identical(res1$SummaryTable$Grading, 1)

  res1 <-
    acc_univariate_outlier(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, label_col = LABEL)

  expect_true(all(
    c("SummaryTable",
      "SummaryPlotList"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 112.45)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryPlotList$CRP_0$data)),
      na.rm = TRUE) - 8005.172)), 0
  )

  expect_identical(colnames(res1$SummaryTable),
                   c("Variables", "Mean", "SD", "Median", "Skewness",
                     "Tukey (N)",  "6-Sigma (N)", "Hubert (N)",
                     "Sigma-gap (N)", "Most likely (N)",  "To low (N)",
                     "To high (N)", "Grading"))

})
