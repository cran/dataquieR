test_that("acc_univariate_outlier works without label_col", {
  skip_on_cran() # slow, errors obvious
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]

  sd1 <- study_data # no outliers at all
  set.seed(234325)
  sd1$v00014 <- rnorm(nrow(sd1), sd = 0.01)
  sd1 <- sd1[sd1$v00014 < 0.02 & sd1$v00014 > -0.02, ]
  expect_message(
    no_ol_det <- acc_univariate_outlier(resp_vars = "v00014", study_data = sd1,
                           meta_data = meta_data, max_non_outliers_plot = 0),
    regexp =
      paste("For .+v00014.+, 0 from 1466 non-outlier data values",
      "were sampled to avoid large plots."))

  expect_match(
    ggplot2::ggplot_build(no_ol_det$SummaryPlotList$v00014)$data[[1]]$label,
    "No outliers detected for .+v00014.+",
    perl = TRUE
  )

  expect_message(invisible(
    acc_univariate_outlier(resp_vars = "v00014", study_data = study_data,
                           meta_data = meta_data, n_rules = cars)),
    regexp =
      "The formal n_rules is not an integer between 1 and 4, default .4. is used.")

  expect_message(invisible(
    acc_univariate_outlier(resp_vars = "v00014", study_data = study_data,
                           meta_data = meta_data,
                           max_non_outliers_plot = 100)),
    regexp =
      paste("For .+v00014.+, 100 from 2633 non-outlier data values",
            "were sampled to avoid large plots."))

  expect_message(invisible(
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

  expect_message(
    expect_error(invisible(
      acc_univariate_outlier(study_data = sd1,
                             meta_data = md1)),
      regexp =
        paste("No variables with suitable data type defined.")),
    regexp = "The following variables:  were selected.",
    all = !TRUE,
    fixed = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE <- NULL
  expect_error(invisible(
    acc_univariate_outlier(study_data = study_data,
                           meta_data = md0, n_rules = 4)),
    regexp = "Missing columns .+DATA_TYPE.+ from .+meta_data.+"
    )

  md0 <- meta_data
  md0$DATA_TYPE[[1]] <- NA
  expect_message(
    expect_warning(
      invisible(
        acc_univariate_outlier(study_data = study_data,
                             meta_data = md0, n_rules = 4))
      ,
      regexp = sprintf("(%s)",
       paste(
         "For the variables.+?v00000.+?I have no valid.+?DATA_TYPE.+?in the",
         ".+?meta_data.+?I.+?ve predicted the.+?DATA_TYPE.+?from",
         "the.+?study_data.+?yielding.+?v00000 = integer.+?"
        )
      ),
      all = TRUE,
      perl = TRUE
    ),
    perl = TRUE,
    regexp = sprintf("(?ms).*(%s|%s|%s).*", paste("The following variables: v00000, v00002, v00003,",
    "v01003, v01002, v10000, v00004, v00005, v00006, v00007, v00009, v00109,",
    "v00010, v20000, v00014, v00015, v00016, v30000, v00018, v01018, v00019,",
    "v00020, v00021, v00022, v00023, v00024, v00025, v00026, v00027, v00028,",
    "v00029, v00030, v00031, v40000, v00034, v00035, v00036, v00037, v00038,",
    "v00039, v00040, v00041, v50000.*"),
    paste("Variables:.+?v00000.+?,.+?v00002.+?,.+?v00003.+?,.+?v01003.+?,.+?v01002.+?,.+?v10000.+?,.+?v00004.+?,.+?v00005.+?,.+?v00007.+?,.+?v00009.+?,.+?v00109.+?,.+?v00010.+?,.+?v20000.+?,.+?v00015.+?,.+?v00016.+?,.+?v30000.+?,.+?v00018.+?,.+?v01018.+?,.+?v00019.+?,.+?v00020.+?,.+?v00021.+?,.+?v00022.+?,.+?v00023.+?,.+?v00024.+?,.+?v00025.+?,.+?v00026.+?,.+?v00027.+?,.+?v00028.+?,.+?v00029.+?,.+?v00030.+?,.+?v00031.+?,.+?v40000.+?,.+?v00034.+?,.+?v00035.+?,.+?v00036.+?,.+?v00037.+?,.+?v00038.+?,.+?v00039.+?,.+?v00040.+?,.+?v00041.+?,.+?v50000.+?show integer values only, but will be nonetheless considered."),
    paste("The variables.+?v00000.+?,.+?v00002.+?,.+?v01002.+?,.+?v10000.+?,.+?v00007.+?,.+?v00109.+?,.+?v00010.+?,.+?v20000.+?,.+?v30000.+?,.+?v00018.+?,.+?v01018.+?,.+?v00019.+?,.+?v00020.+?,.+?v00022.+?,.+?v00023.+?,.+?v00024.+?,.+?v00025.+?,.+?v00028.+?,.+?v00029.+?,.+?v00030.+?,.+?v40000.+?,.+?v50000.+? are neither float nor integer without VALUE_LABELS. Ignoring those.*")
    )
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
      na.rm = TRUE) -2699 - 112.45)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryPlotList$v00014$data)),
      na.rm = TRUE) - 8005.172)), 0
  )

  expect_identical(colnames(res1$SummaryTable),
                   c("Variables", "Mean", "No.records", "SD", "Median", "Skewness",
                     "Tukey (N)",  "3SD (N)", "Hubert (N)",
                     "Sigma-gap (N)", "NUM_acc_ud_outlu",  "Outliers, low (N)",
                     "Outliers, high (N)", "GRADING", "PCT_acc_ud_outlu"))
})

test_that("acc_univariate_outlier works with label_col", {
  skip_on_cran() # slow, errors obvious
  skip_if_translated()
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]

  md0 <- meta_data
  md0$DATA_TYPE <- NA
  expect_warning(
    expect_message(
      res1 <-
        acc_univariate_outlier(resp_vars = c("USR_SOCDEM_0", "CRP_0"),
                               study_data = study_data,
                               meta_data = md0, label_col = LABEL),
      regexp = sprintf("(%s|%s)",
                       paste(
                         "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+string.+does not have an allowed type.+integer.+float.+"),
                       paste(
                         "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+")
      ),
      all = TRUE,
      perl = TRUE
    ),
    perl = TRUE,
    regexp = sprintf("(%s|%s)",
                     paste(
                       "I've predicted the.+DATA_TYPE.+from the.+study_data"
                     ),
                     paste(
                       "In.+resp_vars.+, variables.+USR_SOCDEM_0.+were excluded."
                      )
                    )
  )
  expect_warning(
    expect_message(
      res1 <-
        acc_univariate_outlier(resp_vars = c("USR_SOCDEM_0", "CRP_0"),
                               study_data = study_data,
                               meta_data = md0, label_col = LABEL,
                               exclude_roles = "XXX"),
      regexp = sprintf("(%s|%s|%s)",
                       paste(
                         "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+string.+does not have an allowed type.+integer.+float.+"),
                       paste(
                         "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+"),
                       paste("Specified VARIABLE_ROLE not in meta_data.",
                             "No exclusion applied.")
      ),
      all = TRUE,
      perl = TRUE
    ),
    regexp = sprintf("(%s)",
                     paste(".*predicted.+DATA_TYPE.*")
    ),
    perl = TRUE
  )

  expect_message(
    expect_warning(
      res1 <-
        acc_univariate_outlier(
          resp_vars = c("USR_SOCDEM_0", "CRP_0",
                        "CENTER_0", "SEX_0", "AGE_0"),
          study_data = study_data,
          meta_data = md0,
          label_col = LABEL,
          exclude_roles = c("intro", "process")
        ),
      regexp = sprintf("(%s|%s|%s)",
                       paste("variables.+SEX_0.+USR_SOCDEM_0.+CENTER_0.+were",
                             "excluded."),
                       paste("variables.+USR_SOCDEM_0.+were excluded."),
                       paste("For the variables",
                             ".*predicted.+DATA_TYPE.*from the .study_data.")
      ),
      all = TRUE,
      perl = TRUE
    ),
    regexp = sprintf("(%s|%s|%s|%s|%s)",
                     paste(
                       "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+string.+does not have an allowed type.+integer.+float.+"
                     ),
                     paste(
                       "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+"
                     ),
                     paste(
                       "Argument.+resp_vars.+Variable.+SEX_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+"
                     ),
                     paste(
                       "Argument.+resp_vars.+Variable.+CENTER_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+"
                     ),
                     paste("Study variables.+AGE_0.+have",
                           "been excluded.")
    ),
    all = TRUE,
    perl = TRUE
  )

  sd0 <- study_data
  sd0$v00014 <- NA_integer_
  expect_message(
#    expect_warning(
      expect_error(
        res1 <-
          acc_univariate_outlier(resp_vars = c("CRP_0", "AGE_0"),
                                 study_data = sd0,
                                 meta_data = meta_data, label_col = LABEL,
                                 exclude_roles = c("intro", "process")),
        regexp = "No data left, aborting",
        perl = TRUE
      ),
    #   regexp = "In .resp_vars.+variables .CRP_0. were excluded",
    #   perl = TRUE
    # ),
    regexp = sprintf("(%s|%s|%s)",
                     paste(
                       "Argument.+resp_vars.+Variable.+CRP_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+"
                     ),
                     paste("Study variables.+AGE_0.+have",
                           "been excluded."),
                     paste("Variables: .+CRP_0.+show integer values only,",
                           "but will be nonetheless considered.")
    ),
    perl = TRUE,
    all = TRUE
  )


  sd0 <- study_data
  sd0$v00014 <- NA
  expect_message(
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
  expect_identical(res1$SummaryTable$GRADING, 1)

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
      na.rm = TRUE) -2699 - 112.45)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryPlotList$CRP_0$data)),
      na.rm = TRUE) - 8005.172)), 0
  )

  expect_identical(colnames(res1$SummaryTable),
                   c("Variables", "Mean", "No.records", "SD", "Median", "Skewness",
                     "Tukey (N)",  "3SD (N)", "Hubert (N)",
                     "Sigma-gap (N)", "NUM_acc_ud_outlu",  "Outliers, low (N)",
                     "Outliers, high (N)", "GRADING", "PCT_acc_ud_outlu"))

})
