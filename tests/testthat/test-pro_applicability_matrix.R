test_that("pro_applicability_matrix works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  for (max_vars_per_plot in list(
    1:10, -1, -Inf, NA, NaN, complex(real = 1), "A", letters
  )) {
    expect_error(
      appmatrix <- pro_applicability_matrix(study_data = study_data,
                                            meta_data = meta_data,
                                            label_col = LABEL,
                                            max_vars_per_plot =
                                              max_vars_per_plot),
      regexp =
        paste("max_vars_per_plot must be one strictly positive",
              "non-complex integer value, may be Inf."),
      perl = TRUE)
  }

  for (max_vars_per_plot in list(
    20, Inf
  )) {
    expect_silent(
      appmatrix <- pro_applicability_matrix(study_data = study_data,
                                            meta_data = meta_data,
                                            label_col = LABEL,
                                            max_vars_per_plot =
                                              max_vars_per_plot))
  }

  md0 <- meta_data
  md0$DATA_TYPE <- NULL
  expect_error(
    appmatrix <- pro_applicability_matrix(study_data = study_data,
                                          meta_data = md0,
                                          label_col = LABEL,
                                          max_vars_per_plot =
                                            max_vars_per_plot),
    regexp =
      paste("The attribute DATA_TYPE is not contained in the metadata",
            "but is required for this function."),
    perl = TRUE)

  md0 <- meta_data
  md0$DATA_TYPE[[2]] <- NA
  expect_error(
    appmatrix <- pro_applicability_matrix(study_data = study_data,
                                          meta_data = md0,
                                          label_col = LABEL,
                                          max_vars_per_plot =
                                            max_vars_per_plot),
    regexp =
      paste("The DATA_TYPE for variable.s. ..PSEUDO_ID.. is not",
            "defined in the metadata."),
    perl = TRUE)

  md0 <- meta_data
  md0$DATA_TYPE[[2]] <- "Ordinal"
  expect_warning(
    expect_error(
      appmatrix <- pro_applicability_matrix(study_data = study_data,
                                            meta_data = md0,
                                            label_col = LABEL,
                                            max_vars_per_plot =
                                              max_vars_per_plot),
      regexp =
        paste("Please map data types to: .+integer.+, .+string.+,",
              ".+float.+, .+datetime.+."),
      perl = TRUE),
    regexp =
      paste("The data type.s.: ..Ordinal.. is not",
            "eligible in the metadata concept."),
    all = TRUE,
    perl = TRUE)

  expect_silent(
    appmatrix <- pro_applicability_matrix(study_data = study_data,
                                          meta_data = meta_data,
                                          label_col = LABEL,
                                          split_segments = TRUE)
  )

  md0 <- meta_data
  md0$KEY_STUDY_SEGMENT[[2]] <- NA
  expect_warning(
    appmatrix <- pro_applicability_matrix(study_data = study_data,
                                          meta_data = md0,
                                          label_col = LABEL,
                                          split_segments = TRUE),
    regexp =
      paste("Some KEY_STUDY_SEGMENTS are NA.",
            "Will assign those to an artificial segment .+Other.+"),
    all = TRUE,
    perl = TRUE
  )

  expect_warning(
    appmatrix <- pro_applicability_matrix(study_data = study_data,
                                          meta_data = meta_data,
                                          label_col = LABEL,
                                          split_segments = TRUE,
                                          max_vars_per_plot = 2),
    regexp =
      paste("n pro_applicability_matrix: Will split segemnt",
            ".+ arbitrarily avoiding too large figures"),
    all = TRUE,
    perl = TRUE
  )


  md0 <- meta_data
  md0$KEY_STUDY_SEGMENT <- NULL
  expect_warning(
    appmatrix <- pro_applicability_matrix(study_data = study_data,
                                          meta_data = md0,
                                          label_col = LABEL,
                                          split_segments = TRUE),
    regexp = paste("Stratification for KEY_STUDY_SEGMENTS is not",
                   "possible due to missing metadata. Will split arbitrarily",
                   "avoiding too large figures"),
    perl = TRUE,
    all = TRUE
  )

  appmatrix <- pro_applicability_matrix(study_data = study_data,
                                        meta_data = meta_data,
                                        label_col = LABEL)

  expect_length(appmatrix$ApplicabilityPlotList, 5)
  expect_lt(abs(suppressWarnings(sum(na.rm = TRUE,
    as.numeric(as.matrix(appmatrix$SummaryTable))
  )) - 2225), 5)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("appmatrix plot ok",
                              appmatrix$ApplicabilityPlot)
  vdiffr::expect_doppelganger("appmatrix plot for segment v10000 ok",
                              appmatrix$ApplicabilityPlotList$v10000)

})
