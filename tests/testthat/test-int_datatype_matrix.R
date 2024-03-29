test_that("int_datatype_matrix works", {
  skip_on_cran() # slow and not so complicated. also, errors will be obvious.
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  expect_error({
    appmatrix <- int_datatype_matrix(study_data = study_data,
                                     meta_data = meta_data,
                                     label_col = LABEL,
                                     max_vars_per_plot = c())
    },
    regexp = "max_vars_per_plot must be one strictly positive"
  )

  expect_error(
    int_datatype_matrix(resp_vars = "SEX_0",
                        study_data = study_data,
                        meta_data = meta_data[,
                                              setdiff(colnames(meta_data),
                                                      "DATA_TYPE")],
                        label_col = LABEL),
    regexp =
      paste("Missing columns .+DATA_TYPE.+ from .+meta_data.+.")
  )

  md0 <- meta_data
  md0$DATA_TYPE[2] <- NA

  expect_error(
    int_datatype_matrix(resp_vars = "SEX_0",
                        study_data = study_data,
                        meta_data = md0,
                        label_col = LABEL),
    regexp =
      paste("The DATA_TYPE for variable.s.+PSEUDO_ID.+is not",
            "defined in the metadata.")
  )

  md0 <- meta_data
  md0$DATA_TYPE[2] <- "MY_TYPE"

  expect_error(
    expect_warning(
      int_datatype_matrix(resp_vars = "SEX_0",
                          study_data = study_data,
                          meta_data = md0,
                          label_col = LABEL),
      all = TRUE,
      regexp =
        "The data type.s.+MY_TYPE.+is not eligible in the metadata concept."
    ),
    regexp =
      paste("Please map data types to: .+integer.+,",
            ".+string.+,",
            ".+float.+datetime.+")
  )

  expect_warning(
    appmatrix <- int_datatype_matrix(
                                   study_data = study_data,
                                   meta_data = meta_data[,
                                                         setdiff(
                                                           colnames(meta_data),
                                                           c(STUDY_SEGMENT,
                                                             KEY_STUDY_SEGMENT)
                                                         ),
                                                         drop = FALSE],
                                   label_col = LABEL,
                                   split_segments = TRUE),
    all = TRUE,
    regexp =  paste("Stratification for STUDY_SEGMENT is not possible",
                    "due to missing metadata.",
                    "Will split arbitrarily avoiding too large figures")
  )

  expect_equal(length(appmatrix$DataTypePlotList), 3)

  appmatrix <- int_datatype_matrix(resp_vars = "SEX_0",
                                   study_data = study_data,
                                   meta_data = meta_data,
                                   label_col = LABEL)

  expect_true(all(appmatrix$ReportSummaryTable$MATCH == 0))
  expect_equal(nrow(appmatrix$ReportSummaryTable), 1)
  expect_equal(ncol(appmatrix$ReportSummaryTable), 3)
  expect_identical(colnames(appmatrix$ReportSummaryTable),
                   c("Variables", "MATCH", "N"))

  appmatrix <- int_datatype_matrix(study_data = study_data,
                                   meta_data = meta_data,
                                   label_col = LABEL)

  expect_equal(nrow(appmatrix$ReportSummaryTable), 53)
  expect_equal(ncol(appmatrix$ReportSummaryTable), 3)
  expect_true(all(appmatrix$ReportSummaryTable$MATCH == 0))

  skip_on_cran()
  skip_if_not(capabilities()["long.double"])
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("integrity datatype",
                              appmatrix$SummaryPlot)

  for (n in names(appmatrix$DataTypePlotList)) {
    vdiffr::expect_doppelganger(sprintf("intDt%s", n),
                                appmatrix$DataTypePlotList[[n]])
  }

})
