test_that("int_datatype_matrix works", {
  skip_on_cran() # slow and not so complicated. also, errors will be obvious.
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]

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

  expect_warning(
    int_datatype_matrix(resp_vars = "SEX_0",
                        study_data = study_data,
                        meta_data = md0,
                        label_col = LABEL),
    regexp =
      paste("I've predicted the.+DATA_TYPE.+")
  )

  md0 <- meta_data
  md0$DATA_TYPE[2] <- "MY_TYPE"

  expect_silent(
    expect_warning(
      int_datatype_matrix(resp_vars = "SEX_0",
                          study_data = study_data,
                          meta_data = md0,
                          label_col = LABEL),
      regexp =
        "yielding.+v00001 = string"
    )
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
  expect_gte(ncol(appmatrix$ReportSummaryTable), 4)
  expect_identical(sort(gsub("[^a-zA-Z0-9]", "",
                             tolower(colnames(appmatrix$ReportSummaryTable)))),
                   sort(c("convertiblemismatchstable",
                          "convertiblemismatchunstable", "nonconvertiblemismatch",
                          "n", "variables" )))

  appmatrix <- int_datatype_matrix(study_data = study_data,
                                   meta_data = meta_data,
                                   label_col = LABEL)

  expect_equal(nrow(appmatrix$ReportSummaryTable), 53)
  expect_gte(ncol(appmatrix$ReportSummaryTable), 4)
  expect_true(
    all(appmatrix$ReportSummaryTable$`convertible mismatch, unstable` == 0))
  expect_true(
    all(appmatrix$ReportSummaryTable$`convertible mismatch, stable` == 0))
  expect_true(all(appmatrix$ReportSummaryTable$`nonconvertible mismatch` == 0))

  skip_on_cran()
  # TODO: skip_if_not(capabilities()["long.double"])
  skip_if_not_installed("vdiffr")

  expect_doppelganger2("integrity datatype",
                              appmatrix$SummaryPlot)

  for (n in names(appmatrix$DataTypePlotList)) {
    expect_doppelganger2(sprintf("intDt%s", n),
                                appmatrix$DataTypePlotList[[n]])
  }

})
