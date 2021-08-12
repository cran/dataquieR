test_that("acc_multivariate_outlier works with 3 args", {
  skip_if_translated()
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_error(
    res1 <-
      acc_multivariate_outlier(
        study_data = study_data, meta_data = meta_data),
    regexp =
      "argument .+resp_vars.+ is missing, with no default",
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_multivariate_outlier(
        resp_vars = "v00014",
        study_data = study_data, meta_data = meta_data),
    regexp =
      "Need at least two variables for multivariate outliers.",
    perl = TRUE
  )

  expect_warning(
    res1 <-
      acc_multivariate_outlier(resp_vars = c("v00014", "v00006"),
                         study_data = study_data, meta_data = meta_data),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("As no ID-var has been specified the rownumbers will be used."),
        paste("Due to missing values in v00014, v00006 or dq_id N=602",
              "observations were excluded.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true(all(c("FlaggedStudyData",
                    "SummaryTable",
                    "SummaryPlot") %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$FlaggedStudyData)),
      na.rm = TRUE) - 4493047)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 182
  )
})

test_that("acc_multivariate_outlier works with label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_error(
    res1 <-
      acc_multivariate_outlier(
        label_col = LABEL,
        study_data = study_data, meta_data = meta_data),
    regexp =
      "argument .+resp_vars.+ is missing, with no default",
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_multivariate_outlier(
        label_col = LABEL,
        resp_vars = "CRP_0",
        study_data = study_data, meta_data = meta_data),
    regexp =
      "Need at least two variables for multivariate outliers.",
    perl = TRUE
  )

  expect_warning(
    res1 <-
      acc_multivariate_outlier(resp_vars = c("CRP_0", "GLOBAL_HEALTH_VAS_0"),
                               label_col = LABEL,
                               study_data = study_data, meta_data = meta_data),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("As no ID-var has been specified the rownumbers will be used."),
        paste("Due to missing values in CRP_0, GLOBAL_HEALTH_VAS_0 or",
              "dq_id N=602 observations were excluded.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true(all(c("FlaggedStudyData",
                    "SummaryTable",
                    "SummaryPlot") %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$FlaggedStudyData)),
      na.rm = TRUE) - 4493047)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 182
  )

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "acc_mv_outlierCRP0GLOBHEAVA0",
                              res1$SummaryPlot)
})
