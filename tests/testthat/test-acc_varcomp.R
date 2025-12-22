test_that("acc_varcomp works without label_col", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  expect_error({
    res1 <-
      acc_varcomp(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data)
  }, regexp =
    paste("Argument group_vars is NULL"),
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_varcomp(resp_vars = c("CRP_0", "SBP_0"), study_data = study_data,
                  meta_data = meta_data, group_vars = c("DEV_NO_0", "USR_BP_0"),
                  label_col = LABEL),
    regexp =
      sprintf(
        "(%s)",
  paste("Need exactly one element in argument resp_vars, got 2: .CRP_0, SBP_0.")
      ),
    perl = TRUE
  )

  suppressMessages(
    res1 <-
      acc_varcomp(resp_vars = c("DBP_0"), study_data = study_data,
                  meta_data = meta_data, group_vars = c("USR_BP_0"),
                  label_col = LABEL))

  expect_true(all(
    c("SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 783.112)), 0.1
  )

  expect_true(res1$ScalarValue_max_icc == 0.112)
  expect_true(res1$ScalarValue_argmax_icc == "DBP_0")

})

test_that("acc_varcomp works with label_col", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  expect_error({
      res1 <-
        acc_varcomp(resp_vars = "SBP_0", study_data = study_data,
                    label_col = LABEL,
                    meta_data = meta_data)
    },
    regexp = paste("Argument group_vars is NULL")
  )

  res1 <-
    acc_varcomp(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016")

  expect_true(all(
    c("SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 2082.221)), 0.1
  )

  expect_equal(res1$ScalarValue_max_icc, 0.021)
  expect_equal(res1$ScalarValue_argmax_icc, "v00014")


})

test_that("acc_varcomp works illegal min_obs_in_subgroup/min_subgroups", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  suppressMessages(expect_error(
    res1 <-
      acc_varcomp(resp_vars = c("DBP_0"), study_data = study_data,
                  meta_data = meta_data, group_vars = c("USR_BP_0"),
                  label_col = LABEL, min_obs_in_subgroup = "k",
                  min_subgroups = "x"),
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("min_obs_in_subgroup needs to be integer > 0"),
        paste("Could not convert min_subgroups .+x.+ to a number.",
              "Set to standard value."),
        paste("Levels .+USR_559.+ were excluded",
              "due to fewer than 30 observations.")
      )
  ))

  suppressMessages(
    res1 <-
      acc_varcomp(resp_vars = c("DBP_0"), study_data = study_data,
                  meta_data = meta_data, group_vars = c("USR_BP_0"),
                  label_col = LABEL)
  )

  expect_true(all(
    c("SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 783.112)), 0.1
  )

  expect_true(res1$ScalarValue_max_icc == 0.112)
  expect_true(res1$ScalarValue_argmax_icc == "DBP_0")

})

test_that("acc_varcomp works without resp_vars", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)

  expect_error(
    res1 <-
      acc_varcomp(study_data = study_data,
                  meta_data = meta_data,
                  group_vars = c("v00016", "v00012")),
    regexp =
      paste("Argument resp_vars is NULL")

  )

  res1 <-
    suppressMessages(acc_varcomp(study_data = study_data,
                meta_data = meta_data,
                resp_vars = "v00004",
                group_vars = "v00016"))

  expect_true(all(
    c("SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 1932.009)), 0.1
  )

  expect_equal(res1$ScalarValue_max_icc, 0.008)
  expect_equal(res1$ScalarValue_argmax_icc, "v00004")


})

test_that("acc_varcomp stops on too few subgroups", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)

  expect_error(
    res1 <-
      acc_varcomp(resp_vars = "v00014",
                  study_data = study_data,
                  meta_data = meta_data,
                  group_vars = "v00016",
                  min_subgroups = 50),
    regexp = "5 . 50 levels in .+v00016.+ Will not compute ICCs for .+v00014.+."

  )

  # expect_length(res1$ScalarValue_argmax_icc, 0)
  # expect_equal(res1$ScalarValue_max_icc, -Inf)
  # expect_equal(nrow(res1$SummaryTable), 0)
  # expect_equal(ncol(res1$SummaryTable), 11)

})
