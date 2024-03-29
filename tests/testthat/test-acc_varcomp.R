test_that("acc_varcomp works without label_col", {
  skip_on_cran()
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  expect_warning(
    expect_error({
      res1 <-
        acc_varcomp(resp_vars = "v00014", study_data = study_data,
                    meta_data = meta_data)
    },
    regexp =
      paste(".*acc_varcomp expects one group_var per resp_var.",
            "Here, it has been called with 1 resp_vars but 0 group_vars"),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s)",
        paste("Missing argument .+group_vars.+",
              "without default value. Setting to NULL.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_error(
    res1 <-
      acc_varcomp(resp_vars = c("CRP_0", "SBP_0"), study_data = study_data,
                  meta_data = meta_data, group_vars = c("DEV_NO_0", "USR_BP_0"),
                  label_col = LABEL),
    regexp =
      sprintf(
        "(%s)",
        paste("Need exactly one element in argument group_vars,",
              "got 2: .DEV_NO_0, USR_BP_0.")
      ),
    perl = TRUE
  )

  suppressMessages(expect_message(
    res1 <-
      acc_varcomp(resp_vars = c("DBP_0", "SBP_0"), study_data = study_data,
                  meta_data = meta_data, group_vars = c("USR_BP_0"),
                  label_col = LABEL),
    regexp =
      sprintf(
        "(%s)",
        paste("Levels .+USR_559.+ were excluded",
              "due to less than 30 observations.")
      ),
    perl = TRUE,
    all = FALSE
  ))

  expect_true(all(
    c("SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 1638.564)), 0.1
  )

  expect_equal(res1$ScalarValue_max_icc, 0.111)
  expect_equal(res1$ScalarValue_argmax_icc, "DBP_0")

})

test_that("acc_varcomp works with label_col", {
  skip_on_cran()

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  expect_warning(
    expect_error({
      res1 <-
        acc_varcomp(resp_vars = "SBP_0", study_data = study_data,
                    label_col = LABEL,
                    meta_data = meta_data)
    },
    regexp = paste(".*acc_varcomp expects one group_var per resp_var. Here,",
                   "it has been called with 1 resp_vars but 0 group_vars"),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s)",
        paste("Missing argument .+group_vars.+",
              "without default value. Setting to NULL.")
      ),
    perl = TRUE,
    all = TRUE
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
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  suppressMessages(expect_message(
    res1 <-
      acc_varcomp(resp_vars = c("DBP_0", "SBP_0"), study_data = study_data,
                  meta_data = meta_data, group_vars = c("USR_BP_0"),
                  label_col = LABEL, min_obs_in_subgroup = "k",
                  min_subgroups = "x"),
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("Could not convert min_obs_in_subgroup .+k.+ to a",
              "number. Set to standard value."),
        paste("Could not convert min_subgroups .+x.+ to a number.",
              "Set to standard value."),
        paste("Levels .+USR_559.+ were excluded",
              "due to less than 30 observations.")
      ),
    perl = TRUE,
    all = FALSE
  ))

  expect_true(all(
    c("SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 1638.564)), 0.1
  )

  expect_equal(res1$ScalarValue_max_icc, 0.111)
  expect_equal(res1$ScalarValue_argmax_icc, "DBP_0")

})

test_that("acc_varcomp works without resp_vars", {
  skip_on_cran()

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  expect_error(
    res1 <-
      acc_varcomp(study_data = study_data,
                  meta_data = meta_data,
                  group_vars = c("v00016", "v00012")),
    regexp =
      paste("Need exactly one element in argument group_vars,",
            "got 2: .v00016, v00012.")

  )

  res1 <-
    suppressMessages(acc_varcomp(study_data = study_data,
                meta_data = meta_data,
                group_vars = "v00016"))

  expect_true(all(
    c("SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE) - 73930.06)), 0.1
  )

  expect_equal(res1$ScalarValue_max_icc, 0.021)
  expect_equal(res1$ScalarValue_argmax_icc, "v00014")


})

test_that("acc_varcomp stops on too few subgroups", {
  skip_on_cran()

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  expect_message(
    res1 <-
      acc_varcomp(resp_vars = "v00014",
                  study_data = study_data,
                  meta_data = meta_data,
                  group_vars = "v00016",
                  min_subgroups = 50),
    regexp = "5 . 50 levels in .+v00016.+ Will not compute ICCs for .+v00014.+."

  )

  expect_length(res1$ScalarValue_argmax_icc, 0)
  expect_equal(res1$ScalarValue_max_icc, -Inf)
  expect_equal(nrow(res1$SummaryTable), 0)
  expect_equal(ncol(res1$SummaryTable), 11)

})
