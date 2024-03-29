test_that("acc_margins works without label_col", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  expect_error({# STRING not allowed
    res1 <-
      acc_margins(resp_vars = "v00001", study_data = study_data,
                  meta_data = meta_data)
  },
  regexp = paste(".*Argument .+resp_vars.+: Variable .+v00001.+ \\(string\\)",
                 "does not have an allowed type \\(integer\\|float\\)"),
  perl = TRUE
  )

  expect_error({# DATETIME not allowed
    res1 <-
      acc_margins(resp_vars = "v00013", study_data = study_data,
                  meta_data = meta_data)
  },
  regexp = paste(".*Argument .+resp_vars.+: Variable .+v00013.+ \\(datetime\\)",
                 "does not have an allowed type \\(integer\\|float\\)"),
  perl = TRUE
  )

  expect_error( # float in group_vars
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "CRP_0",
                label_col = LABEL),
    regexp = paste("Argument .+group_vars.+: Variable .+CRP_0.+",
                   "\\(float\\) has a disallowed type \\(.+float.+\\)"),
    perl = TRUE
  )

  expect_error( # no group_vars
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "not_avail",
                label_col = LABEL),
    regexp = paste("Variable .+not_avail.+ \\(group_vars\\) not found in",
                   "study data. Did you mean .+ASTHMA_0.+\\?"),
    perl = TRUE
  )

  expect_error( # no group_vars
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = NULL,
                label_col = LABEL),
    regexp = paste("Argument group_vars is NULL"),
    perl = TRUE
  )

  skip_on_cran() # the remainder of this file runs slow.

  expect_error( # many group_vars
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = c("DEV_NO_0", "USR_BP_0"),
                label_col = LABEL),
    regexp = paste("Need exactly one element in argument group_vars,",
                   "got 2: .DEV_NO_0, USR_BP_0."),
    perl = TRUE
  )


  expect_message(
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = c("SEX_0"),
                label_col = LABEL),
    regexp = sprintf(
      "(%s|%s)",
      paste("Due to missing values in SEX_0, N = 60 observations",
            "were excluded. Due to missing values in CRP_0, N = 241",
            "observations were excluded additionally."),
      paste("No or many threshold type specified and set to empirical.")
    ),
    perl = TRUE,
    all = TRUE
  )

  expect_message( # float
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL)
  )

  expect_message( # float
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                threshold_value = data.frame(l = letters, L = LETTERS),
                label_col = LABEL),
    regexp = "threshold_value is not numeric.1.: .+,",
    perl = TRUE,
    all = FALSE
  )

  prep_load_workbook_like_file("meta_data_v2")

  md <- prep_get_data_frame("item_level")

  # tweak metadata to enable the test
  md$SCALE_LEVEL[md$LABEL == "MEAT_CONS_0"] <- SCALE_LEVELS$INTERVAL

  expect_message( # integer
    acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                meta_data = md, group_vars = "DEV_NO_0",
                label_col = LABEL)
  )

  expect_error(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data),
    regexp = paste("Argument group_vars is NULL"),
    fixed = TRUE
  )


  expect_error(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00001"),
    regexp = "Argument .group_vars.+Variable .v00001.+na. does not have an allowed scale level",
    perl = TRUE
  )

  # TODO: Should this throw an error or be caught and replaced? (check also the warnings here)
  expect_error(
    res1 <-
      suppressWarnings(acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  min_obs_in_subgroup = NA)),
    regexp = "Argument min_obs_in_subgroup must match the predicate",
    perl = TRUE
  )

  expect_message(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  min_obs_in_subgroup = 2),
    regexp =
      paste("min_obs_in_subgroup is not specified correctly",
            "and is set to 5 instead.")
  )

  expect_message(
      res1 <-
        acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016"),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("Due to missing values in v00016, N = 308",
              "observations were excluded. Due to missing values in",
              "v00014, N = 131 observations were excluded additionally."),
        paste("No or many threshold type specified and set to empirical.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true(all(
    c("SummaryData",
      "SummaryTable",
      "SummaryPlot"
      ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryData)),
      na.rm = TRUE) - 2590.933)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 0
  )

  expect_identical(colnames(res1$SummaryTable), c("Variables", "FLG_acc_ud_loc", "PCT_acc_ud_loc"))
})

test_that("acc_margins works with label_col", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_error({
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data)
  },
  regexp = paste("Argument group_vars is NULL"),
  perl = TRUE
)

suppressWarnings(expect_error(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
              meta_data = meta_data, group_vars = "DEV_NO_0",
              label_col = LABEL, threshold_type = "nonex",
              threshold_value = 1),
  regexp = ".+arg.+ should be one of .+empirical.+, .+user.+, .+none.+"
))

suppressWarnings(expect_error(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = list()),
  regexp = ".+arg.+ must be NULL or a character vector",
  perl = TRUE
))

expect_message(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "none",
                threshold_value = 1),
  regexp =
    sprintf(
      "(Due|%s|%s|%s|%ss|%s)",
      paste(
       "No or many minimum observation.+"),
      paste("No co_vars specified"),
      paste("Due to missing values.+308",
            "observations were excluded."),
      paste("Due to missing values.+131",
            "observations were excluded additionally."),
      paste("threshold_value is not numeric.+it to default value 1.")
    ),
  perl = TRUE,
  all = TRUE
)

expect_message(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "none"),
  regexp =
    sprintf(
      "(%s|%s|%s)",
      paste(
       "No or many minimum observation count was specified and is set to n=5."),
      paste("No co_vars specified"),
      paste("Due to missing values in DEV_NO_0, N = 308",
            "observations were excluded. Due to missing values in",
            "CRP_0, N = 131 observations were excluded additionally")
    ),
  perl = TRUE,
  all = TRUE
)

expect_message(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "user"),
  regexp =
    sprintf(
      "(%s|%s|%s|%s)",
      paste(
       "No or many minimum observation count was specified and is set to n=5."),
      paste("No co_vars specified"),
      paste("Due to missing values in DEV_NO_0, N = 308",
            "observations were excluded. Due to missing values in",
            "CRP_0, N = 131 observations were excluded additionally."),
      paste("Threshold was set to user but no value for the unit of",
            "measurements was defined.")),
  perl = TRUE,
  all = TRUE
)

expect_message(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "empirical"),
  regexp =
    sprintf(
      "(%s|%s)",
      paste(
       "No or many minimum observation count was specified and is set to n=5."),
      paste("Due to missing values in DEV_NO_0, N = 308",
            "observations were excluded. Due to missing values in",
            "CRP_0, N = 131 observations were excluded additionally.")
    ),
  perl = TRUE,
  all = TRUE
)

expect_message(
    res1 <-
      acc_margins(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data, group_vars = "DEV_NO_0",
                  label_col = LABEL),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("Due to missing values .+", "observations were excluded."),
        paste("No or many threshold type specified and set to empirical.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_true(all(
    c("SummaryData",
      "SummaryTable",
      "SummaryPlot"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryData)),
      na.rm = TRUE) - 2590.933)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 0
  )

  expect_identical(colnames(res1$SummaryTable), c("Variables", "FLG_acc_ud_loc", "PCT_acc_ud_loc"))

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("margins plot for CRP_0 ok",
                              res1$SummaryPlot)
})
