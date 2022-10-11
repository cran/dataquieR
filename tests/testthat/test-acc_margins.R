test_that("acc_margins works without label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

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
                   "\\(float\\) does not have an allowed type \\(!float\\)"),
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
    regexp = paste("Need excactly one element in argument group_vars,",
                   "got 2: .DEV_NO_0, USR_BP_0."),
    perl = TRUE
  )


  gv <- prep_map_labels("SEX_0", meta_data, VAR_NAMES, LABEL)

  expect_warning(
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = c("SEX_0"),
                label_col = LABEL),
    regexp = sprintf(
      "(%s|%s|%s|%s|%s)",
      paste("Due to missing values in  or SEX_0 N=60",
                   "observations were excluded."),
      paste("Due to missing values in CRP_0 N=241 observations were excluded."),
      paste("No or many threshold type specified and set to empirical."),
      paste(
      "No or many minimum observation count was specified and is set to n=5."),
      paste("No co_vars specified")
    ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning( # float
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL)
  )

  expect_warning( # float
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                threshold_value = data.frame(l = letters, L = LETTERS),
                label_col = LABEL),
    regexp = "acc_margins: threshold_value is not numeric.1.: .+,",
    perl = TRUE,
    all = FALSE
  )

  expect_warning( # integer
    acc_margins(resp_vars = "ASTHMA_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL)
  )

  expect_error({
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data)
    },
    regexp = paste("Argument group_vars is NULL"),
    perl = TRUE
  )


  expect_warning(
    expect_error({
      res1 <-
        acc_margins(resp_vars = "v00014", study_data = study_data,
                    meta_data = meta_data, group_vars = "v00001")
      },
      regexp =
        conditionMessage(attr(
          try(
            lm("Petal.Length ~ Sepal.Length + Species",
               iris[iris$Species == "setosa", ]),
            silent = TRUE
          ),
          "condition")),
      fixed = TRUE
    )
  )

  expect_warning(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  min_obs_in_subgroup = NA),
    regexp =
      sprintf("(%s|%s|%s|%s|%s)",
        paste("No co_vars specified"),
        paste("min_obs_in_subgroup is not integer: .+NA.+,",
              "setting it to default value 5."),
        paste("Due to missing values .+ observations were excluded."),
        paste("No or many minimum observation count was",
              "specified and is set to n=5"),
        paste("No or many threshold type specified and set to empirical.")
      ),
    perl = TRUE,
    all = TRUE
  )

  expect_warning(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  min_obs_in_subgroup = 2),
    regexp =
      paste("In acc_margins: min_obs_in_subgroup cannot be set below 5.")
  )

  expect_warning(
      res1 <-
        acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016"),
    regexp =
      sprintf(
        "(%s|%s|%s|%s|%s)",
        paste(
      "No or many minimum observation count was specified and is set to n=5."),
        paste("No co_vars specified"),
        paste("Due to missing values in  or v00016 N=308",
              "observations were excluded."),
        paste("Due to missing values in v00014 N=131",
              "observations were excluded."),
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
      na.rm = TRUE) - 12858.53)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 0
  )

  expect_identical(colnames(res1$SummaryTable), c("Variables", "GRADING"))
})

test_that("acc_margins works with label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
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

expect_warning(
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
            "observations were excluded."),
      paste("threshold_value is not numeric.+it to default value 1.")
    ),
  perl = TRUE,
  all = TRUE
)

expect_warning(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "none"),
  regexp =
    sprintf(
      "(%s|%s|%s|%s)",
      paste(
       "No or many minimum observation count was specified and is set to n=5."),
      paste("No co_vars specified"),
      paste("Due to missing values in  or DEV_NO_0 N=308",
            "observations were excluded."),
      paste("Due to missing values in CRP_0 N=131",
            "observations were excluded.")
    ),
  perl = TRUE,
  all = TRUE
)

expect_warning(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "user"),
  regexp =
    sprintf(
      "(%s|%s|%s|%s|%s)",
      paste(
       "No or many minimum observation count was specified and is set to n=5."),
      paste("No co_vars specified"),
      paste("Due to missing values in  or DEV_NO_0 N=308",
            "observations were excluded."),
      paste("Due to missing values in CRP_0 N=131",
            "observations were excluded."),
      paste("Threshold was set to user but no value for the unit of",
            "measurements was defined.")),
  perl = TRUE,
  all = TRUE
)

expect_warning(
  res1 <-
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "empirical"),
  regexp =
    sprintf(
      "(%s|%s|%s|%s)",
      paste(
       "No or many minimum observation count was specified and is set to n=5."),
      paste("No co_vars specified"),
      paste("Due to missing values in  or DEV_NO_0 N=308",
            "observations were excluded."),
      paste("Due to missing values in CRP_0 N=131",
            "observations were excluded.")
    ),
  perl = TRUE,
  all = TRUE
)

expect_warning(
    res1 <-
      acc_margins(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data, group_vars = "DEV_NO_0",
                  label_col = LABEL),
    regexp =
      sprintf(
        "(%s|%s|%s|%s)",
        paste(
      "No or many minimum observation count was specified and is set to n=5."),
        paste("No co_vars specified"),
        paste("Due to missing values .+",
              "observations were excluded."),
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
      na.rm = TRUE) - 12858.53)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryTable)),
      na.rm = TRUE))), 0
  )

  expect_identical(colnames(res1$SummaryTable), c("Variables", "GRADING"))

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("margins plot for CRP_0 ok",
                              res1$SummaryPlot)
})
