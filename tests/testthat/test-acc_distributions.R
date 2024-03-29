test_that("acc_distributions catches unsuitable input and works otherwise", {
  skip_on_cran() # slow, errors obvious
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  # data type mismatch between study data and metadata will be converted
  # -> runs without problems
  sd0 <- study_data
  sd0$v00000 <- as.character(sd0$v00000)
  md0 <- meta_data
  expect_silent(
    res1 <- acc_distributions(
        resp_vars ="v00000",
        study_data = sd0, meta_data = md0)
  )

  # all values NA
  sd0 <- study_data
  sd0$v00000 <- NA_real_
  expect_error(
      res1 <-
        acc_distributions(
          resp_vars <- "v00000",
          study_data = sd0, meta_data = md0)
    ,
    regexp =
      "Variable .+v00000.+resp_vars.+ has only NA observations",
    perl = TRUE
  )

  # too few distinct values
  sd0 <- study_data
  sd0$v00000 <- 0
  # error if there is only one variable
  expect_error(
    res1 <-
      acc_distributions(
       resp_vars <- "v00000",
       study_data = sd0, meta_data = md0)
   ,
   regexp =
     paste("Variable .+v00000.+resp_vars.+ has fewer distinct values",
           "than required"),
   perl = TRUE
  )
  # warning if there are other variables that can be used by the function
  expect_message(
    expect_warning(
      res1 <-
        acc_distributions(
          resp_vars <- c("v00000", "v00002"),
          study_data = sd0, meta_data = md0),
      regexp = "In .+resp_vars.+ variables .+v00000.+ were excluded.",
      perl = TRUE),
    regexp = paste("Variable .+v00000.+resp_vars.+ has fewer distinct values",
                   "than required"),
    perl = TRUE
  )

  # more than one grouping variable
  md0 <- meta_data
  expect_error(
      res1 <-
        acc_distributions(resp_vars = head(meta_data$VAR_NAMES[
          meta_data$DATA_TYPE %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)
        ], 5),
                          study_data = study_data, meta_data = md0,
                          group_vars =
                            c("v00012", "v00103")),
      regexp =
        paste("Need exactly one element in argument group_vars,",
              "got 2: .v00012, v00103.")
  )

  # unsuitable grouping variable
  md0 <- meta_data
  md0[meta_data$VAR_NAMES == "v00012", VALUE_LABELS] <- NA
  expect_warning(
    res1 <-
      acc_distributions(resp_vars = head(meta_data$VAR_NAMES[
        meta_data$DATA_TYPE %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)
      ], 5),
      study_data = study_data, meta_data = md0,
      group_vars =
        c("v00012")),
    regexp =
      paste("Variables v00012 have no assigned labels and levels and can",
            "therefore not be used as grouping variables in acc_distributions.")
  )

  # check that only suitable variables are selected
  expect_warning(suppressMessages(
    res1 <-
      acc_distributions(study_data = study_data, meta_data = meta_data)))
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    39
  )

  # only two string variables in the study data, but not specified as resp_vars
  # thus, the function will only search for integer and float variables
  # (currently) and not use the two given string variables
  md0 <- subset(meta_data, VAR_NAMES %in% c("v00001", "v00103"))
  md0$KEY_STUDY_SEGMENT <- NA
  sd0 <- study_data[, c("v00001", "v00103")]
  expect_error(
    suppressWarnings(suppressMessages(
      res1 <-
        acc_distributions(
          study_data = sd0, meta_data = md0)
    )),
    regexp =
      "No suitable variables were defined for acc_distributions."
  )
})

test_that("acc_distributions works with label_col", {
  skip_on_cran() # slow, errors obvious
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  expect_warning(suppressMessages(
    res1 <-
      acc_distributions(study_data = study_data, meta_data = meta_data,
                        label_col = LABEL)))
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    39
  )
})

test_that("acc_distributions works with group_vars", {
  skip_on_cran() # slow test
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  expect_warning(suppressMessages(
    res1 <-
      acc_distributions(study_data = study_data, meta_data = meta_data,
                        label_col = LABEL, group_vars = "CENTER_0")))
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    38
  )
  # TODO: skip_if_not(capabilities()["long.double"])
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("ecdf plot for sbp0 ok",
                              res1$SummaryPlotList$SBP_0)
})

test_that("acc_distributions robust with miss-codes", {
  skip_on_cran() # slow test.
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  md0 <- meta_data
  sd0 <- study_data
  sd0$v00003[8:50] <- 8:50
  sd0$v00003[[6]] <- 9999999
  expect_silent(
    res1 <-
      acc_distributions(resp_vars = "v00003",
      study_data = sd0, meta_data = md0,
      group_vars =
        c("v00012")))
  sd0$v00003[[6]] <- 10000000
  expect_silent(
    res1 <-
      acc_distributions(resp_vars = "v00003",
                        study_data = sd0, meta_data = md0,
                        group_vars =
                          c("v00012")))
})

test_that("acc_distributions is robust to other issues", {
  skip_on_cran() # slow, errors obvious
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  md0 <- meta_data
  sd0 <- study_data
  # drop the last category, which is given as value label - this should get
  # caught by the function
  sd0$v00000[which(sd0$v00000 == 5)] <- 2
  expect_silent(
    res1 <-
      acc_distributions(resp_vars = "v00000",
                        study_data = sd0, meta_data = md0)
  )
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  vdiffr::expect_doppelganger("empty_cat5",
                              res1$SummaryPlotList$v00000)
  # use non-consecutive codes for categories
  sd0$v00000[which(sd0$v00000 == 3)] <- 33
  sd0$v00000[which(sd0$v00000 %in% c(2,4))] <- 111
  md0$VALUE_LABELS[which(md0$VAR_NAMES == "v00000")] <-
    "33 = Berlin | 1 = Hamburg | 111 = Cologne"
  expect_silent(
    res2 <- acc_distributions_prop(resp_vars = "v00000",
                                   study_data = sd0, meta_data = md0)
  )
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  vdiffr::expect_doppelganger("non_consecutive_codes_work",
                              res2$SummaryPlotList$v00000)
  # only few different float values, should still produce a histogram
  # (here with four bars, a bar chart would have five bars)
  sd0$v00009 <- rep(c(19.2, 19.9, 22.5, 25.7, 29.4), each = 600)
  expect_silent(
    res3 <- acc_distributions_loc(resp_vars = "v00009",
                                  study_data = sd0, meta_data = md0)
  )
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  vdiffr::expect_doppelganger("few_float_values",
                              res3$SummaryPlotList$v00009)

  prep_load_workbook_like_file("meta_data_v2")
  md0 <- prep_meta_data_v1_to_item_level_meta_data()
  md0 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = md0)

  sd0 <- study_data
  md0 <-
    prep_scalelevel_from_data_and_metadata(study_data = sd0,
                                           meta_data = md0)
  sd0$v00002 <- as.character(util_study_var2factor(
    "SEX_0", study_data = sd0, code_name = c(VALUE_LABELS),
    meta_data = md0, include_sysmiss = FALSE)[["SEX_0"]]
  )
  expect_message(
    expect_error(
      acc_distributions(study_data = sd0, meta_data = md0, resp_vars = "SEX_0",
                        label_col = "LABEL")
    )
  )

  md0[[DATA_TYPE]][md0[[LABEL]] == "SEX_0"] <- "string"
  expect_silent(
    acc_distributions(study_data = sd0, meta_data = md0, resp_vars = "SEX_0",
                      label_col = "LABEL")
  )
})
