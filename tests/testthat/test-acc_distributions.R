test_that("acc_distributions works with 2 args", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  md0 <- subset(meta_data, VAR_NAMES %in% c("v00001", "v00103"))
  expect_error(
    suppressWarnings(suppressMessages(
    res1 <-
      acc_distributions(
        study_data = study_data, meta_data = md0)
    )),
    regexp =
      "No suitable variables were defined."
  )

  sd0 <- study_data
  sd0$v00000 <- as.character(sd0$v00000)
  md0 <- meta_data
  expect_warning(
    res1 <-
      acc_distributions(
        resp_vars <- "v00000",
        study_data = sd0, meta_data = md0)
    ,
    regexp =
      "No variables left to analyse."
  )

  sd0 <- study_data
  sd0$v00000 <- NA_real_
  expect_warning(
      res1 <-
        acc_distributions(
          resp_vars <- "v00000",
          study_data = sd0, meta_data = md0)
    ,
    regexp =
      paste("(No variables left to analyse.|contain",
            "NAs only and will be removed from analyses.)")
  )

  sd0 <- study_data
  sd0$v00000 <- 42.0
  expect_warning(
    res1 <-
      acc_distributions(
        resp_vars <- "v00000",
        study_data = sd0, meta_data = md0)
    ,
    regexp =
      paste("(No variables left to analyse.|contain",
            "only one value and will be removed from analyses.)")
  )

  md0 <- meta_data
  expect_warning(
      res1 <-
        acc_distributions(resp_vars = head(meta_data$VAR_NAMES[
          meta_data$DATA_TYPE %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)
        ], 5),
                          study_data = study_data, meta_data = md0,
                          group_vars =
                            c("v00012", "v00103")),
      regexp =
        sprintf(
          "(%s|%s|%s)",
          paste("All variables defined to be integer or float",
                "in the metadata are used"),
          paste("Variables v10000, v20000, v30000 contain only",
                "one value and will be removed from analyses."),
          paste("Only 1 group variables allowed. Variable:",
                "v00012 was selected.")
        ),
      all = TRUE,
      perl = TRUE
    )

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
      sprintf(
        "(%s|%s|%s)",
        paste("All variables defined to be integer or float",
              "in the metadata are used"),
        paste("Variables v10000, v20000, v30000 contain only",
              "one value and will be removed from analyses."),
        paste("v00012 have no assigned labels and levels.")
      )
  )

  expect_warning(
    res1 <-
      acc_distributions(study_data = study_data, meta_data = meta_data),
      regexp =
        sprintf(
          "(%s|%s)",
          paste("All variables defined to be integer or float",
                "in the metadata are used"),
          paste("Variables v10000, v20000, v30000 contain only",
                "one value and will be removed from analyses.")
        ),
      perl = TRUE,
      all = TRUE
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    40
  )
  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryPlotList$v00000$data)),
        na.rm = TRUE) - 1125630)), 0.5
  )
})

test_that("acc_distributions works with label_col", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    res1 <-
      acc_distributions(study_data = study_data, meta_data = meta_data,
                        label_col = LABEL),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("All variables defined to be integer or float",
              "in the metadata are used"),
        paste("Variables PART_STUDY, PART_PHYS_EXAM, PART_LAB contain only",
              "one value and will be removed from analyses.")
      ),
    perl = TRUE,
    all = TRUE
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    40
  )
  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$SummaryPlotList$ITEM_8_0$data)),
      na.rm = TRUE) - 774242.4)), 0.5
  )
})

test_that("acc_distributions works with group_vars", {
  skip_on_cran() # slow test
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  expect_warning(
    res1 <-
      acc_distributions(study_data = study_data, meta_data = meta_data,
                        label_col = LABEL, group_vars = "CENTER_0"),
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("All variables defined to be integer or float",
              "in the metadata are used"),
        paste("Variables CENTER_0 are not of type float or integer",
              "and will be removed from analyses."),
        paste("Variables PART_STUDY, PART_PHYS_EXAM, PART_LAB contain only",
              "one value and will be removed from analyses.")
      ),
    perl = TRUE,
    all = TRUE
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    39
  )
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("ecdf plot for sbp0 ok",
                              res1$SummaryPlotList$SBP_0)
})

test_that("acc_distributions robust with miss-codes", {
  skip_on_cran() # slow test.
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())
  md0 <- meta_data
  sd0 <- study_data
  sd0$v00003[8:50] <- 8:50 # ensure, that we get a histogram, either by
  # having > 30 values or fractions.
  sd0$v00003[[6]] <- 9999999
  expect_warning(
    res1 <-
      acc_distributions(resp_vars = "v00003",
      study_data = sd0, meta_data = md0,
      group_vars =
        c("v00012")),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("I have 13131879 breaks. Did you forget to specify some missing",
              "codes .+9999999.+ Will arbitrarily reduce the number of breaks",
              "below 10000 to avoid rendering problems."),
        paste("For .+v00003.+. Will arbitrarily reduced the number of breaks",
              "to 6415 <= 10000 to avoid rendering problems.")
      ),
    all = TRUE,
    perl = TRUE
  )
  sd0$v00003[[6]] <- 10000000
  expect_warning(
    res1 <-
      acc_distributions(resp_vars = "v00003",
                        study_data = sd0, meta_data = md0,
                        group_vars =
                          c("v00012")),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("In acc_distributions: For .+v00003.+, I have 13131881 breaks.",
              "Did you forget to specify some missing codes .+1e.07.+ or .+8.+",
              "Will arbitrarily reduce the number of breaks below 10000 to",
              "avoid rendering problems."),
        paste("For .+v00003.+. Will arbitrarily reduced the number of breaks",
              "to 6415 <= 10000 to avoid rendering problems.")
      ),
    all = TRUE,
    perl = TRUE
  )
})
