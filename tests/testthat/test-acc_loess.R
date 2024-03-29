test_that("acc_loess works without label_col", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  skip_on_cran() # slow test
  skip_if_translated()
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data)
      },
      regexp = paste(".*Argument time_vars is NULL"),
      perl = TRUE
    ),
    regexp = paste("Missing argument .+time_vars.+ without default value.",
              "Setting to NULL. As a dataquieR developer"),
    perl = TRUE
  )

  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016")
    },
    regexp = paste(".*Argument time_vars is NULL"),
    perl = TRUE
    ),
    regexp = paste("Missing argument .+time_vars.+ without default value.",
                   "Setting to NULL. As a dataquieR developer"),
    perl = TRUE
  )

  sd1 <- study_data
  sd1[["v00017"]][1:1000] <- NA
  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = sd1,
                meta_data = meta_data, group_vars = "v00016",
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp = "Due to missing values in v00016 or v00017, N = 1243 observations were excluded. Due to missing values in v00014, N = 82 observations were excluded"
  )

  sd1 <- study_data
  sd1[["v00017"]] <- as.character(sd1[["v00017"]])
  sd1[["v00017"]][1:1000] <- "2001-02-29"
  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = sd1,
                meta_data = meta_data, group_vars = "v00016",
                time_vars = "v00017"), # ===> "LAB_DT_0"
    regexp = sprintf("(%s|%s)",
                     paste("Data type transformation of .v00017. introduced",
                           "1000 additional missing values"),
                     paste("Due to missing values in v00016 or v00017,",
                           "N = 1243 observations were excluded. Due to",
                           "missing values in v00014, N = 82 observations",
                           "were excluded")),
    perl = TRUE,
    all = TRUE
  )

  expect_error(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                min_obs_in_subgroup = 1:2,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp =
      paste("Need exactly one element in argument min_obs_in_subgroup, got 2"),
    perl = TRUE
  )

  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                resolution = Inf,
                min_obs_in_subgroup = 30,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp = sprintf("(%s|%s)",
                     paste("Argument resolution is not specified correctly and is set to 80 instead"),
                     paste("Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded")),
    perl = TRUE
  )

  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                resolution = "12",
                min_obs_in_subgroup = 30,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp = "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded"
  )

  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                resolution = complex(imaginary = 12),
                min_obs_in_subgroup = 30,
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp = sprintf("(%s|%s)",
                     "Argument resolution is not specified correctly and is set to 80 instead",
                     "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded")
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  mark_time_points = "x",
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      ,
      regexp =
        paste("Argument mark_time_points must match the predicate"),
      perl = TRUE
    )
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  mark_time_points = TRUE,
                  comparison_lines = 42,
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      ,
      regexp =
        paste(".+comparison_lines.+ needs to be a list of arguments",
              "as specified in the documentation."),
      perl = TRUE
    )
  )

  sd0 <- study_data
  sd0$v00017 <- "XXXX"
  suppressMessages(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = sd0,
                  meta_data = meta_data, group_vars = "v00016",
                  mark_time_points = TRUE,
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      ,
      regexp = "Variable .v00017.+time_vars. has only NA observations",
      perl = TRUE
    )
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  mark_time_points = "TRUE",
                  min_obs_in_subgroup = 30,
                  time_vars = "v00017") # ===> "LAB_DT_0"
      , regexp = "Argument mark_time_points must match the predicate"
    )
  )

  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                min_obs_in_subgroup = NA,
                time_vars = "v00017") # ===> "LAB_DT_0"
  )

  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                min_obs_in_subgroup = "x",
                time_vars = "v00017") # ===> "LAB_DT_0"
    ,
    regexp = sprintf("(%s|%s)",
                     "Argument resolution is not specified correctly and is set to 80 instead",
                     "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded")
  )

  expect_message(
    res1 <-
      acc_loess(resp_vars = "v00014", study_data = study_data,
                meta_data = meta_data, group_vars = "v00016",
                time_vars = "v00017", plot_format = "BOTH") # ===> "LAB_DT_0"
    ,
    regexp = "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded"
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  expect_lt(
    suppressWarnings(abs(mean(as.numeric(
      as.matrix(res1$SummaryPlotList$Loess_fits_combined$data)),
      na.rm = TRUE) - 21.82126)), 50
  )
})

test_that("acc_loess works with label_col", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data,
                  label_col = LABEL)
    },
    regexp = paste(".*Argument time_vars is NULL"),
    perl = TRUE
    ),
    regexp = paste("Missing argument .+time_vars.+ without default value.",
                   "Setting to NULL. As a dataquieR developer"),
    perl = TRUE
  )

  expect_warning(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data, group_vars = "DEV_NO_0",
                  label_col = LABEL)
    },
    regexp = paste(".*Argument time_vars is NULL"),
    perl = TRUE
    ),
    regexp = paste("Missing argument .+time_vars.+ without default value.",
                   "Setting to NULL. As a dataquieR developer"),
    perl = TRUE
  )

  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "BOTH")
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  expect_lt(
    suppressWarnings(abs(mean(as.numeric(
      as.matrix(res1$SummaryPlotList$Loess_fits_combined$data)),
      na.rm = TRUE) - 21.82126)), 50
  )
})

test_that("acc_loess output matches", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "BOTH")
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  # TODO: skip_if_not(capabilities()["long.double"])
  # skip_on_travis() # vdiffr fails
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("loess facets plot for CRP_0 ok",
                              res1$SummaryPlotList$Loess_fits_facets)
  vdiffr::expect_doppelganger("loess combined plot for CRP_0 ok",
                              res1$SummaryPlotList$Loess_fits_combined)
})

test_that("acc_loess min_obs_in_subgroups with label_col", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_message(
    expect_error({
      res1 <-
        acc_loess(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data, group_vars = "DEV_NO_0",
                  time_vars = "LAB_DT_0",
                  label_col = LABEL, min_obs_in_subgroup = 999)
    },
    regexp = paste("No data left after data preparation."),
    perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s|%s)",
        "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded.",
        paste("Levels of the group_var with too few observations",
              "were discarded .levels 1, 2, 3, 4, 5.+")
      ),
    perl = TRUE,
    all = TRUE
  )
})

test_that("acc_loess with co-vars output matches", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  sd0 <- study_data
  sd0$v00003[1:10] <- NA
  sd0$v00002[11:20] <- NA
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
                label_col = LABEL)
    ,
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 327 observations were excluded. Due to missing values in CRP_0, N = 130 observations were excluded"
  )

  sd0 <- study_data
  sd0$v00014 <- as.factor(sd0$v00014)
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
                label_col = LABEL, plot_format = "BOTH")
    ,
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  # TODO: skip_if_not(capabilities()["long.double"])
  # skip_on_travis() # vdiffr fails
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("loess facets plot for CRP_0 with Covars ok",
                              res1$SummaryPlotList$Loess_fits_facets)
  vdiffr::expect_doppelganger("loess combined plot for CRP_0 with Covars ok",
                              res1$SummaryPlotList$Loess_fits_combined)
})

test_that("acc_loess works for all time span ranges", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  skip_on_cran() # slow test
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  sd0 <- study_data
  v <- subset(meta_data, LABEL == "LAB_DT_0", VAR_NAMES, TRUE)
  sd0[[v]] <- min(sd0[[v]], na.rm = TRUE)
  expect_error(
    suppressWarnings(res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL)),
    regexp = paste("Variable .+LAB_DT_0.+time_vars.+has fewer distinct values",
                   "than required")
  )

  sd0 <- study_data
  sd0[["v00017"]] <- sample(sd0[["v00017"]][c(100, 700, 1500)],
                            size = nrow(sd0), replace = TRUE) #LAB_DT_0
  expect_silent(
    suppressMessages(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 label_col = LABEL,
                                 comparison_lines = list(type = "quartiles",
                                                         color = "red",
                                                         linetype = 3)))
  )

  sd0 <- study_data
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                resolution = 0.1,
                label_col = LABEL),
    regexp = sprintf(
      "(%s|%s)",
      "Argument resolution is not specified correctly and is set to 80 instead",
      "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
    ),
  all = TRUE
  )

  expect_silent(
    suppressMessages(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 10,
                                 label_col = LABEL))
  )

  expect_silent(
    suppressMessages(res1 <-
                       acc_loess(resp_vars = "CRP_0", study_data = sd0,
                                 meta_data = meta_data, group_vars = "DEV_NO_0",
                                 time_vars = "LAB_DT_0",
                                 resolution = 10000,
                                 label_col = LABEL))
  )

  set.seed(1024)
  sd0 <- data.frame("v00014" = c(rnorm(4000, mean = 10, sd = 1),
                                 rnorm(4000, mean = 20, sd = 10),
                                 rnorm(12, mean = 5, sd = 1),
                                 rnorm(50, mean = 5, sd = 1)),
                    "v00016" = c(rep(1, 4000),
                                 rep(2, 4000),
                                 rep(3, 12),
                                 rep(4, 50)),
                    "v00017" = c(sample(study_data$v00017,
                                        size = 8012, replace = TRUE),
                                 rep(min(study_data$v00017, na.rm = TRUE), 25),
                                 rep(max(study_data$v00017, na.rm = TRUE), 25)))
  md0 <- meta_data[which(meta_data$VAR_NAMES %in% colnames(sd0)), ]
  expect_message(
    acc_test <-
      acc_loess(resp_vars = "v00014", study_data = sd0,
                meta_data = md0, group_vars = "v00016",
                time_vars = "v00017",
                plot_observations = TRUE),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste("Not all entries in .KEY_STUDY_SEGMENT. are found in .VAR_NAMES.+",
            "Could convert 0 of these 3 to match .VAR_NAMES.+standard.+This",
            "may be caused by providing subsets of .meta_data.+"),
      "Due to missing values in",
      paste("Levels of the group_var with too few observations were",
            "discarded .levels 3, 4.+")
    ),
    perl = TRUE,
    all = TRUE
  )
  expect_true("SummaryPlotList" %in% names(acc_test))
  expect_lt(
    suppressWarnings(abs(mean(as.numeric(
      as.matrix(acc_test$SummaryPlotList$v00014$data)),
      na.rm = TRUE) - mean(sd0$v00014))), 50
  )

  sd0 <- study_data
  g <- subset(meta_data, LABEL == "DEV_NO_0", VAR_NAMES, TRUE)
  set.seed(42)
  sd0[[g]][!is.na(sd0[[g]])] <-
    sample(x = 11, size = sum(!is.na(sd0[[g]])),
           replace = TRUE) # for >= 11 groups,
                           # R standard colors are used.
  expect_message(
    res0 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "BOTH")
  )

  sd0[[g]][!is.na(sd0[[g]])] <-
    sample(x = 8, size = sum(!is.na(sd0[[g]])),
           replace = TRUE) # for <= 8 groups,
                           # dataquieR standard colors are used.
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd0,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "BOTH")
  )

  g <- ggplot2::ggplot_build(res1$SummaryPlotList$Loess_fits_combined)
  got1 <- sort(unique(g$data[[1]][["colour"]]))
  hex_code <- sort(c( # the dataquieR colors
    "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8C510A"
  ))
  expect_equal(got1, hex_code)

  g <- ggplot2::ggplot_build(res0$SummaryPlotList$Loess_fits_combined)
  got0 <- sort(unique(g$data[[1]][["colour"]]))
  ggs_default <- sort(ggplot2::scale_color_discrete()$palette(11))

  expect_equal(got0, ggs_default)
})

test_that("acc_loess output matches plot_format=auto", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "AUTO")
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    1
  )
  sd1 <- study_data
  set.seed(42)
  sd1$v00016 <- sample(1:20, size = nrow(sd1), replace = TRUE)
  expect_message(
    res2 <-
      acc_loess(resp_vars = "CRP_0", study_data = sd1,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "AUTO")
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 60 observations were excluded. Due to missing values in CRP_0, N = 241 observations were excluded"
  )
  expect_true("SummaryPlotList" %in% names(res2))
  expect_equal(
    length(res2$SummaryPlotList),
    1
  )
  skip_on_cran()
  # skip_on_travis() # vdiffr fails
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("loess plot for CRP_0 AUTO1 ok",
                              res1$SummaryPlotList$CRP_0)
  vdiffr::expect_doppelganger("loess plot for CRP_0 AUTO2 ok",
                              res2$SummaryPlotList$CRP_0)
})

test_that("acc_loess output matches plot_format=combined", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "COMBINED")
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    1
  )
  skip_on_cran()
  # skip_on_travis() # vdiffr fails
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("loess combined plot for CRP_0 COMBINED ok",
                              res1$SummaryPlotList$CRP_0)
})

test_that("acc_loess output matches plot_format=facets", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "FACETS")
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    1
  )
  skip_on_cran()
  # skip_on_travis() # vdiffr fails
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("loess facets plot for CRP_0 FACETS ok",
                              res1$SummaryPlotList$CRP_0)
})

test_that("acc_loess output matches plot_format=both", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_message(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "BOTH")
    ,
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded"
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  # skip_on_travis() # vdiffr fails
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("loess facets plot for CRP_0 BOTH ok",
                              res1$SummaryPlotList$Loess_fits_facets)
  vdiffr::expect_doppelganger("loess combined plot for CRP_0 BOTH ok",
                              res1$SummaryPlotList$Loess_fits_combined)
})

test_that("acc_loess output matches plot_format=invalid1", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_error(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = "invalid")
    ,
    regexp = "Argument plot_format must match the predicate"
  )
})

test_that("acc_loess output matches plot_format=invalid2", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  # testthat::local_reproducible_output()
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  expect_error(
    res1 <-
      acc_loess(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                time_vars = "LAB_DT_0",
                label_col = LABEL, plot_format = 1:10)
    ,
    regexp = "Need exactly one element in argument plot_format, got 10"
  )
})
