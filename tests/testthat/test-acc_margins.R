test_that("acc_margins works without label_col", {
  skip_on_cran() # slow
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  expect_error({
    res1 <-
      acc_margins(resp_vars = "v00001", study_data = study_data,
                  meta_data = meta_data)
  },
  regexp = paste(".*Argument .+resp_vars.+: Variable .+v00001.+ \\(na\\)",
                 "has a disallowed scale level \\(.+na.+\\)"),
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

  expect_error( # wrong group_vars
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
                meta_data = meta_data, group_vars = "SEX_0",
                label_col = LABEL),
    regexp = paste("Due to missing values in SEX_0, N = 60 observations",
                   "were excluded. Due to missing values in CRP_0, N = 241",
                   "observations were excluded additionally."),
    perl = TRUE
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
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  md <- prep_get_data_frame("item_level")

  # test function for different scale levels, with and without sorting
  # 1. nominal (dichotomized or not)
  md$SCALE_LEVEL[md$LABEL == "MEAT_CONS_0"] <- SCALE_LEVELS$NOMINAL
  expect_message(
    res2 <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                        meta_data = md, group_vars = "CENTER_0",
                        label_col = LABEL, dichotomize_categorical_resp = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res2$SummaryPlot)), "try-error"))
  expect_message(
    res2b <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, dichotomize_categorical_resp = TRUE,
                         sort_group_var_levels = FALSE)
  )
  expect_false(
    inherits(try(ggplot_build(res2b$SummaryPlot)), "try-error"))

  # should also work if some levels of the grouping variable did detect only
  # a single category
  sd1 <- study_data
  sd1[which(sd1[, md$VAR_NAMES[md$LABEL == "CENTER_0"]] == 1),
      md$VAR_NAMES[md$LABEL == "MEAT_CONS_0"]] <- 0
  sd1[which(sd1[, md$VAR_NAMES[md$LABEL == "CENTER_0"]] == 4),
      md$VAR_NAMES[md$LABEL == "MEAT_CONS_0"]] <- 1
  expect_message(
    res2c <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = sd1,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, dichotomize_categorical_resp = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res2c$SummaryPlot)), "try-error"))

  # variant if the response variable is not being dichotomized
  res2d <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                       meta_data = md, group_vars = "CENTER_0",
                       label_col = LABEL, dichotomize_categorical_resp = FALSE)
  expect_false(
    inherits(try(ggplot_build(res2d$SummaryPlot)), "try-error"))
  res2e <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                       meta_data = md, group_vars = "CENTER_0",
                       label_col = LABEL, dichotomize_categorical_resp = FALSE,
                       sort_group_var_levels = FALSE)
  expect_false(
    inherits(try(ggplot_build(res2e$SummaryPlot)), "try-error"))

  # 2. ordinal (linear model or ordinal regression)
  md$SCALE_LEVEL[md$LABEL == "MEAT_CONS_0"] <- SCALE_LEVELS$ORDINAL
  expect_message(
    res3 <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                        meta_data = md, group_vars = "CENTER_0",
                        label_col = LABEL, sort_group_var_levels = TRUE,
                        include_numbers_in_figures = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res3$SummaryPlot)), "try-error"))
  expect_message(
    res3b <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, sort_group_var_levels = FALSE,
                         include_numbers_in_figures = FALSE,
                         n_violin_max = 3)
  )
  expect_false(
    inherits(try(ggplot_build(res3b$SummaryPlot)), "try-error"))

  expect_message(
    res3c <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, cut_off_linear_model_for_ord = NULL)
  )
  expect_false(
    inherits(try(ggplot_build(res3c$SummaryPlot)), "try-error"))
  expect_message(
    res3d <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, cut_off_linear_model_for_ord = NULL,
                         sort_group_var_levels = FALSE)
  )
  expect_false(
    inherits(try(ggplot_build(res3d$SummaryPlot)), "try-error"))

  # 3. poisson model
  md$SCALE_LEVEL[md$LABEL == "N_INJURIES_0"] <- SCALE_LEVELS$INTERVAL
  expect_message(
    res4 <- acc_margins(resp_vars = "N_INJURIES_0", study_data = study_data,
                        meta_data = md, group_vars = "USR_SOCDEM_0",
                        label_col = LABEL, sort_group_var_levels = TRUE,
                        include_numbers_in_figures = FALSE)
  )
  expect_false(
    inherits(try(ggplot_build(res4$SummaryPlot)), "try-error"))
  expect_message(
    res4b <- acc_margins(resp_vars = "N_INJURIES_0", study_data = study_data,
                         meta_data = md, group_vars = "USR_SOCDEM_0",
                         label_col = LABEL, sort_group_var_levels = FALSE,
                         include_numbers_in_figures = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res4b$SummaryPlot)), "try-error"))

  expect_error(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00001"),
    regexp = "Argument .group_vars.+Variable .v00001.+na. does not have an allowed scale level",
    perl = TRUE
  )

  expect_message(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  min_obs_in_subgroup = 2, sort_group_var_levels = FALSE,
                  n_violin_max = 3),
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
    perl = TRUE
  )

  expect_true(all(
    c("ResultData",
      "SummaryTable",
      "SummaryPlot"
      ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$ResultData)),
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
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
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
  perl = TRUE
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
  perl = TRUE
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
  perl = TRUE
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
  perl = TRUE
)

expect_message(
    res1 <-
      acc_margins(resp_vars = "CRP_0", study_data = study_data,
                  meta_data = meta_data, group_vars = "DEV_NO_0",
                  label_col = LABEL, sort_group_var_levels = FALSE),
    regexp = paste("Due to missing values .+", "observations were excluded."),
    perl = TRUE
  )

  expect_true(all(
    c("ResultData",
      "SummaryTable",
      "SummaryPlot"
    ) %in% names(res1)))

  expect_lt(
    suppressWarnings(abs(sum(as.numeric(
      as.matrix(res1$ResultData)),
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

  # TODO: As soon as vdiffr supports the variant argument for snapshots -- https://github.com/r-lib/vdiffr/issues/125, use it everywhere.
  # r_minor_version <- function() paste0("R", getRversion()[, 1:2])
  # ggplot_version <- function() packageVersion("ggplot2")

  # TODO: skip_if_not(capabilities()["long.double"])
  expect_doppelganger2("margins plot for CRP_0 ok",
                              res1$SummaryPlot)
})
