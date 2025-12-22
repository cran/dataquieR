test_that("acc_margins works without label_col", {
  skip_on_cran() # slow

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
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

  expect_message2(
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "SEX_0",
                label_col = LABEL),
    regexp = paste("Due to missing values in SEX_0, N = 60 observations",
                   "were excluded. Due to missing values in CRP_0, N = 241",
                   "observations were excluded additionally."),
    perl = TRUE
  )

  expect_message2( # float
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL)
  )

  expect_message2( # float util_margins_lm
    acc_margins(resp_vars = "CRP_0", study_data = study_data,
                meta_data = meta_data, group_vars = "DEV_NO_0",
                label_col = LABEL, threshold_type = "none")
  )

  expect_message2( # float
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
  expect_message2( #util_margins_bin
    res2 <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                        meta_data = md, group_vars = "CENTER_0",
                        label_col = LABEL, dichotomize_categorical_resp = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res2$SummaryPlot)), "try-error"))
  expect_message2(  #util_margins_bin
    res2_new <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                        meta_data = md, group_vars = "CENTER_0",
                        label_col = LABEL, dichotomize_categorical_resp = TRUE,
                        threshold_type = "none")
  )
  expect_message2(
    res2b <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, dichotomize_categorical_resp = TRUE,
                         sort_group_var_levels = FALSE)
  )
  expect_false(
    inherits(try(ggplot_build(res2b$SummaryPlot)), "try-error"))

  #util_margins_bin binary
  expect_message2(
    res2b_bin <- acc_margins(resp_vars = "PREGNANT_0", study_data = study_data,
                         meta_data = md, group_vars = "USR_SOCDEM_0",
                         label_col = LABEL)
  )

  # should also work if some levels of the grouping variable did detect only
  # a single category
  sd1 <- study_data
  sd1[which(sd1[, md$VAR_NAMES[md$LABEL == "CENTER_0"]] == 1),
      md$VAR_NAMES[md$LABEL == "MEAT_CONS_0"]] <- 0
  sd1[which(sd1[, md$VAR_NAMES[md$LABEL == "CENTER_0"]] == 4),
      md$VAR_NAMES[md$LABEL == "MEAT_CONS_0"]] <- 1
  expect_message2( #util_margins_bin
    res2c <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = sd1,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, dichotomize_categorical_resp = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res2c$SummaryPlot)), "try-error"))

  # variant if the response variable is not being dichotomized
  res2d <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,  #util_margins_nom
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
  expect_message2( #util_margins_lm
    res3 <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                        meta_data = md, group_vars = "CENTER_0",
                        label_col = LABEL, sort_group_var_levels = TRUE,
                        include_numbers_in_figures = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res3$SummaryPlot)), "try-error"))
  expect_message2( #util_margins_lm
    res3b <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, sort_group_var_levels = FALSE,
                         include_numbers_in_figures = FALSE,
                         n_violin_max = 3)
  )
  expect_false(
    inherits(try(ggplot_build(res3b$SummaryPlot)), "try-error"))

  skip_if_not_installed("ordinal")

  expect_message2( #util_margins_ord
    res3c <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, cut_off_linear_model_for_ord = NULL)
  )
  expect_false(
    inherits(try(ggplot_build(res3c$SummaryPlot)), "try-error"))
  expect_message2( #util_margins_ord
    res3d <- acc_margins(resp_vars = "MEAT_CONS_0", study_data = study_data,
                         meta_data = md, group_vars = "CENTER_0",
                         label_col = LABEL, cut_off_linear_model_for_ord = NULL,
                         sort_group_var_levels = FALSE)
  )
  expect_false(
    inherits(try(ggplot_build(res3d$SummaryPlot)), "try-error"))

  # 3. Poisson model
  md$SCALE_LEVEL[md$LABEL == "N_INJURIES_0"] <- SCALE_LEVELS$INTERVAL
  expect_message2( #util_margins_poi
    res4 <- acc_margins(resp_vars = "N_INJURIES_0", study_data = study_data,
                        meta_data = md, group_vars = "USR_SOCDEM_0",
                        label_col = LABEL, sort_group_var_levels = TRUE,
                        include_numbers_in_figures = FALSE)
  )
  expect_false(
    inherits(try(ggplot_build(res4$SummaryPlot)), "try-error"))
  expect_message2( #util_margins_poi
    res4b <- acc_margins(resp_vars = "N_INJURIES_0", study_data = study_data,
                         meta_data = md, group_vars = "USR_SOCDEM_0",
                         label_col = LABEL, sort_group_var_levels = FALSE,
                         include_numbers_in_figures = TRUE)
  )
  expect_false(
    inherits(try(ggplot_build(res4b$SummaryPlot)), "try-error"))

  expect_message2( #util_margins_poi
    res4b_new <- acc_margins(resp_vars = "N_INJURIES_0", study_data = study_data,
                         meta_data = md, group_vars = "USR_SOCDEM_0",
                         label_col = LABEL, sort_group_var_levels = FALSE,
                         include_numbers_in_figures = TRUE,
                         threshold_type = "none")
  )

  expect_error(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00001"),
    regexp = "Argument .group_vars.+Variable .v00001.+na. does not have an allowed scale level",
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_margins(resp_vars = "v00014", study_data = study_data,
                  meta_data = meta_data, group_vars = "v00016",
                  min_obs_in_subgroup = 2, sort_group_var_levels = FALSE,
                  n_violin_max = 3),
    regexp =
      paste("min_obs_in_subgroup is not specified correctly",
            "and is set to 5 instead.")
  )

  expect_message2(
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

  expect_identical(colnames(res1$SummaryTable), c("Variables"))
})

test_that("acc_margins works with label_col", {
  skip_on_cran() # slow

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
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

expect_message2(
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

expect_message2(
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

expect_message2(
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

expect_message2(
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

expect_message2(
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

  expect_identical(colnames(res1$SummaryTable), c("Variables"))

  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # TODO: As soon as vdiffr supports the variant argument for snapshots -- https://github.com/r-lib/vdiffr/issues/125, use it everywhere.
  # r_minor_version <- function() paste0("R", getRversion()[, 1:2])
  # ggplot_version <- function() packageVersion("ggplot2")

  # TODO: skip_if_not(capabilities()["long.double"])
  expect_doppelganger2("margins plot for CRP_0 ok",
                              res1$SummaryPlot)
})

test_that("acc_margins works with co_vars", {
  skip_on_cran() # slow

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  meta_data <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)

  # 1. linear regression
  # example 1: obvious differences between examiners
  set.seed(1352)
  sd1 <- study_data
  sd1[["v00012"]] <- rep(c("USR_301", "USR_243", "USR_121"), 1000)
  sd1[["v00004"]][which(sd1[["v00012"]] == "USR_301")] <- 10
  sd1[["v00004"]][which(sd1[["v00012"]] == "USR_243")] <- 0
  sd1[["v00004"]][which(sd1[["v00012"]] == "USR_121")] <- -10
  sd1[["v00004"]] <- round(sd1[["v00004"]] + 70 +
                             5 * sd1[["v00002"]] +
                             0.5 * sd1[["v00003"]] + rnorm(n = nrow(sd1)))
  md1 <- meta_data
  md1[["HARD_LIMITS"]][md1[[VAR_NAMES]] == "v00004"] <- ""

  suppressMessages(
    res1_lm_unadj <- acc_margins(resp_vars = "SBP_0", # v00004
                                 group_vars = "USR_BP_0", # v00012
                                 co_vars = NULL,
                                 study_data = sd1,
                                 meta_data = md1,
                                 label_col = LABEL,
                                 sort_group_var_levels = FALSE)
  )
  # compare estimates from the model with empirical mean values
  mu_emp <- tapply(sd1[["v00004"]], sd1[["v00012"]], mean, na.rm = TRUE)
  mu_emp <- mu_emp[match(res1_lm_unadj$ResultData$USR_BP_0, names(mu_emp))]
  expect_lte(max(abs(as.numeric(res1_lm_unadj$ResultData$margins) - mu_emp)),
             0.01)
  suppressMessages(
    res1_lm <- acc_margins(resp_vars = "SBP_0",
                           group_vars = "USR_BP_0",
                           co_vars = c("SEX_0", "AGE_0"),
                           study_data = sd1,
                           meta_data = md1,
                           label_col = LABEL,
                           sort_group_var_levels = FALSE)
  )
  # marginal means should be almost the same in both approaches
  expect_lte(
    max(abs(
      as.numeric(res1_lm_unadj$ResultData$margins) -
        as.numeric(res1_lm$ResultData$margins)
      )), 2)
  # standard errors should be much smaller when adjusting for the covariates in
  # this example
  expect_true(all(
    as.numeric(res1_lm_unadj$ResultData$SE) >
      as.numeric(res1_lm$ResultData$SE)
  ))

  # example 2: no differences between examiners after adjusting for covariates
  set.seed(1352)
  sd1 <- study_data
  age_cut3 <- as.numeric(cut(sd1[["v00003"]], breaks = 3))
  age_cut3[which(is.na(age_cut3))] <- 1
  sd1[["v00012"]] <- c("USR_301", "USR_243", "USR_121")[age_cut3]
  sd1[["v00002"]][which(sd1[["v00012"]] == "USR_301")] <- 0
  sd1[["v00002"]][which(sd1[["v00012"]] == "USR_243")] <- 1
  sd1[["v00002"]][which(sd1[["v00012"]] == "USR_121")] <-
    sample(c(0,1), replace = TRUE,
           size = length(which(sd1[["v00012"]] == "USR_121")))
  sd1[["v00004"]] <- round(70 + 20 * sd1[["v00002"]] + 0.2 * sd1[["v00003"]] +
                             rnorm(sd = 3, n = nrow(sd1)))
  md1 <- meta_data
  md1[["HARD_LIMITS"]][md1[[VAR_NAMES]] == "v00004"] <- ""
  suppressMessages(
    res2_lm_unadj <- acc_margins(resp_vars = "SBP_0", # v00004
                                 group_vars = "USR_BP_0", # v00012
                                 co_vars = NULL,
                                 study_data = sd1,
                                 meta_data = md1,
                                 label_col = LABEL,
                                 sort_group_var_levels = FALSE)
  )
  res2u_mar <- as.numeric(res2_lm_unadj$ResultData$margins)
  # compare estimates from the model with empirical mean values
  mu_emp <- tapply(sd1[["v00004"]], sd1[["v00012"]], mean, na.rm = TRUE)
  mu_emp <- mu_emp[match(res2_lm_unadj$ResultData$USR_BP_0, names(mu_emp))]
  expect_lte(max(abs(res2u_mar - mu_emp)), 0.01)
  # the estimated marginal means should deviate considerably
  expect_gte(max(res2u_mar) - min(res2u_mar), 15)
  suppressMessages(
    res2_lm <- acc_margins(resp_vars = "SBP_0",
                           group_vars = "USR_BP_0",
                           co_vars = c("SEX_0", "AGE_0"),
                           study_data = sd1,
                           meta_data = md1,
                           label_col = LABEL,
                           sort_group_var_levels = FALSE)
  )
  res2a_mar <- as.numeric(res2_lm$ResultData$margins)
  # the estimated marginal means should deviate only minimally
  expect_lte(max(res2a_mar) - min(res2a_mar), 2)
  # there should be levels of the grouping variable that deviate considerably
  # between both approaches, and one level of the grouping variable that
  # deviates only slightly
  expect_gte(max(abs(res2u_mar - res2a_mar)), 10)
  expect_lte(min(abs(res2u_mar - res2a_mar)), 2)

  # 2. binary regression
  # no differences between examiners after adjusting for covariates
  set.seed(603)
  sd1 <- study_data
  age_cut3 <- as.numeric(cut(sd1[["v00003"]], breaks = 3))
  age_cut3[which(is.na(age_cut3))] <- 1
  sd1[["v00011"]] <- c("USR_321", "USR_590", "USR_213")[age_cut3]
  sd1[["v00002"]][which(sd1[["v00011"]] == "USR_321")] <- 0
  sd1[["v00002"]][which(sd1[["v00011"]] == "USR_590")] <- 1
  sd1[["v00002"]][which(sd1[["v00011"]] == "USR_213")] <-
    sample(c(0,1), replace = TRUE,
           size = length(which(sd1[["v00011"]] == "USR_213")))
  mlin <- log(0.3) + 0.25 * sd1[["v00002"]] +
    2 * (sd1[["v00003"]] - mean(sd1[["v00003"]], na.rm = TRUE)) /
    sd(sd1[["v00003"]], na.rm = TRUE)
  mlin[which(is.na(mlin))] <- mean(mlin, na.rm = TRUE)
  sd1[["v00007"]] <- rbinom(n = nrow(sd1), size = 1, prob = 1/(1 + exp(-mlin)))
  md1 <- meta_data
  md1[["RECODE_CASES"]] <- ""
  md1[["RECODE_CONTROL"]] <- ""
  md1[["RECODE_CASES"]][md1$VAR_NAMES == "v00007"] <- "yes"
  md1[["RECODE_CONTROL"]][md1$VAR_NAMES == "v00007"] <- "no"

  suppressMessages(
    res3_bin_unadj <- acc_margins(resp_vars = "ASTHMA_0",
                                  study_data = sd1,
                                  meta_data = md1,
                                  group_vars = "USR_VO2_0",
                                  co_vars = NULL,
                                  label_col = LABEL)
  )
  res3u_mar <- as.numeric(res3_bin_unadj$ResultData$margins)
  # compare estimates from the model with empirical mean values
  mu_emp <- tapply(sd1[["v00007"]], sd1[["v00011"]], mean, na.rm = TRUE)
  mu_emp <- mu_emp[match(res3_bin_unadj$ResultData$USR_VO2_0, names(mu_emp))]
  expect_lte(max(abs(res3u_mar - mu_emp)), 0.01)
  # the estimated marginal means should deviate considerably
  expect_gte(max(res3u_mar) - min(res3u_mar), 0.5)
  suppressMessages(
    res3_bin <- acc_margins(resp_vars = "ASTHMA_0",
                            study_data = sd1,
                            meta_data = md1,
                            group_vars = "USR_VO2_0",
                            co_vars = c("SEX_0", "AGE_0"),
                            label_col = LABEL)
  )
  res3a_mar <- as.numeric(res3_bin$ResultData$margins)
  # the estimated marginal means should deviate only minimally
  expect_lte(max(res3a_mar) - min(res3a_mar), 0.2)
  # there should be levels of the grouping variable that deviate considerably
  # between both approaches, and one level of the grouping variable that
  # deviates only slightly
  expect_gte(max(abs(res3u_mar - res3a_mar)), 0.5)
  expect_lte(min(abs(res3u_mar - res3a_mar)), 0.2)

  # 3. Poisson regression
  # example 1: obvious differences between examiners
  set.seed(1646)
  sd1 <- study_data
  parm_age <- (sd1[["v00003"]] - min(sd1[["v00003"]], na.rm = TRUE)) /
    (max(sd1[["v00003"]], na.rm = TRUE) - min(sd1[["v00003"]], na.rm = TRUE))
  parm_sex <- 0.4 * sd1[["v00002"]]
  sd1[["v00032"]] <- rep(c("USR_321", "USR_247", "USR_520"), 1000)
  sd1[["v00026"]][which(sd1[["v00032"]] == "USR_321")] <- 1
  sd1[["v00026"]][which(sd1[["v00032"]] == "USR_247")] <- 0.5
  sd1[["v00026"]][which(sd1[["v00032"]] == "USR_520")] <- 0
  ll1 <- exp(sd1[["v00026"]] + parm_age + parm_sex)
  ll1[which(is.na(ll1))] <- 1
  sd1[["v00026"]] <- rpois(lambda = ll1, n = nrow(sd1))
  # scale down to trigger Poisson regression (v2.5.0)
  if (max(sd1[["v00026"]], na.rm = TRUE) >= 20) {
    sd1[["v00026"]] <- round(sd1[["v00026"]]/max(sd1[["v00026"]]) * 19)
  }
  md1 <- meta_data
  md1[["HARD_LIMITS"]][md1[[VAR_NAMES]] == "v00026"] <- ""
  md1$SCALE_LEVEL[md1$LABEL == "N_INJURIES_0"] <- SCALE_LEVELS$INTERVAL

  suppressMessages(
    res5_poi_unadj <- acc_margins(resp_vars = "N_INJURIES_0",
                                  group_vars = "USR_SOCDEM_0",
                                  co_vars = NULL,
                                  study_data = sd1,
                                  meta_data = md1,
                                  label_col = LABEL,
                                  sort_group_var_levels = FALSE)
  )
  # compare estimates from the model with empirical mean values
  mu_emp <- tapply(sd1[["v00026"]], sd1[["v00032"]], mean, na.rm = TRUE)
  mu_emp <- mu_emp[match(res5_poi_unadj$ResultData$USR_SOCDEM_0, names(mu_emp))]
  expect_lte(max(abs(as.numeric(res5_poi_unadj$ResultData$margins) - mu_emp)),
             0.01)
  suppressMessages(
    res5_poi <- acc_margins(resp_vars = "N_INJURIES_0",
                            group_vars = "USR_SOCDEM_0",
                            co_vars = c("SEX_0", "AGE_0"),
                            study_data = sd1,
                            meta_data = md1,
                            label_col = LABEL,
                            sort_group_var_levels = FALSE)
  )

  # marginal means should be almost the same in both approaches
  expect_lte(
    max(abs(
      as.numeric(res5_poi_unadj$ResultData$margins) -
        as.numeric(res5_poi$ResultData$margins)
    )), 2)

  # example 2: no differences between examiners after adjusting for covariates
  set.seed(2128)
  sd1 <- study_data
  age_cut3 <- as.numeric(cut(sd1[["v00003"]], breaks = 3))
  age_cut3[which(is.na(age_cut3))] <- 1
  sd1[["v00032"]] <- c("USR_321", "USR_247", "USR_520")[age_cut3]
  sd1[["v00002"]][which(sd1[["v00032"]] == "USR_321")] <- 0
  sd1[["v00002"]][which(sd1[["v00032"]] == "USR_520")] <- 1
  sd1[["v00002"]][which(sd1[["v00032"]] == "USR_247")] <-
    sample(c(0,1), replace = TRUE,
           size = length(which(sd1[["v00032"]] == "USR_247")))
  ll1 <- exp(parm_age + 0.5 * sd1[["v00002"]])
  ll1[which(is.na(ll1))] <- 1
  sd1[["v00026"]] <- rpois(lambda = ll1, n = nrow(sd1))
  md1 <- meta_data
  md1[["HARD_LIMITS"]][md1[[VAR_NAMES]] == "v00026"] <- ""
  md1$SCALE_LEVEL[md1$LABEL == "N_INJURIES_0"] <- SCALE_LEVELS$INTERVAL

  suppressMessages(
    res6_poi_unadj <- acc_margins(resp_vars = "N_INJURIES_0",
                                  group_vars = "USR_SOCDEM_0",
                                  co_vars = NULL,
                                  study_data = sd1,
                                  meta_data = md1,
                                  label_col = LABEL,
                                  sort_group_var_levels = FALSE)
  )
  # compare estimates from the model with empirical mean values
  mu_emp <- tapply(sd1[["v00026"]], sd1[["v00032"]], mean, na.rm = TRUE)
  mu_emp <- mu_emp[match(res6_poi_unadj$ResultData$USR_SOCDEM_0, names(mu_emp))]
  expect_lte(max(abs(as.numeric(res6_poi_unadj$ResultData$margins) - mu_emp)),
             0.01)
  suppressMessages(
    res6_poi <- acc_margins(resp_vars = "N_INJURIES_0",
                            group_vars = "USR_SOCDEM_0",
                            co_vars = c("SEX_0", "AGE_0"),
                            study_data = sd1,
                            meta_data = md1,
                            label_col = LABEL,
                            sort_group_var_levels = FALSE)
  )
  res6u_mar <- as.numeric(res6_poi_unadj$ResultData$margins)
  res6a_mar <- as.numeric(res6_poi$ResultData$margins)
  # the estimated marginal means in the adjusted model should deviate
  # less than in the unadjusted model
  expect_gte(var(res6u_mar), var(res6a_mar))
  # the estimated marginal means in the adjusted model should deviate
  # only minimally
  expect_lte(max(res6a_mar) - min(res6a_mar), 0.5)
  # there should be levels of the grouping variable that deviate considerably
  # between both approaches, and one level of the grouping variable that
  # deviates only slightly
  expect_gte(max(abs(res6u_mar - res6a_mar)), 1)
  expect_lte(min(abs(res6u_mar - res6a_mar)), 0.5)

  # 4. ordinal regression
  # TODO
  md1 <- meta_data
  md1$SCALE_LEVEL[md1$LABEL == "MEAT_CONS_0"] <- SCALE_LEVELS$ORDINAL
  skip_if_not_installed("ordinal")
  expect_error(
    res8_ord <- acc_margins(resp_vars = "MEAT_CONS_0",
                            group_vars = "CENTER_0",
                            co_vars = "AGE_0",
                            study_data = study_data,
                            meta_data = md1,
                            label_col = LABEL,
                            cut_off_linear_model_for_ord = NULL,
                            sort_group_var_levels = FALSE),
    regexp = paste("Covariate argument for ordinal regression",
                   "not yet supported.")
  )

  # 5. multinomial regression
  set.seed(940)
  sd1 <- study_data
  age_cut3 <- as.numeric(cut(sd1[["v00003"]], breaks = 3))
  age_cut3[which(is.na(age_cut3))] <- 1
  sd1[["v00000"]] <- c(1:3)[age_cut3]
  sd1[["v00002"]][which(sd1[["v00002"]] == 1)] <-
    sample(c(0,1), replace = TRUE,
           size = length(which(sd1[["v00002"]] == 1))) #0
  sd1[["v00002"]][which(sd1[["v00002"]] == 2)] <-
    sample(c(0,1), replace = TRUE,
           size = length(which(sd1[["v00002"]] == 2)))#1
  sd1[["v00002"]][which(sd1[["v00002"]] == 3)] <-
    sample(c(0,1), replace = TRUE,
           size = length(which(sd1[["v00002"]] == 3)))
  xx <- #0.25 * sd1[["v00002"]] +
    #0.75 *
    (rank(sd1[["v00003"]], na.last = FALSE) - 1)/nrow(sd1)
  xx[which(is.na(xx))] <- mean(xx, na.rm = TRUE)
  xx <- (xx - min(xx))/(max(xx) - min(xx))
  sd1[["v00023"]] <- round(xx * 4)
  md1 <- meta_data
  md1[["HARD_LIMITS"]][md1[[VAR_NAMES]] == "v00023"] <- ""
  md1$SCALE_LEVEL[md1$LABEL == "MEAT_CONS_0"] <- SCALE_LEVELS$NOMINAL

  res10_cat_unadj <- acc_margins(resp_vars = "MEAT_CONS_0",
                                 group_vars = "CENTER_0",
                                 study_data = sd1,
                                 meta_data = md1,
                                 label_col = LABEL,
                                 dichotomize_categorical_resp = FALSE)

  expect_error(
    res10_cat <- acc_margins(resp_vars = "MEAT_CONS_0",
                             group_vars = "CENTER_0",
                             co_vars = c("AGE_0", "SEX_0"),
                             study_data = sd1,
                             meta_data = md1,
                             label_col = LABEL,
                             dichotomize_categorical_resp = FALSE),
    regexp = paste("Adjusting for covariates is currently",
                   "only supported for factor variables",
                   "with 2 or more levels")
  )
  # TODO
  res10_cat <- acc_margins(resp_vars = "MEAT_CONS_0",
                                 group_vars = "CENTER_0",
                           co_vars = "AGE_GROUP_0",
                                 study_data = sd1,
                                 meta_data = md1,
                                 label_col = LABEL,
                                 dichotomize_categorical_resp = FALSE)

})
