test_that("con_limit_deviations works", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())
  expect_warning(
    MyValueLimits <- con_limit_deviations(resp_vars  = c("AGE_0", "SBP_0",
                                                         "DBP_0", "SEX_0",
                                                         "QUEST_DT_0",
                                                         "EDUCATION_1",
                                                         "SMOKE_SHOP_0"),
                                          label_col  = "LABEL",
                                          study_data = study_data,
                                          meta_data  = meta_data,
                                          limits     = "HARD_LIMITS"),
    regexp = sprintf("(%s|%s|%s|%s)",
                     paste("The variables SEX_0 have no defined limits."),
                     paste("N = 9 values in QUEST_DT_0 have",
                           "been below HARD_LIMIT_LOW and were removed."),
                     paste("N = 3 values in EDUCATION_1 have been above",
                           "HARD_LIMIT_UP and were removed."),
                     paste("N = 24 values in SMOKE_SHOP_0 have been above",
                           "HARD_LIMIT_UP and were removed.")
                     ),
    all = TRUE,
    perl = TRUE
  )

  expect_equal(sum(MyValueLimits$SummaryTable$GRADING),
               3)
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] <
      as.POSIXct("2018-01-01")))
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(!as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] >=
      as.POSIXct("2018-01-01")))
})

test_that("con_limit_deviations and timevars with < 20 integer sec-values ok", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  vn <- subset(meta_data, LABEL == "QUEST_DT_0", VAR_NAMES, TRUE)

  sd0 <- study_data[
    study_data[[vn]] >= as.POSIXct("2018-05-01") &
      study_data[[vn]] < as.POSIXct("2018-05-15"), , FALSE]

  sd0[[vn]] <- round.POSIXt(sd0[[vn]], "days")

  expect_lt(length(unique(as.character(sd0[[vn]]))), 20)

  MyValueLimits <- con_limit_deviations(resp_vars  = "QUEST_DT_0",
                                        label_col  = "LABEL",
                                        study_data = sd0,
                                        meta_data  = meta_data,
                                        limits     = "HARD_LIMITS")


  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  skip_if_not(capabilities()["long.double"])
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations QUEST_DT_0",
                                MyValueLimits$SummaryPlotList$QUEST_DT_0)
  )

})

test_that("con_limit_deviations works w/o resp_vars", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  resp_vars <- c("AGE_0", "SBP_0",
                 "DBP_0", "SEX_0",
                 "QUEST_DT_0",
                 "EDUCATION_1",
                 "SMOKE_SHOP_0")

  resp_vars_names <- prep_map_labels(resp_vars,
                                     meta_data,
                                     from = LABEL,
                                     to = VAR_NAMES)

  sd0 <- study_data[, resp_vars_names, FALSE]
  md0 <- meta_data[meta_data$VAR_NAMES %in% resp_vars_names, , FALSE]
  md0$JUMP_LIST <- SPLIT_CHAR # signals unambiguously no codes intentionally


  expect_warning(
    MyValueLimits <- con_limit_deviations(label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS"),
    regexp = sprintf("(%s|%s|%s|%s)",
                     paste("All variables with HARD_LIMITS in",
                           "the metadata are used."),
                     paste("N = 9 values in QUEST_DT_0 have",
                           "been below HARD_LIMIT_LOW and were removed."),
                     paste("N = 3 values in EDUCATION_1 have been above",
                           "HARD_LIMIT_UP and were removed."),
                     paste("N = 24 values in SMOKE_SHOP_0 have been above",
                           "HARD_LIMIT_UP and were removed.")
    ),
    all = TRUE,
    perl = TRUE
  )

  expect_equal(sum(MyValueLimits$SummaryTable$GRADING),
               3)
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] <
      as.POSIXct("2018-01-01")))
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(!as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] >=
      as.POSIXct("2018-01-01")))
})

test_that("con_limit_deviations handles errors", {
  Sys.setenv(TZ = 'CET')

  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  resp_vars <- c("AGE_0", "SBP_0",
                 "DBP_0", "SEX_0",
                 "QUEST_DT_0",
                 "EDUCATION_1",
                 "SMOKE_SHOP_0")

  resp_vars_names <- prep_map_labels(resp_vars,
                                     meta_data,
                                     from = LABEL,
                                     to = VAR_NAMES)

  sd0 <- study_data[, resp_vars_names, FALSE]
  md0 <- meta_data[meta_data$VAR_NAMES %in% resp_vars_names, , FALSE]
  md0$JUMP_LIST <- SPLIT_CHAR # signals unambiguously no codes intentionally

  md0$HARD_LIMITS <- NA # no limits

  expect_error(
    MyValueLimits <- con_limit_deviations(label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS"),
    regexp = "No Variables with defined HARD_LIMITS.",
    perl = TRUE
  )

  expect_error(
    MyValueLimits <- con_limit_deviations(resp_vars = resp_vars,
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS"),
    regexp = "No Variables with defined HARD_LIMITS.",
    perl = TRUE
  )

  resp_vars <- meta_data$LABEL

  resp_vars_names <- prep_map_labels(resp_vars,
                                     meta_data,
                                     from = LABEL,
                                     to = VAR_NAMES)

  sd0 <- study_data[, resp_vars_names, FALSE]
  md0 <- meta_data[meta_data$VAR_NAMES %in% resp_vars_names, , FALSE]
  md0$JUMP_LIST <- SPLIT_CHAR # signals unambiguously no codes intentionally

  md0$HARD_LIMITS[md0$DATA_TYPE == DATA_TYPES$STRING] <-
    "[1; 5]" # Some unsuitable limits

  expect_warning(
      MyValueLimits <- con_limit_deviations(label_col  = "LABEL",
                                            study_data = sd0,
                                            meta_data  = md0,
                                            limits     = "HARD_LIMITS"),
    regexp = sprintf("(%s|%s|%s)",
                     paste("Variables PSEUDO_ID, AGE_GROUP_0,",
                           "VO2_CAPCAT_0, USR_VO2_0, USR_BP_0, USR_SOCDEM_0",
                           "are neither numeric nor datetime and will be",
                           "removed from analyses."),
                     paste("N = (3|24|1066|349|9) values in",
                "(EDUCATION_1|SMOKE_SHOP_0|PREGNANT_0|MEDICATION_0|QUEST_DT_0)",
                "have been (above|below)",
                "HARD_LIMIT_(UP|LOW) and were removed."),
                     paste("All variables with HARD_LIMITS in the",
                           "metadata are used.")
    ),
    all = TRUE,
    perl = TRUE
  )

  expect_equal(
    sum(MyValueLimits$SummaryTable$GRADING), 5
  )

  md0$HARD_LIMITS <- NA # no limits
  md0$HARD_LIMITS[md0$DATA_TYPE == DATA_TYPES$STRING] <-
    "[1; 5]" # Only unsuitable limits

  expect_warning(
    expect_error(
      MyValueLimits <- con_limit_deviations(label_col  = "LABEL",
                                            study_data = sd0,
                                            meta_data  = md0,
                                            limits     = "HARD_LIMITS"),
      regexp = "No variables left, no limit checks possible.",
      perl = FALSE
    ),
    regexp = sprintf("(%s|%s)",
                     paste("Variables PSEUDO_ID, AGE_GROUP_0,",
                           "VO2_CAPCAT_0, USR_VO2_0, USR_BP_0, USR_SOCDEM_0",
                           "are neither numeric nor datetime and will be",
                           "removed from analyses."),
                     paste("All variables with HARD_LIMITS in the",
                           "metadata are used.")
    ),
    all = TRUE,
    perl = TRUE
  )

})

test_that("con_limit_deviations and values < 0", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  sd0 <- study_data
  x <- sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]]

  x[!is.na(x) & x < 10] <- 0 - x[!is.na(x) & x < 10]

  sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]] <- x

  md0 <- meta_data
  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <-
    "[-6;0]"

  MyValueLimits <- con_limit_deviations(resp_vars  = "EDUCATION_0",
                                        label_col  = "LABEL",
                                        study_data = sd0,
                                        meta_data  = md0,
                                        limits     = "HARD_LIMITS")

  expect_is(MyValueLimits, "list")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations for reverse order ok",
      MyValueLimits$SummaryPlotList$EDUCATION_0)
  )

})

test_that("con_limit_deviations and values < 0 with max -1", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  sd0 <- study_data
  x <- sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]]

  x[!is.na(x) & x < 10] <- -1 - x[!is.na(x) & x < 10]

  sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]] <- x

  md0 <- meta_data
  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <-
    "[-7;-1]"

  MyValueLimits <- con_limit_deviations(resp_vars  = "EDUCATION_0",
                                        label_col  = "LABEL",
                                        study_data = sd0,
                                        meta_data  = md0,
                                        limits     = "HARD_LIMITS")

  expect_is(MyValueLimits, "list")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations rev order w/ -1 ==> 0 max ok",
      MyValueLimits$SummaryPlotList$EDUCATION_0)
  )

})



test_that("con_limit_deviations with no lower limit", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  sd0 <- study_data
  x <- sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]]

  x[2:5] <- -2

  sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]] <- x
  md0 <- meta_data

  expect_warning(
    MyValueLimits1 <- con_limit_deviations(resp_vars  = "EDUCATION_0",
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS"),
    regexp = paste("N = 4 values in EDUCATION_0 have",
                   "been below HARD_LIMIT_LOW and were removed."),
    all = TRUE,
    perl = TRUE
  )

  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <-
    "[;6]"

  MyValueLimits2 <- con_limit_deviations(resp_vars  = "EDUCATION_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS")

  expect_equal(MyValueLimits1$SummaryTable$`Above HARD (N)`,
               MyValueLimits2$SummaryTable$`Above HARD (N)`)


  expect_equal(MyValueLimits1$SummaryTable$`Below HARD (N)`, 4)
  expect_equal(MyValueLimits2$SummaryTable$`Below HARD (N)`, 0)
})

test_that("con_limit_deviations with constant data", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  sd0 <- study_data
  x <- sd0[[prep_map_labels("CRP_0", meta_data, VAR_NAMES, LABEL)]]

  x <- 1.1 # with 1, even for DATA_TYPE == float variables, barplots are shown
  # However, we want  to test a special edge case for histograms

  sd0[[prep_map_labels("CRP_0", meta_data, VAR_NAMES, LABEL)]] <- x
  md0 <- meta_data

  MyValueLimits  <- con_limit_deviations(resp_vars  = "CRP_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS")

  expect_is(MyValueLimits, "list")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations 4 hists 1 val",
      MyValueLimits$SummaryPlotList$CRP_0)
  )
})

test_that("con_limit_deviations does not crash with missing codes", {
  Sys.setenv(TZ = 'CET')
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())

  sd0 <- study_data
  x <- sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]]

  x[[5]] <- 99999 # add a "forgotten missing code" to the data, which causes
  # too many breaks, that should be removed to avoid a crash

  sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]] <- x
  md0 <- meta_data

  expect_warning(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "SBP_0",
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS"),
    regexp = sprintf("(%s|%s|%s)",
                     paste("For .+SBP_0.+, I have 66505 breaks. Did you",
                           "forget to specify some missing codes .+99999.+",
                           "Will arbitrarily reduce the number of breaks below",
                           "10000 to avoid rendering problems."),
                     paste("For .+SBP_0.+. Will arbitrarily reduced the number",
                           "of breaks to 8315 <= 10000 to avoid rendering",
                           "problems."),
                     paste("N = 1 values in SBP_0 have been above",
                           "HARD_LIMIT_UP and were removed.")
                     ),
    perl = TRUE,
    all = TRUE
  )

  expect_is(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryTable$`Above HARD (N)`, 1)
  expect_equal(MyValueLimits$SummaryTable$`Below HARD (N)`, 0)

  sd0 <- study_data
  x <- sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]]

  x[[5]] <- 100000 # add a less obvious"forgotten missing code" to the data,
  # which causes too many breaks, that should be removed to avoid a crash

  sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]] <- x

  expect_warning(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "SBP_0",
                                           label_col  = "LABEL",
                                           study_data = sd0,
                                           meta_data  = md0,
                                           limits     = "HARD_LIMITS"),
    regexp = sprintf("(%s|%s|%s|%s)",
                     paste("For .+SBP_0.+, I have 66507 breaks. Did you",
                           "forget to specify some missing codes .+1e.05.+",
                           "or .+97.+",
                           "Will arbitrarily reduce the number of breaks below",
                           "10000 to avoid rendering problems."),
                     paste("For .+SBP_0.+. Will arbitrarily reduced the number",
                           "of breaks to 8315 <= 10000 to avoid rendering",
                           "problems."),
                     paste("N = 1 values in SBP_0 have been above",
                           "HARD_LIMIT_UP and were removed."),
                     "Removed 439 rows containing non-finite values .stat_bin.."
    ),
    perl = TRUE,
    all = TRUE
  )

  expect_is(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryTable$`Above HARD (N)`, 1)
  expect_equal(MyValueLimits$SummaryTable$`Below HARD (N)`, 0)

  skip_on_cran()
  skip_if_not(capabilities()["long.double"])
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations histgrms + misscds",
      MyValueLimits$SummaryPlotList$SBP_0)
  )
})
