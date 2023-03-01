test_that("con_limit_deviations works", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
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
                     paste("N = 9 values in QUEST_DT_0 lie outside hard",
                           "limits and were removed."),
                     paste("N = 3 values in EDUCATION_1 lie outside hard",
                           "limits and were removed."),
                     paste("N = 24 values in SMOKE_SHOP_0 lie outside hard",
                           "limits and were removed.")
                     ),
    all = TRUE,
    perl = TRUE
  )

  expect_equal(sum(MyValueLimits$SummaryData$GRADING), 3)
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] <
      as.POSIXct("2018-01-01")))
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(!as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] >=
      as.POSIXct("2018-01-01")))
  expect_equal(rowSums(MyValueLimits$SummaryTable[, c("NUM_con_rvv_inum", "NUM_con_rvv_itdat")], na.rm = TRUE),
               MyValueLimits$SummaryData$`Below hard limits (N)` + MyValueLimits$SummaryData$`Above hard limits (N)`)
})

test_that("con_limit_deviations and timevars with < 20 integer sec-values ok", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")
  withr::local_locale(c(LC_TIME = "en_US.UTF-8"))
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

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
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

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
                     paste("N = 9 values in QUEST_DT_0 lie outside hard",
                           "limits and were removed."),
                     paste("N = 3 values in EDUCATION_1 lie outside hard",
                           "limits and were removed."),
                     paste("N = 24 values in SMOKE_SHOP_0 lie outside hard",
                           "limits and were removed.")
    ),
    all = TRUE,
    perl = TRUE
  )

  expect_equal(sum(MyValueLimits$SummaryData$GRADING),
               3)
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] <
      as.POSIXct("2018-01-01")))
  expect_true(all(MyValueLimits$FlaggedStudyData$QUEST_DT_0[which(!as.logical(
    MyValueLimits$FlaggedStudyData$QUEST_DT_0_below_HARD))] >=
      as.POSIXct("2018-01-01")))
})

test_that("con_limit_deviations handles errors", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

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

  expect_message(expect_error(
    MyValueLimits <- con_limit_deviations(resp_vars = resp_vars,
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS"),
    regexp = "No variables with defined HARD_LIMITS.",
    perl = TRUE
  ),
  regexp = "Not all entries in .+KEY_STUDY_SEGMENT.+0.+7.*")

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

  expect_error(
      MyValueLimits <- suppressWarnings(
        con_limit_deviations(resp_vars = md0$LABEL, # missing, only resp_vars with suitable data type are picked by the function now.
                             label_col  = "LABEL",
                             study_data = sd0,
                             meta_data  = md0,
                             limits     = "HARD_LIMITS")),
      regexp = paste("Variable .+PSEUDO_ID.+ .+string.+ does not have an",
                           "allowed type"),
    perl = TRUE
  )

  sd0 <- study_data
  sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]] <- NA
  expect_error(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "BSG_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = meta_data,
                                         limits     = "HARD_LIMITS"),
    regexp = "Variable .+BSG_0.+ .+resp_vars.+ has only NA observations"
  )
})

test_that("con_limit_deviations and values < 0", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

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

  expect_type(MyValueLimits, "list")

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
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

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

  expect_type(MyValueLimits, "list")

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
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

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
    regexp = paste("N = 4 values in EDUCATION_0 lie outside hard limits",
                   "and were removed."),
    perl = TRUE
  )

  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <- "[;6]"

  MyValueLimits2 <- con_limit_deviations(resp_vars  = "EDUCATION_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS")

  expect_equal(MyValueLimits1$SummaryData$`Above hard limits (N)`,
               MyValueLimits2$SummaryData$`Above hard limits (N)`)

  expect_equal(MyValueLimits1$SummaryData$`Below hard limits (N)`, 4)
  expect_equal(MyValueLimits2$SummaryData$`Below hard limits (N)`, 0)
})

test_that("con_limit_deviations with constant data", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  sd0 <- study_data
  sd0[[prep_map_labels("CRP_0", meta_data, VAR_NAMES, LABEL)]] <- 1.1
  # with 1, even for DATA_TYPE == float variables, barplots are shown
  # However, we want  to test a special edge case for histograms

  md0 <- meta_data

  MyValueLimits  <- con_limit_deviations(resp_vars  = "CRP_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS")

  expect_type(MyValueLimits, "list")

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
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

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
    regexp = sprintf("(%s|%s)",
                     paste("The number of bins in the histogram were reduced",
                           "below \\d+ bins."),
                     paste("N = 1 values in SBP_0 lie outside hard limits",
                           "and were removed.")
                     ),
    perl = TRUE,
    all = TRUE
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryData$`Above hard limits (N)`, 1)
  expect_equal(MyValueLimits$SummaryData$`Below hard limits (N)`, 0)

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
    regexp = sprintf("(%s|%s)",
                     paste("The number of bins in the histogram were reduced",
                           "below \\d+ bins."),
                     paste("N = 1 values in SBP_0 lie outside hard limits",
                           "and were removed.")
    ),
    perl = TRUE,
    all = TRUE
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryData$`Above hard limits (N)`, 1)
  expect_equal(MyValueLimits$SummaryData$`Below hard limits (N)`, 0)

  skip_on_cran()
  skip_if_not(capabilities()["long.double"])
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations histgrms + misscds",
      MyValueLimits$SummaryPlotList$SBP_0)
  )
})

test_that("con_limit_deviations does not crash with strong outliers", {
  skip_on_cran() # slow, errors obvious
  # issue 116
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  md0 <- meta_data
  md0[["HARD_LIMITS"]][which(md0$LABEL == "BSG_0")] <- "[0;5000]"
  sd0 <- study_data
  x <- sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]]
  x[c(2,8,92)] <- c(26027, 28097, 4031012019)
  sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]] <- x
  expect_warning(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "BSG_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS"),

    regexp = sprintf("(%s|%s)",
                     paste("The number of bins in the histogram were reduced",
                           "below \\d+ bins."),
                     paste("N = 3 values in BSG_0 lie outside hard limits and",
                           "were removed.")
    ),
    perl = TRUE,
    all = TRUE
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryData$`Above hard limits (N)`, 3)
  expect_equal(MyValueLimits$SummaryData$`Below hard limits (N)`, 0)
})

test_that("con_limits_deviations does not crash with NAs in datetime vars", {
  skip_on_cran() # slow, errors obvious
  # issue 106
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  #sd0 <- study_data
  #x <- sd0[[prep_map_labels("QUEST_DT_0", meta_data, VAR_NAMES, LABEL)]]
  #any(is.na(x))
  #length(which(is.na(x)))
  # Variable QUEST_DT_0 already contains NA values
  expect_warning(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "QUEST_DT_0",
                                         label_col  = "LABEL",
                                         study_data = study_data,
                                         meta_data  = meta_data,
                                         limits     = "HARD_LIMITS"),
    regexp = paste("N = 9 values in QUEST_DT_0 lie outside hard limits",
                   "and were removed.")
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryData$`Above hard limits (N)`, 0)
  expect_equal(MyValueLimits$SummaryData$`Below hard limits (N)`, 9)
})

test_that("con_limit_deviations works with no values within limits", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  md0 <- meta_data
  md0[["HARD_LIMITS"]][which(md0$LABEL == "BSG_0")] <- "[51.5;51.8]"
  sd0 <- study_data

  expect_warning(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "BSG_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS"),
    regexp = sprintf("(%s|%s)",
                     paste("The number of bins in the histogram were reduced",
                           "below \\d+ bins."),
                     paste("N = 2686 values in BSG_0 lie outside hard limits and",
                           "were removed.")
    ),
    perl = TRUE,
    all = TRUE
  )

  expect_type(MyValueLimits, "list")
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations_nothing_within",
      MyValueLimits$SummaryPlotList$BSG_0)
  )
})

test_that("con_limit_deviations works with wrong datetime limits", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  md0 <- meta_data
  md0[["HARD_LIMITS"]][which(md0$LABEL == "QUEST_DT_0")] <- "[51.5;51.8]"
  sd0 <- study_data

  expect_warning(
    MyValueLimits <- con_limit_deviations(resp_vars  = "QUEST_DT_0",
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS"))

  md0[["HARD_LIMITS"]][which(md0$LABEL == "QUEST_DT_0")] <-
    "[2018-10-01; 2017-10-01]"
  expect_warning(expect_error(
    MyValueLimits <- con_limit_deviations(resp_vars  = "QUEST_DT_0",
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS")))

  md0[["HARD_LIMITS"]][which(md0$LABEL == "QUEST_DT_0")] <-
    "[0; test]"
  expect_warning(expect_error(
    MyValueLimits <- con_limit_deviations(resp_vars  = "QUEST_DT_0",
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS")))
})
