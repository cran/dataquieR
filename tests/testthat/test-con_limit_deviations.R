test_that("con_limit_deviations works", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]

  expect_message(
    MyValueLimits <- con_limit_deviations(resp_vars  = c("AGE_0", "SBP_0",
                                                         "DBP_0", "SEX_0",
                                                         "QUEST_DT_0",
                                                         "EDUCATION_1",
                                                         "SMOKE_SHOP_0"),
                                          label_col  = "LABEL",
                                          study_data = study_data,
                                          meta_data  = meta_data),
    regexp = "No limits specified for SEX_0")

   expect_equal(
     unname(rowSums(MyValueLimits$SummaryTable[
       order(MyValueLimits$SummaryTable$Variables),
       c("NUM_con_rvv_inum", "NUM_con_rvv_itdat")], na.rm = TRUE)),
     MyValueLimits$SummaryData %>%
       dplyr::filter(Section != "within" & Limits == "HARD_LIMITS") %>%
       dplyr::group_by(Variables) %>%
       dplyr::summarise(n = sum(Number)) %>%
       dplyr::pull(n)
   )
})

test_that("con_limit_deviations and timevars with < 20 integer sec-values ok", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


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
  # TODO: skip_if_not(capabilities()["long.double"])
  # TODO: skip_if_not(capabilities()["long.double"])
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


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


  expect_message(
    MyValueLimits <- con_limit_deviations(label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS",
                                          return_flagged_study_data = TRUE),
    regexp = sprintf("(%s|%s)",
                     paste("All variables for which limits are specified",
                           "in the metadata are used."),
                     paste("Not all entries in.+KEY_STUDY_SEGMENT.+are",
                           "found in.+VAR_NAMES.+")
    ),
    all = TRUE,
    perl = TRUE
  )

  expect_equal(sum(rowSums(
    MyValueLimits$SummaryTable[,
      grepl("^FLG\\_.*$", colnames(MyValueLimits$SummaryTable))],
    na.rm = TRUE) > 0), 4)
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


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
  # no limits:
  md0$HARD_LIMITS <- NA
  md0$DETECTION_LIMITS <- NA
  md0$SOFT_LIMITS <- NA

  expect_message(expect_error(
    MyValueLimits <- con_limit_deviations(resp_vars = resp_vars,
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0),
    regexp = paste("The limit deviation check cannot be performed",
                   "without a metadata column specifying suitable limits")
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
                             meta_data  = md0)),
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


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
  # TODO: skip_if_not(capabilities()["long.double"])
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


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
  # TODO: skip_if_not(capabilities()["long.double"])
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  sd0 <- study_data
  x <- sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]]

  x[2:5] <- -2

  sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]] <- x
  md0 <- meta_data

  expect_silent(
    MyValueLimits1 <- con_limit_deviations(resp_vars  = "EDUCATION_0",
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS")
  )

  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <- "[;6]"

  MyValueLimits2 <- con_limit_deviations(resp_vars  = "EDUCATION_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS")

  expect_equal(MyValueLimits1$SummaryTable$NUM_con_rvv_inum, 4)
  expect_equal(MyValueLimits2$SummaryTable$NUM_con_rvv_inum, 0)

  expect_equal(MyValueLimits1$SummaryData$Number[
    which(MyValueLimits1$SummaryData$Section == "above")], MyValueLimits2$SummaryData$Number[
      which(MyValueLimits2$SummaryData$Section == "above")])
})

test_that("con_limit_deviations with constant data", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  sd0 <- study_data
  sd0[[prep_map_labels("CRP_0", meta_data, VAR_NAMES, LABEL)]] <- 1.1

  md0 <- meta_data

  expect_silent(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "CRP_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS"))

  expect_type(MyValueLimits, "list")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  suppressWarnings(
    vdiffr::expect_doppelganger(
      "con_limit_deviations 4 hists 1 val",
      MyValueLimits$SummaryPlotList$CRP_0)
  )

  md0[which(md0$LABEL == "CRP_0"), HARD_LIMITS] <- NA
  md0[which(md0$LABEL == "CRP_0"), DETECTION_LIMITS] <- NA
  md0[which(md0$LABEL == "CRP_0"), SOFT_LIMITS] <- NA
  md0[["NEW_LIMITS_ONE"]] <- NA
  md0[which(md0$LABEL == "CRP_0"), "NEW_LIMITS_ONE"] <- "(-Inf; 1.1)"
  md0[["NEW_LIMITS_TWO"]] <- NA
  md0[which(md0$LABEL == "CRP_0"), "NEW_LIMITS_TWO"] <- "(1.1; Inf)"

  expect_silent(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "CRP_0",
                                           label_col  = "LABEL",
                                           study_data = sd0,
                                           meta_data  = md0,
                                           limits     = c("NEW_LIMITS_ONE", "NEW_LIMITS_TWO")))
})

test_that("con_limit_deviations does not crash with missing codes", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  sd0 <- study_data
  x <- sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]]

  x[[5]] <- 99999 # add a "forgotten missing code" to the data, which causes
  # too many breaks, that should be removed to avoid a crash

  sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]] <- x
  md0 <- meta_data

  expect_silent(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "SBP_0",
                                          label_col  = "LABEL",
                                          study_data = sd0,
                                          meta_data  = md0,
                                          limits     = "HARD_LIMITS")
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryData$Number[
    which(MyValueLimits$SummaryData$Section == "above" &
            MyValueLimits$SummaryData$Limits == "HARD_LIMITS")], 1)
  expect_equal(MyValueLimits$SummaryData$Number[
    which(MyValueLimits$SummaryData$Section == "below" &
            MyValueLimits$SummaryData$Limits == "HARD_LIMITS")], 0)

  sd0 <- study_data
  x <- sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]]

  x[[5]] <- 100000 # add a less obvious "forgotten missing code" to the data,
  # which causes too many breaks, that should be removed to avoid a crash

  sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]] <- x

  expect_silent(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "SBP_0",
                                           label_col  = "LABEL",
                                           study_data = sd0,
                                           meta_data  = md0,
                                           limits     = "HARD_LIMITS")
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryData$Number[
    which(MyValueLimits$SummaryData$Section == "above" &
            MyValueLimits$SummaryData$Limits == "HARD_LIMITS")], 1)
  expect_equal(MyValueLimits$SummaryData$Number[
    which(MyValueLimits$SummaryData$Section == "below" &
            MyValueLimits$SummaryData$Limits == "HARD_LIMITS")], 0)

  skip_on_cran()
  # TODO: skip_if_not(capabilities()["long.double"])
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  md0 <- meta_data
  md0[["HARD_LIMITS"]][which(md0$LABEL == "BSG_0")] <- "[0;5000]"
  sd0 <- study_data
  x <- sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]]
  x[c(2,8,92)] <- c(26027, 28097, 4031012019)
  sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]] <- x
  expect_silent(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "BSG_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS")
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryTable$NUM_con_rvv_inum, 3)
})

test_that("con_limits_deviations does not crash with NAs in datetime vars", {
  skip_on_cran() # slow, errors obvious
  # issue 106
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  #sd0 <- study_data
  #x <- sd0[[prep_map_labels("QUEST_DT_0", meta_data, VAR_NAMES, LABEL)]]
  #any(is.na(x))
  #length(which(is.na(x)))
  # Variable QUEST_DT_0 already contains NA values
  expect_silent(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "QUEST_DT_0",
                                         label_col  = "LABEL",
                                         study_data = study_data,
                                         meta_data  = meta_data,
                                         limits     = "HARD_LIMITS")
  )

  expect_type(MyValueLimits, "list")
  expect_equal(MyValueLimits$SummaryTable$NUM_con_rvv_itdat, 9)
})

test_that("con_limit_deviations works with no values within limits", {
  skip_on_cran() # slow, errors obvious
  skip_if_not_installed("withr")
  withr::local_timezone("CET")

  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  md0 <- meta_data
  md0[["HARD_LIMITS"]][which(md0$LABEL == "BSG_0")] <- "[51.5;51.8]"
  sd0 <- study_data

  expect_silent(
    MyValueLimits  <- con_limit_deviations(resp_vars  = "BSG_0",
                                         label_col  = "LABEL",
                                         study_data = sd0,
                                         meta_data  = md0,
                                         limits     = "HARD_LIMITS")
  )

  expect_type(MyValueLimits, "list")
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
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
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  md0 <- meta_data
  md0[["HARD_LIMITS"]][which(md0$LABEL == "QUEST_DT_0")] <- "[51.5;51.8]"
  sd0 <- study_data

  expect_warning(
      MyValueLimits <- con_limit_deviations(resp_vars  = "QUEST_DT_0",
                                            label_col  = "LABEL",
                                            study_data = sd0,
                                            meta_data  = md0,
                                            limits     = "HARD_LIMITS"),
  regexp = "Invalid limits detected")

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
