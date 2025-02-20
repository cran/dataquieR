test_that("com_segment_missingness works", {
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
                   dataquieR.ERRORS_WITH_CALLER = FALSE,
                   dataquieR.WARNINGS_WITH_CALLER = FALSE,
                   dataquieR.MESSAGES_WITH_CALLER = FALSE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  expect_message(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                               threshold_value = NA, color_gradient_direction = "above",
                               exclude_roles = VARIABLE_ROLES$PROCESS),
    regexp = sprintf("%s|%s",
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%."))
  )

  expect_message(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = NA, color_gradient_direction = "above"),
    regexp = sprintf("%s|%s|%s",
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%."),
                     paste("Formal exclude_roles is used with default:",
                           "all process variables are not included here."))
  )

  expect_message(
    r <- com_segment_missingness(study_data, meta_data,
                                 threshold_value = NA, color_gradient_direction = "above"),
    regexp = sprintf("%s|%s|%s",
                     paste("Study variables: .+v00010.+,",
                           ".+v00011.+, .+v00012.+,",
                           ".+v00013.+, .+v00016.+,",
                           ".+v00017.+, .+v00032.+,",
                           ".+v00033.+, .+v00042.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%."),
                     paste("Formal exclude_roles is used with default:",
                           "all process variables are not included here."))
  )

  expect_message(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = NA, color_gradient_direction = "above",
                                 strata_vars = "CENTER_0"),
    regexp = sprintf("%s|%s|%s",
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE."),
                     paste("threshold_value should be a single number between",
                           "0 and 100. Invalid value specified,",
                           "setting to 10%."),
                     paste("Formal exclude_roles is used with default:",
                           "all process variables are not included here."))
  )

  meta_data2 <- meta_data
  meta_data2$KEY_STUDY_SEGMENT <- NULL
  meta_data2$STUDY_SEGMENT <- NULL
  expect_error(suppressWarnings(suppressMessages(
    r <- com_segment_missingness(study_data, meta_data2,
                                 threshold_value = 10, color_gradient_direction = "above",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid"))
    )),
    regexp = paste(".*Metadata do not contain",
                   "the column STUDY_SEGMENT"),
    perl = TRUE
  )

  meta_data2 <- meta_data
  meta_data2$LONG_LABEL <- NA
  expect_warning(
    r <- com_segment_missingness(study_data, meta_data2,
                                 threshold_value = 10, color_gradient_direction = "above",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid")),
    regexp = sprintf("%s|%s",
                     paste("Specified VARIABLE_ROLE.s.:",
                           ".+invalid.+ was not found in metadata, only:",
                           ".+process.+ is used."),
                     paste("Study variables: .+v00010.+, .+v00011.+,",
                           ".+v00012.+, .+v00013.+, .+v00016.+, .+v00017.+,",
                           ".+v00032.+, .+v00033.+, .+v00042.+ are not",
                           "considered due to their VARIABLE_ROLE.")),
    perl = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data,
                                 threshold_value = 10, color_gradient_direction = "above",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid")),
    regexp = sprintf("%s|%s",
                     paste("Specified VARIABLE_ROLE.s.:",
                           ".+invalid.+ was not found in metadata, only:",
                           ".+process.+ is used."),
                     paste("Study variables: .+v00010.+, .+v00011.+,",
                           ".+v00012.+, .+v00013.+, .+v00016.+, .+v00017.+,",
                           ".+v00032.+, .+v00033.+, .+v00042.+ are not",
                           "considered due to their VARIABLE_ROLE.")),
    perl = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, color_gradient_direction = "above",
                                 exclude_roles = c(VARIABLE_ROLES$PROCESS,
                                                   "invalid")),
    regexp = sprintf("%s|%s",
                     paste("Specified VARIABLE_ROLE.s.:",
                           ".+invalid.+ was not found in metadata, only:",
                           ".+process.+ is used."),
                     paste("Study variables: .+ARM_CUFF_0.+,",
                           ".+USR_VO2_0.+, .+USR_BP_0.+,",
                           ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                           ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                           ".+INT_DT_0.+, .+QUEST_DT_0.+",
                           "are not considered due to their",
                           "VARIABLE_ROLE.")),
    perl = TRUE
  )

  expect_error(suppressWarnings(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, color_gradient_direction = "invalid",
                                 exclude_roles = VARIABLE_ROLES$PROCESS)
    ),
    regexp = paste("Parameter .+color_gradient_direction.+ should be either .+above.+ or",
                   ".+below.+, but not .+invalid.+."),
    perl = TRUE
  )

  expect_error(suppressMessages(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, color_gradient_direction = 1:2,
                                 exclude_roles = VARIABLE_ROLES$PROCESS)
    ),
    regexp = paste("Parameter .+color_gradient_direction.+ should be of length",
                   "1, but not 2."),
    perl = TRUE
  )

  expect_message(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, color_gradient_direction = "above",
                                 exclude_roles = VARIABLE_ROLES$PROCESS),
    regexp = paste("Study variables: .+ARM_CUFF_0.+,",
                   ".+USR_VO2_0.+, .+USR_BP_0.+,",
                   ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                   ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                   ".+INT_DT_0.+, .+QUEST_DT_0.+",
                   "are not considered due to their",
                   "VARIABLE_ROLE.")
  )
  expect_message(
    r <- com_segment_missingness(study_data, meta_data, label_col = LABEL,
                                 threshold_value = 10, color_gradient_direction = "below",
                                 exclude_roles = VARIABLE_ROLES$PROCESS),
    regexp = paste("Study variables: .+ARM_CUFF_0.+,",
                   ".+USR_VO2_0.+, .+USR_BP_0.+,",
                   ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
                   ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
                   ".+INT_DT_0.+, .+QUEST_DT_0.+",
                   "are not considered due to their",
                   "VARIABLE_ROLE.")
  )
  expect_equal(
    length(intersect(
      names(r),
      c("ResultData", "SummaryPlot", "ReportSummaryTable")
    )), length(union(
      names(r),
      c("ResultData", "SummaryPlot", "ReportSummaryTable")
    ))
  )
  expect_true(abs(suppressWarnings(sum(as.numeric(as.matrix(
    r$ResultData)), na.rm = TRUE)) - 15288.63) < 2)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  expect_doppelganger2("segment missingness plot ok",
                              r$SummaryPlot)
})

test_that("com_segment_missingness works w/g (group|strata)_vars", {
  skip_on_cran() # slow and not frequently used
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
                   dataquieR.ERRORS_WITH_CALLER = FALSE,
                   dataquieR.WARNINGS_WITH_CALLER = FALSE,
                   dataquieR.MESSAGES_WITH_CALLER = FALSE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  expect_message({
    r1 <- com_segment_missingness(study_data, meta_data, strata_vars = "CENTER_0",
                                  threshold_value = 5,
                                  color_gradient_direction = "above",
                                  exclude_roles = VARIABLE_ROLES$PROCESS)
    r2 <- com_segment_missingness(study_data, meta_data, strata_vars = "CENTER_0",
                                  group_vars = "SEX_0",
                                  threshold_value = 5,
                                  color_gradient_direction = "above",
                                  exclude_roles = VARIABLE_ROLES$PROCESS)
    r3 <- com_segment_missingness(study_data, meta_data, group_vars = "SEX_0",
                                  threshold_value = 5,
                                  color_gradient_direction = "above",
                                  exclude_roles = VARIABLE_ROLES$PROCESS)
    },
    regexp = "Study variables: .+ are not considered due to their VARIABLE_ROLE.",
    perl = TRUE
  )
  testthat::local_edition(3)
  expect_snapshot_value(style = "deparse",
                        r1$ResultData)
  expect_snapshot_value(style = "deparse",
                        r2$ResultData)
  expect_snapshot_value(style = "deparse",
                        r3$ResultData)
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  expect_doppelganger2("segment missingness plot r1 ok",
                              r1$SummaryPlot)
  expect_doppelganger2("segment missingness plot r2 ok",
                              r2$SummaryPlot)
  expect_doppelganger2("segment missingness plot r3 ok",
                              r3$SummaryPlot)
})
