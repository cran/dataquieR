test_that("con_contradictions_redcap works", {
  skip_on_cran() # slow, redcap parser is tested anyway, errors in plots obvious
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  withr::local_timezone("CET")
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  meta_data_cross_item <- prep_get_data_frame("meta_data_v2|cross-item_level")
  label_col <- "LABEL"
  threshold_value <- 1
  #expect_message(
    expect_warning({
      default <- con_contradictions_redcap(
        study_data = study_data, meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item
      )
      off <- con_contradictions_redcap(
        study_data = study_data, meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item,
        summarize_categories = FALSE
      )
      on <- con_contradictions_redcap(
        study_data = study_data, meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item,
        summarize_categories = TRUE
      )
    },
    regexp = sprintf("%s|%s|%s",
                     paste("VARIABLE_LIST is ignored for contradictions. The",
                           "rule will be used for finding all related variables."),
                     paste("No labels assigned for some levels, use levels as labels"),
                     paste("N = [0-9]+ values in [A-Z_]+_[0-1] have",
                           "been (above)|(below) HARD_LIMITS and were removed.")
    ),
    perl = TRUE,
    all = TRUE
    )
  #)
  expect_equal(off$FlaggedStudyData, on$all_checks$FlaggedStudyData)
  expect_equal(off$SummaryTable[-which(colnames(off$SummaryTable) == "CONTRADICTION_TYPE")],
               on$all_checks$SummaryTable)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("summary contradiction plot ok",
                              on$all_checks$SummaryPlot)
  vdiffr::expect_doppelganger("summary contradiction plot ok",
                              default$SummaryPlot)
  vdiffr::expect_doppelganger("summary contradiction plot ok",
                              off$SummaryPlot)
  vdiffr::expect_doppelganger("one cat contradiction plot ok",
                              on$EMPIRICAL$SummaryPlot)
})

test_that("con_contradictions_redcap works with tiny inputs", {
  skip_on_cran() # slow, redcap parser is tested anyway, errors in plots obvious
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  # catch if some objects will be reduced to scalars or vectors instead of dataframes or matrices
  withr::local_timezone('CET')
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  threshold_value <- 1

  meta_data_cross_item <- data.frame("CONTRADICTION_TERM" = "[AGE_0] < 18",
                            "CHECK_LABEL" = "Age hard limits check for testing",
                            "CONTRADICTION_TYPE" = "LOGICAL")
  meta_data_cross_item2 <- data.frame("CONTRADICTION_TERM" = "[v00003] < 18",
                             "CHECK_LABEL" = "Age hard limits check for testing",
                             "CONTRADICTION_TYPE" = "LOGICAL")
  meta_data_cross_item3 <- data.frame("CONTRADICTION_TERM" = c("[v00003] < 18", "[v00003] > 130"),
                             "CHECK_LABEL" = c("Age hard limits check for testing", "Age hard limits check for testing no. 2"),
                             "CONTRADICTION_TYPE" = c("LOGICAL", "EMPIRICAL"))
  tiny_sd <- study_data[, which(colnames(study_data) == meta_data$VAR_NAMES[which(meta_data$LABEL == "AGE_0")]), drop = FALSE]
  tiny_md <- meta_data[which(meta_data$LABEL == "AGE_0"), , drop = FALSE]
  tiny_md[[JUMP_LIST]][util_empty(tiny_md[[JUMP_LIST]])] <- SPLIT_CHAR
  tiny_md[[MISSING_LIST]][util_empty(tiny_md[[MISSING_LIST]])] <- SPLIT_CHAR
  #expect_message(
  expect_message({
    check1 <- con_contradictions_redcap( # only one contradiction check -> nrow(meta_data_cross_item) is 1
      study_data = tiny_sd, meta_data = tiny_md, label_col = "LABEL", # using VAR_NAMES and LABELs => two "needles" to generate the variable list
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item)
    check2 <- con_contradictions_redcap(
      study_data = tiny_sd, meta_data = tiny_md, label_col = "VAR_NAMES", # only one "needle" to generate the variable list
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item2) # only one contradiction check -> nrow(meta_data_cross_item2) is 1
    check3 <- con_contradictions_redcap(
      study_data = tiny_sd, meta_data = tiny_md, label_col = "VAR_NAMES", # only one "needle" to generate the variable list
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item3) # nrow(meta_data_cross_item3) is 2
  })
  #)
})
