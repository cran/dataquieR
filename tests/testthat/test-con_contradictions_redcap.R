test_that("con_contradictions_redcap works", {
  skip_on_cran() # slow, redcap parser is tested anyway, errors in plots obvious
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  withr::local_timezone("CET")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  meta_data_cross_item <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|cross-item_level")
  # ignore here 'special' data preparation steps, will be checked separately
  meta_data_cross_item$DATA_PREPARATION <- ""
  label_col <- "LABEL"
  threshold_value <- 1
  #expect_message(
    expect_silent({
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
    })
  #)
  expect_equal(off$FlaggedStudyData, on$Other$all_checks$FlaggedStudyData)
  expect_equal(off$VariableGroupTable, on$Other$all_checks$VariableGroupTable)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  expect_doppelganger2("summary contradiction plot ok",
                              on$Other$all_checks$SummaryPlot)
  expect_doppelganger2("summary contradiction plot ok",
                              default$SummaryPlot)
  expect_doppelganger2("summary contradiction plot ok",
                              off$SummaryPlot)
  expect_doppelganger2("one cat contradiction plot ok",
                              on$Other$EMPIRICAL$SummaryPlot)
})

test_that("con_contradictions_redcap works with tiny inputs", {
  skip_on_cran() # slow, redcap parser is tested anyway, errors in plots obvious
  skip_if_not_installed("withr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  # catch if some objects will be reduced to scalars or vectors instead of dataframes or matrices
  withr::local_timezone('CET')
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
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

test_that("con_contradictions_redcap uses DATA_PREPARATION correctly", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = study_data,
                                           meta_data = meta_data)
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  meta_data_cross_item <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|cross-item_level")

  # If nothing is specified in DATA_PREPARATION, missing value labels and values
  # outside hard limits should be replaced by NA.
  mdci <- meta_data_cross_item[1, ]
  res1 <- con_contradictions_redcap(study_data = study_data,
                                    meta_data = meta_data,
                                    label_col = LABEL,
                                    threshold_value = 1,
                                    meta_data_cross_item = mdci)
  mdci$DATA_PREPARATION <- "LIMITS | MISSING_NA"
  res2 <- con_contradictions_redcap(study_data = study_data,
                                    meta_data = meta_data,
                                    label_col = LABEL,
                                    threshold_value = 1,
                                    meta_data_cross_item = mdci)
  expect_equal(res1$FlaggedStudyData, res2$FlaggedStudyData)

  # check option MISSING_INTERPRET
  mdci$CHECK_LABEL <- "Inconsistent reason for missingness in number of children"
  mdci$CONTRADICTION_TERM <- "[N_BIRTH_0] > 0 and ([N_CHILD_0] in set('NE', 'P', 'NC'))"
  mdci$CONTRADICTION_TYPE <- "EMPIRICAL"
  mdci$DATA_PREPARATION <- "MISSING_INTERPRET"
  # We would expect these observations to be picked up:
  miss_tab <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|missing_table")
  sel_miss_codes <- miss_tab$CODE_VALUE[which(miss_tab$CODE_INTERPRET %in%
                                                c("NE", "NC", "P"))]
  check_res <- sum(table(study_data[which(study_data$v00021 %in% sel_miss_codes
                                          & study_data$v00027 > 0
                                          & study_data$v00027 < 8000),
                                    c("v00021", "v00027")]))
  # NE = 99981, NC = 99983, P = 99988
  #expect_equal(res3$VariableGroupTable$NUM_con_con, check_res)
  # but numerical variables are not yet supported here
  expect_warning({
    res3 <- con_contradictions_redcap(study_data = study_data,
                                    meta_data = meta_data,
                                    label_col = LABEL,
                                    threshold_value = 1,
                                    meta_data_cross_item = mdci)},
    regexp = "not yet supported for numerical variables")

  # check that the function catches problems with variables on smoking
  # (replacing missing value codes by NA not specified, but should be done;
  # inadmissible categorical values on top;
  # 91 observations match the contradiction rule)
  mdci <- meta_data_cross_item[grepl("Non-smokers inconsistency",
                                     meta_data_cross_item$CHECK_LABEL), ,
                               drop = FALSE]
  expect_message({
    res4 <- con_contradictions_redcap(study_data = study_data,
                                    meta_data = meta_data,
                                    label_col = LABEL,
                                    threshold_value = 1,
                                    meta_data_cross_item = mdci)},
    regexp = sprintf("(%s|%s)",
      "replace the missing codes by NA, too",
      "Number of levels in variable greater than in character string"))
  expect_equal(res4$VariableGroupTable$NUM_con_con, 91)
})

#Temporarily skipped due to ship moving to website
test_that("no regression, rule errors should not be missed", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  sd1 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx")
  md1 <- prep_get_data_frame("item_level")
  checks <- prep_get_data_frame("cross-item_level")
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(study_data = sd1,
                                           meta_data = md1)
  md1[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      md1[[VAR_NAMES]]
    ]

  checks[3,3] <- "[BODY_HEIGHT_0] < [OBS_SOMA_0]"
  checks[4,3] <- "[BODY_HEIGHT_0] < [EXAM_DT_0]"
  checks[4,3] <- "[BODY_HEIGHT] < 1.2"
  checks[5,3] <- "[SEX] < 3"
  checks <- checks[-c(1:2), ]
  checks <- checks[-c(4:10), ]

  suppressMessages(suppressWarnings(expect_warning(
    AnyContradictions <- con_contradictions_redcap(study_data = sd1,
                                                   meta_data       = md1,
                                                   label_col       = "LABEL",
                                                   meta_data_cross_item = checks,
                                                   threshold_value = 1),
    regexp = "object.+SEX.+not found"
  )))

  expect_equal(AnyContradictions$VariableGroupTable$NUM_con_con,
               c(2152, NA_real_, NA_real_))
  # the first test is by default comparing lexicographically, since obs_soma is a factor and LABEL is default for DATA_PREPARATION
  # the other two tests always fail because of missing variables
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  sd1 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx")
  md1 <- prep_get_data_frame("item_level")
  checks <- prep_get_data_frame("cross-item_level")

  suppressWarnings(suppressMessages(
    AnyContradictions <- con_contradictions_redcap(study_data = sd1,
                                                   meta_data       = md1,
                                                   label_col       = "LABEL",
                                                   meta_data_cross_item = checks,
                                                   threshold_value = 1)))

  expect_equal(AnyContradictions$VariableGroupTable$NUM_con_con, c(35,
                                                             0,
                                                             0,
                                                             63,
                                                             12,
                                                             0,
                                                             0))
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  sd1 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx")
  md1 <- prep_get_data_frame("item_level")
  checks <- prep_get_data_frame("cross-item_level")

  checks[1,3] <- "[sbp] < [dbp]" # variables not in dataset/metadata
  checks[2,3] <- "sbp2 < dbp2"   # omit brackets
  checks[6,3] <-
    "[diab_known] = \"yes\" NOT [diab_age] > 0" # uses variable names instead of labels
  checks <- checks[-c(3:5), ]
  checks <- checks[-c(4:9), ]


  suppressMessages(suppressWarnings(expect_warning({
    AnyContradictions <- con_contradictions_redcap(study_data = sd1,
                                                   meta_data       = md1,
                                                   label_col       = "LABEL",
                                                   meta_data_cross_item = checks,
                                                   threshold_value = 1)
    }, regexp = "Parser error"
    )))

  expect_true(all(is.na(AnyContradictions$VariableGroupTable$NUM_con_con)))

})
