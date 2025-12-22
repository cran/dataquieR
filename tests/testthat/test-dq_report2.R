test_that("dq_report2 works", {
  skip_if_not_installed("DT")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("markdown")
  skip_on_cran() # slow, parallel, ...

  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                       dataquieR.ERRORS_WITH_CALLER = TRUE,
                       dataquieR.WARNINGS_WITH_CALLER = TRUE,
                       dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")

  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100)
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| missing_table")

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL

  # md0$MISSING_LIST_TABLE <- NULL

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report2
  # here in the first place
  report <- dq_report2(sd0, md0,
                       resp_vars = c("v00000", "v00001", "v00002",
                                     "v00003", "v00004", "v00012"),
                       filter_indicator_functions =
                         c("^com_item_missingness$",
                           "^acc_varcomp$"),
                       filter_result_slots =
                         c("^SummaryTable$"),
                       cores = NULL,
                       dimensions = # for speed, omit Accuracy
                         c("Integrity",
                           "Completeness",
                           "Consistency",
                           "Accuracy"))

  sts <- report[, "com_item_missingness", "SummaryTable"]

  expect_equal(sum(sts$SummaryTable$GRADING), 1)

  expect_silent(invisible(summary(report)))

  r <- report
  r$acc_varcomp_observer.SBP_0$SummaryTable <- NULL
  expect_silent(summary(r))

  r <- report
  r$acc_varcomp_observer.SBP_0$SummaryTable <-
    r$acc_varcomp_observer.SBP_0$SummaryTable[FALSE, , FALSE]
  expect_silent(summary(r))

  expect_error(
    report <-
      suppressWarnings(suppressMessages(dq_report2(sd0, md0,
                                 cores = NULL,
                                 dimensions = 42))
      ),
    regexp =
      sprintf("The argument %s must be character or NULL",
              sQuote("dimensions")),
    perl = TRUE
  )


  expect_warning(
    report <-
      (dq_report2(sd0, md0,
                  resp_vars = c("v00000", "v00001", "v00002",
                                "v00003", "v00004", "v00012"),
                  filter_indicator_functions =
                    c("^com_item_missingness$",
                      "^acc_varcomp$"),
                  filter_result_slots =
                    c("^SummaryTable$"),
                  cores = NULL,
                   dimensions = c("invalid"),
      )),
    regexp =
      paste("(?ms)Missing",
            ".+invalid.+from.+Completeness.+Consis.+Accuracy.+Integrity.+did",
            "you mean.+Integrity.+"),
    perl = TRUE
  )

  # md0$MISSING_LIST_TABLE <- NULL

  expect_silent(
    report <-
      suppressMessages(dq_report2(sd0, md0,
                                  resp_vars = c("v00000", "v00001", "v00002",
                                                "v00003", "v00004", "v00012"),
                                  filter_indicator_functions =
                                    c("^com_item_missingness$",
                                      "^acc_varcomp$"),
                                  filter_result_slots =
                                    c("^SummaryTable$"),
                                  cores = NULL,
                                 strata_attribute = "GROUP_VAR_XXX",
                                 dimensions = c("Completeness"),
      )
      )
  )

  report <- suppressWarnings(dq_report2(sd0, md0,
                                       cores = NULL,
                                       label_col = LABEL,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency",
                                           "Accuracy"),
                                       resp_vars = c("SBP_0", "SEX_0"),
                                       filter_indicator_functions =
                                         c("^com_item_missingness$",
                                           "^acc_distributions_loc$",
                                           "^acc_margins$"),
                                       filter_result_slots =
                                         c("^SummaryTable$"),
                                       specific_args = list(
                                         acc_margins =
                                           list(min_obs_in_subgroup = 40),
                                         acc_distributions_loc =
                                           list(flip_mode = "flip"),
                                         com_item_missingness = list(
                                           label_col = LONG_LABEL
                                         ))
  ))

  expect_equal(
    attr(report$acc_margins_observer.SBP_0, "call")[["min_obs_in_subgroup"]],
    40)

  expect_equal(
    attr(report$acc_distributions_loc.SBP_0, "call")[["flip_mode"]],
    "flip")

  expect_null(
    attr(report$acc_distributions_loc_ecdf_observer.SBP_0,
         "call")[["flip_mode"]])

  expect_equal(
    attr(report$com_item_missingness.SBP_0, "call")[["label_col"]],
    LABEL) # this should not be overwritable

  report <- suppressWarnings(dq_report2(sd0, md0,
                                       resp_vars = c("SBP_0", "SEX_0"),
                                       filter_indicator_functions =
                                         c("^acc_distributions_loc_ecdf$",
                                           "^acc_distributions_loc$"),
                                       cores = NULL,
                                       flip_mode = "flip",
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency",
                                           "Accuracy"))
  )

  expect_equal(attr(report$acc_distributions_loc.SBP_0, "call")$resp_vars,
               "SBP_0") # resp_vars cannot be overwritten, but SEX_0 is not a pimary output

  expect_equal(attr(report$acc_distributions_loc.SBP_0, "call")$flip_mode,
               "flip")

  md1 <- md0
  md1$LABEL <- c("CENTER_0",
                 "",
                 "CENTER_0 DUPLICATE", # will become a duplicated label
                 "CENTER_0", # direct duplication of the first label
                 "Have you been physically vigorously active in the past 12 hours ('physically vigorously active' means at least 30 minutes of jogging or fast cycling, digging up your garden, carrying heavy objects weighing more than 10 kg for a long time, or similar physical activities)?", # very long label
                 "Hybpvaitp1hpvamal3mojofcduygchowmt1kfaltospa") # matches the very long label after abbreviation
  md1$VAR_NAMES[2] <- "yOvCzPY60JRjmrYb16Tsd6qMymal4B5Skw9rZ5PHSCtaBqOVglAKcguPkQhakampFJcC8xqLbZJs7kZUdKH804pbOmM5ORPVabrkEkVkiWbakWiixZ99NRYF6BP8SRxzNYY2tED7DjmhMUwk0t674RjH828jq9zoTJgDxYP6nEdHBxhmXJh0ClCPjGsi1q" # very long variable name that should get caught and not be used as label as it is
  colnames(sd0)[2] <- md1$VAR_NAMES[2]

  suppressWarningsMatching(
    expect_warning(expect_warning(
      report <- dq_report2(sd0, md1,
                           label_col = LABEL,
                           cores = NULL,
                           dimensions =
                             c("Integrity",
                               "Consistency")),
      regexp = ".*Labels are required to create a report.*"),
      regexp = ".*Unique labels are required to create a report.*"),
    c("Some variables have labels with more than 60 characters in .+LABEL.+",
      "Unique labels are required",
      ".*duplicated in the metadata and cannot be used as label.*")
  )
  suppressWarningsMatching(
    expect_warning(
      report <- dq_report2(sd0, md1,
                           label_col = VAR_NAMES,
                           cores = NULL,
                           dimensions =
                             c("Integrity",
                               "Consistency")),
      regexp = ".*This will cause suboptimal outputs and possibly.*"
      ),
    c("Some variables have no label in .+LABEL", "Unique labels are required",
      "more than 60 characters")
  )

  md1$VAR_NAMES[2] <- "" # this will be considered different
  colnames(sd0)[2] <- "" # because both are missing, and NA maybe unequal
                         # from NA

  suppressWarningsMatching(
    expect_warning(expect_warning(expect_warning(
            report <- dq_report2(sd0, md1,
                                 label_col = LABEL,
                                 cores = NULL,
                                 dimensions =
                                   c("Integrity",
                                     "Consistency")),
            regexp = "Need.+VAR_NAMES.+in.*meta_data",
            perl = TRUE
          ),
          regexp = "Some variables have duplicated labels in .+LABEL.+",
          perl = TRUE
        ),
      regexp =
        "Some variables have labels with more than 60 characters in .+LABEL.+",
      perl = TRUE
    ),
    c("(Some variables have duplicated labels in .+LABEL.+|Need.+VAR_NAMES.+in.*meta_data|Some variables have labels with more than 60 characters in .+LABEL.+|Lost 16.7% of the study data because of missing/not assignable metadata|.+dummy names|Some variables have no label in .+LABEL)",
      ".*duplicated in the metadata and cannot be used as label.*")
  )

  suppressWarningsMatching(
      report <- dq_report2(sd0, md1,
                           label_col = VAR_NAMES,
                           cores = NULL,
                           dimensions =
                             c("Integrity",
                               "Consistency")),
    "(Unique labels are required|Some variables have labels with more than 60 characters in .+LABEL.+|Lost 16.7% of the study data because of missing/not assignable metadata|Need.+VAR_NAMES.+discard.+|.+dummy names|Some variables have no label in .+LABEL)"
  )

  #warning message that there is no metadata
  prep_purge_data_frame_cache()
  study_data <- head(
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"),
    50)

  expect_warning(dq_report2(study_data = study_data,
                            cores = NULL,
                 dimensions = "Integrity"),
                 regexp = "NO ITEM LEVEL METADATA.+PLEASE CONSIDER PASSING.+")


  md1 <-
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| item_level")
  md1 <- md1[, !colnames(md1) %in% c("VARIABLE_ROLE", "MISSING_LIST_TABLE")]


  #message about no assigned variable roles
  suppressWarningsMatching(dq_report2(study_data = study_data,
                            meta_data = md1,
                            cores = NULL,
                            dimensions = "Integrity"),
                           "Metadata does not provide.+for replacing codes with NAs.")

  md1 <-
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| item_level")
  md1 <- md1[, !colnames(md1) %in% "MISSING_LIST_TABLE"]
  md1[1, VARIABLE_ROLE] <- "non-existing_role"


  # message about non existing role (ignore warnings about missing JUMP_LIST and
  # MISSING_LIST)
  suppressWarningsMatching(dq_report2(study_data = study_data,
                           meta_data = md1,
                           cores = NULL,
                           dimensions = "Integrity"),
                 "Metadata does not provide.+for replacing codes with NAs.")


  #message of variable not found in the metadata
  md1<- md1[, colnames(md1) %in% c("VAR_NAMES", "LABEL", "DATA_TYPE",
                                   "SCALE_LEVEL", "VALUE_LABELS")]
  md1 <- rbind(md1, data.frame(VAR_NAMES = "no_real_var_name",
                               LABEL = "no_var_name",
                               DATA_TYPE = "integer",
                               SCALE_LEVEL = "nominal",
                               VALUE_LABELS = NA_character_))

  expect_message2(dq_report2(study_data = study_data,
                            meta_data = md1,
                            cores = NULL,
                            dimensions = "Integrity"))

  # sd0 <- study_data
  # md0 <- meta_data
  # md0 <- md0[which(md0[[LABEL]] %in%
  #                    c("CENTER_0", "CRP_0", "BSG_0", "PSEUD0_ID", "DEV_NO_0", "PART_STUDY", "PART_LAB")), ]
  # md0[[LABEL]] <- ""
  # # create a grouping variable with large integer numbers and without specifying value labels
  # sd0$v11111 <- sd0$v00000 * 1000
  # md0[["KEY_DEVICE"]][md0[["VAR_NAMES"]] == "v00015"] <- "v11111"
  # md0 <- rbind(md0, c("v11111", "", "integer", rep(NA_character_, ncol(md0) - 3)))
  # sd0 <- sd0[which(colnames(sd0) %in% md0[[VAR_NAMES]])]
  # test_rep <- dq_report2(study_data = sd0, meta_data = md0, dimensions = NULL)
})

