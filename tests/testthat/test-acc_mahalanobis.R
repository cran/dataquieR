test_that("mahalanobis works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  df1 <- data.frame(
    ID = c(letters[1:26], "aa", "bb", "cc", "dd"),
    Q1 = c(5, 4, 1, 2, 2, 99999, 2,rep(1, 10), 2, 2, 2, 2, 3, 2, 1, 1, 2, 2, 2, 2, 1),
    Q2 = c(2, 5, rep(1, 4), 2, rep(1, 13), 2, 2, 1, 1, 1, 1, 1, 1, 2, 1),
    Q3 = c(4, 5, rep(1, 20),3, 1, 1, 1, 1, 1, 1, 1 ),
    Q4 = c(NA, 1, 4, 4, 5, 3, 4, 5, 3, 3, 5, 5, 4, 4, 4, 3, 4, 3, 4, 5, 5, 4, 4, 3, 4, 4, 4, 4, 5, 4),
    Q5 = c(88888, 1, 3, 4, 3, 3, 4, 3, 4, 3, 4, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 4, 4, 3, 3, 3, 4, 3, 4, 3),
    Q6 = c(88888, 1, 5, 5, 5, 4, 5, 4, 5, 3, 4, 5, 4, 4,rep(5, 5), 4, 4, 5, 5, 4, 4, 5, 5, 4, 5, 5),
    Q7 = c(NA, 2, 3, 3, 2, rep(3, 4), 4, 3, 3, 4, 3, 3, 4, 3, 2, 3, 3, 4, 3, 4, 3, 3, 4, 3, 3, 3, 4),
    Q8 = c(NA, 1, rep(4,5), 5, 4, 4, 3, 4, 3, 3, 4, 4, 5, 5, 4, 4, 3, 3, 4, 4, 4, 4, 5, 5, 4, 4),
    Q9 = c(NA, 1, 4, 3, 3, 4, 3, 3, 2, 3, 3, 2, rep(3, 4), 4, 3, 3, 4, 3, 3, 3, 4, 3, 4, 4, 3, 3, 3),
    Q10 = c(1, 1, 2, 1, rep(2, 4), 1, 2, 2, 2, 3, 3, rep(2, 4), 3, rep(2, 4),3, 2, 2, 3, 2, 1, 2),
    stringsAsFactors = FALSE
  )

  #PREPARE_METADATA
  # Define the common value labels to avoid repetitive typing
  likert_labels <-
    "1 = not apply | 2 = rather not apply | 3 = intermediate | 4 = rather apply | 5 = apply completely"

  # Create the item_level data frame
  item_level <- data.frame(
    VAR_NAMES = c("ID", paste0("Q", 1:10), "T1", "T2"),
    LABEL = c(
      "pseudo id", "shy and reserved", "trust people", "tasks throughly",
      "relaxed", "active imagination", "sociable", "find faults in others",
      "lazy", "nervous", "artistic interest", "begin", "end"
    ),
    DATA_TYPE = c("string", rep("integer", 10), "datetime", "datetime"),
    SCALE_LEVEL = c("na", rep("ordinal", 10), "interval", "interval"),
    TIME_VAR = c(NA, rep("T1", 10), NA, NA),
    TIME_VAR_END = c(NA, rep("T2", 10), NA, NA),
    MISSING_LIST_TABLE = c(NA, rep("missing_table", 10), "missing_table", "missing_table"),
    VALUE_LABELS = c(
      NA,
      rep(likert_labels, 10),
      NA,
      NA
    ),
    STUDY_SEGMENT = c(NA, rep("Questionnaire", 12)),
    VARIABLE_ROLE = c("intro", rep("primary", 10), "process", "process"),
    stringsAsFactors = FALSE
  )

  #add it to the cache
  prep_purge_data_frame_cache()
  prep_add_data_frames(item_level)

  # Create the missing_table data frame
  missing_table <- data.frame(
    CODE_VALUE = c(99999, 99998, 99997, 88888),
    CODE_LABEL = c(
      "Missing - other reason",
      "Missing - exclusion criteria",
      "Missing - refusal",
      "JUMP 88880"
    ),
    CODE_CLASS = c("MISSING", "MISSING", "MISSING", "JUMP"),
    stringsAsFactors = FALSE
  )

  # add in the cache
  prep_add_data_frames(missing_table)


  # Create the cross_item_level data frame
  cross_item_level <- data.frame(
    VARIABLE_LIST = c(
      "Q1 | Q2 | Q3 | Q4 | Q5 | Q6",
      "Q7 | Q8 | Q9 | Q10",
      "Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10"
    ),
    CHECK_LABEL = c(
      "First part",
      "Second part",
      "all questionnaire"
    ),
    CONTRADICTION_TERM = c(NA, NA, NA),
    CONTRADICTION_TYPE = c(NA, NA, NA),
    MULTIVARIATE_OUTLIER_CHECKTYPE = c(NA, NA, NA),
    N_RULES = c(NA, NA, NA),
    MAXIMUM_LONG_STRING = c("[;2)", "[;]", "[;3]"),
    IRV = c(NA, "[-1; 1]", NA),
    IRV_ARGS = c(NA, "20", NA),
    stringsAsFactors = FALSE
  )

  ## add in the cache
  prep_add_data_frames(cross_item_level)


  mahal_res <- acc_mahalanobis(variable_group = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"),
                       study_data = df1,
                       meta_data = "item_level")


  expect_equal(mahal_res$SummaryTable$NUM_ssc_MD, 2)
  expect_equal(mahal_res$SummaryTable$observational_units_removed, 2)
  expect_equal(mahal_res$SummaryData$`MD_outliers (%)`, 7.14)
})
