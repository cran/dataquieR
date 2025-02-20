test_that("con_contradictions works", {
  skip_on_cran() # slow and deprecated, use redcap rules, now
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("withr")
  withr::local_timezone('CET')
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  meta_data <- prep_scalelevel_from_data_and_metadata(meta_data = meta_data,
                                                      study_data = study_data)
  meta_data[startsWith(meta_data[[LABEL]], "EDUCATION_"), SCALE_LEVEL] <-
    SCALE_LEVELS$ORDINAL
  check_table <- read.csv(
    "https://dataquality.ship-med.uni-greifswald.de/extdata/contradiction_checks.csv",
    header = TRUE, sep = "#"
  )
  check_table[1, "tag"] <- "Logical"
  check_table[1, "Label"] <- "Becomes younger"
  check_table[2, "tag"] <- "Empirical"
  check_table[2, "Label"] <- "sex transformation"
  check_table[3, "tag"] <- "Empirical"
  check_table[3, "Label"] <- "looses academic degree"
  check_table[4, "tag"] <- "Logical"
  check_table[4, "Label"] <- "vegetarian eats meat"
  check_table[5, "tag"] <- "Logical"
  check_table[5, "Label"] <- "vegan eats meat"
  check_table[6, "tag"] <- "Empirical"
  check_table[6, "Label"] <- "non-veg* eats meat"
  check_table[7, "tag"] <- "Empirical"
  check_table[7, "Label"] <- "Non-smoker buys cigarettes"
  check_table[8, "tag"] <- "Empirical"
  check_table[8, "Label"] <- "Smoker always scrounges"
  check_table[9, "tag"] <- "Logical"
  check_table[9, "Label"] <- "Cuff didn't fit arm"
  check_table[10, "tag"] <- "Empirical"
  check_table[10, "Label"] <- "Very mature pregnant woman"
  label_col <- LABEL
  threshold_value <- 1
  check_table[1, "tag"] <- "Logical, Age-Related"
  check_table[10, "tag"] <- "Empirical, Age-Related"
  suppressMessages(suppressWarnings({
      default <- con_contradictions(
        study_data = study_data, meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, check_table = check_table
      )
      off <- con_contradictions(
        study_data = study_data, meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, check_table = check_table,
        summarize_categories = FALSE
      )
      on <- con_contradictions(
        study_data = study_data, meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, check_table = check_table,
        summarize_categories = TRUE
      )
    }
  ))
  # expect_equal(off, default) because of plots not always true,
  # e.g.:   Component "SummaryPlot": Component "layers": Component 1:
  # Component 11: Component 1: target is not list-like
  expect_equal(off$FlaggedStudyData, on$all_checks$FlaggedStudyData)
  expect_equal(off$SummaryTable, on$all_checks$SummaryTable)
  expect_equal(off$SummaryData, on$all_checks$SummaryData)

  expect_lt(
    abs(suppressWarnings(sum(as.numeric(as.matrix(default$SummaryData)),
                             na.rm = TRUE)) - 12052.56), 10)

  expect_lt(
    abs(suppressWarnings(sum(as.numeric(as.matrix(on$Empirical$SummaryTable)),
                             na.rm = TRUE)) -  5474.32), 10)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  expect_doppelganger2("summary contradiction plot ok",
                              on$all_checks$SummaryPlot)
  expect_doppelganger2("summary contradiction plot ok",
                              default$SummaryPlot)
  expect_doppelganger2("summary contradiction plot ok",
                              off$SummaryPlot)

  expect_doppelganger2("one cat contradiction plot ok",
                              on$Empirical$SummaryPlot)
})
