test_that("con_contradictions works", {
  load(system.file("extdata", "study_data.RData", package = "dataquieR"),
       envir = environment())
  load(system.file("extdata", "meta_data.RData", package = "dataquieR"),
       envir = environment())
  check_table <- read.csv(
    system.file("extdata",
      "contradiction_checks.csv",
      package = "dataquieR"
    ), header = TRUE, sep = "#"
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
  expect_message(
    expect_warning({
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
      },
      regexp = sprintf("%s|%s|%s|%s",
                       paste("All variables with CONTRADICTIONS in the",
                             "metadata are used."),
                       paste("N = 21 values in EDUCATION_1 have",
                             "been above HARD_LIMITS and were removed."),
                       paste("N = 120 values in SMOKE_SHOP_0 have been",
                             "above HARD_LIMITS and were removed."),
                       paste("Variables: AGE_0, AGE_1, EXAM_DT_0, LAB_DT_0",
                             "have no assigned labels and levels.")
                       ),
      perl = TRUE,
      all = TRUE
    ),
    regexp =
      paste("Labels of variables from .LABEL. will be used. In this case",
            "columns A and B in check_tables must refer to labels."),
    perl = TRUE,
    all = TRUE
  )
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
  vdiffr::expect_doppelganger("summary contradiction plot ok",
                              on$all_checks$SummaryPlot)
  vdiffr::expect_doppelganger("summary contradiction plot ok",
                              default$SummaryPlot)
  vdiffr::expect_doppelganger("summary contradiction plot ok",
                              off$SummaryPlot)

  vdiffr::expect_doppelganger("one cat contradiction plot ok",
                              on$Empirical$SummaryPlot)
})
