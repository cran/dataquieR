test_that("distribution plot works", { # acc_distributions.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    r <-
      acc_distributions(
        resp_vars = "DBP_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
        label_col = LABEL)

    expect_doppelganger2("acc_distributions gr dbp0", r$SummaryPlotList$DBP_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    r <-
      acc_distributions(
        resp_vars = "EDUCATION_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
        label_col = LABEL)

    expect_doppelganger2("acc_distributions def edu0", r$SummaryPlotList$EDUCATION_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$EDUCATION_0), 15 * 1024 * 1024)

  })
})

test_that("loess plot works", { # acc_loess.R ----
  # testthat::local_reproducible_output()
  # skip_on_travis() # vdiffr fails
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  require_english_locale_and_berlin_tz()

  ({
    time_vars <- prep_map_labels("DBP_0", meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level", from = LABEL,
                                 to = TIME_VAR)
    group_vars <- prep_map_labels("DBP_0", meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level", from = LABEL,
                                  to = GROUP_VAR_OBSERVER)
    r <-
      acc_loess(  resp_vars = "DBP_0",
                  study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                  meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
                  group_vars = group_vars,
                  time_vars = time_vars,
                  label_col = LABEL,
                  co_vars = "AGE_0")

    expect_doppelganger2("loess def dbp0", r$SummaryPlotList$DBP_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    r <- acc_loess(resp_vars = "EDUCATION_0",
                   study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                   meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
                   time_vars = time_vars,
                   group_vars = group_vars,
                   label_col = LABEL,
                   co_vars = "AGE_0")

    expect_doppelganger2("loess def edu0", r$SummaryPlotList$EDUCATION_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$EDUCATION_0), 15 * 1024 * 1024)

    r <-
      acc_loess(  resp_vars = "DBP_0",
                  study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                  meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
                  group_vars = group_vars,
                  time_vars = time_vars,
                  plot_format = "FACETS",
                  label_col = LABEL,
                  co_vars = "AGE_0")

    expect_doppelganger2("loess fac dbp0", r$SummaryPlotList$DBP_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    r <-
      acc_loess(  resp_vars = "EDUCATION_0",
                  study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                  meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
                  time_vars = time_vars,
                  group_vars = group_vars,
                  plot_format = "FACETS",
                  label_col = LABEL,
                  co_vars = "AGE_0")

    expect_doppelganger2("loess fac edu0", r$SummaryPlotList$EDUCATION_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$EDUCATION_0), 15 * 1024 * 1024)

  })
})

test_that("margins plot works", { # acc_margins.R -----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  ({
    group_vars <- prep_map_labels("DBP_0",
                                  meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level",
                                  from = LABEL,
                                  to = GROUP_VAR_OBSERVER)
    r <-
      acc_margins(resp_vars = "DBP_0",
                  study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                  meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
                  group_vars = group_vars,
                  label_col = LABEL,
                  co_vars = "AGE_0",
                  sort_group_var_levels = FALSE)

    expect_doppelganger2("margins dbp0", r$SummaryPlot)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

    prep_purge_data_frame_cache()
    skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
    prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx")
    md <- prep_get_data_frame("item_level")

    # tweak metadata to enable the test
    md$SCALE_LEVEL[md$LABEL == "EDUCATION_0"] <- SCALE_LEVELS$INTERVAL

    r <-
      acc_margins(resp_vars = "EDUCATION_0",
                  study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
                  meta_data = md,
                  group_vars = group_vars,
                  label_col = LABEL,
                  co_vars = "AGE_0")

    expect_doppelganger2("margins edu0", r$SummaryPlot)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)
  })
})

test_that("multivariate outlier plot works", { # acc_multivariate_outlier.R ----
  skip_on_cran()

  skip_if_not_installed("vdiffr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  withr::with_seed(32245253, {
    r <-
      acc_multivariate_outlier(
        variable_group = c("DBP_0", "SBP_0", "AGE_0"),
        label_col = LABEL,
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
        scale = FALSE)

    expect_doppelganger2("acc_multivariate_outlier test 1",
                                r$SummaryPlot)
    skip_if(TRUE || R.version$major == "4" && R.version$minor == "5.1", message = "Strange size differences in lobstr on test containers with many, but not all R versions for this plot. No idea, but likely related with the FIXME at the end of this file.") # FIXME
    skip_if_not_installed("lobstr")
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

  })
})


test_that("shape or scale plot works", { # acc_shape_or_scale.R -----
  skip_on_cran()
  skip_if_not_installed("lobstr")

  skip_if_not_installed("vdiffr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  ({
    r <-
      acc_shape_or_scale(
        resp_vars = "DBP_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
        label_col = LABEL)

    expect_doppelganger2("shape or scale dbp0", r$SummaryPlot)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

  })
})

test_that("univariate outlier plot works", { # acc_univariate_outlier.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  withr::with_seed(32245253, {
    r <-
      acc_univariate_outlier(
        resp_vars = c("DBP_0", "DEV_NO_0"),
        label_col = LABEL,
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")

    expect_doppelganger2("acc_univariate_outlier.R DBP_0",
                                r$SummaryPlotList$DBP_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    # Argument “resp_vars”: Variable 'DEV_NO_0' (nominal) does not have an
    # allowed scale level (interval | ratio)
    # In “resp_vars”, variables “DEV_NO_0” were excluded.
    expect_null(r$SummaryPlotList$DEV_NO_0)

  })
})

test_that("old contradiction plots work", { # con_contradictions.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    check_table <- read.csv(
        "https://dataquality.qihs.uni-greifswald.de/extdata/contradiction_checks.csv",
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
    check_table[1, "tag"] <- "Logical, Age-Related"
    check_table[10, "tag"] <- "Empirical, Age-Related"
    label_col <- "LABEL"
    threshold_value <- 1
    study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
    meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
    meta_data <- prep_scalelevel_from_data_and_metadata(meta_data = meta_data,
                                                        study_data = study_data)
    meta_data[startsWith(meta_data[[LABEL]], "EDUCATION_"), SCALE_LEVEL] <-
      SCALE_LEVELS$ORDINAL
    r <-
      con_contradictions(
        study_data = study_data, meta_data = meta_data, label_col =
          label_col,
        threshold_value = threshold_value, check_table = check_table,
        summarize_categories = TRUE
      )
    expect_doppelganger2("con_contradictions by tag",
                                r$SummaryPlot)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2("con_contradictions logical age checks, only",
                                r$`Logical, Age-Related`$SummaryPlot)
    expect_lt(lobstr::obj_size(r$`Logical, Age-Related`$SummaryPlot),
              15 * 1024 * 1024)

    expect_doppelganger2("con_contradictions all checks",
                                r$all_checks$SummaryPlot)
    expect_lt(lobstr::obj_size(r$all_checks$SummaryPlot),
              15 * 1024 * 1024)


  })
})

test_that("redcap based contradiction plots work", { # con_contradictions_redcap.R ----
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    label_col <- "LABEL"
    threshold_value <- 1
    r <- con_contradictions_redcap(
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", label_col = label_col,
      threshold_value = threshold_value, meta_data_cross_item =
        "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|cross-item_level",
      summarize_categories = TRUE
    )

    expect_doppelganger2("con_contradictions rc by tag",
                                r$SummaryPlot)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2("con_contradictions rc logical checks, only",
                                r$Other$LOGICAL$SummaryPlot)
    expect_lt(lobstr::obj_size(r$Other$LOGICAL$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2("con_contradictions rc all checks",
                                r$Other$all_checks$SummaryPlot)
    expect_lt(lobstr::obj_size(r$Other$all_checks$SummaryPlot),
              15 * 1024 * 1024)

  })
})

test_that("limit deviation plots work", { # con_limit_deviations.R ----
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  set.seed(2012) # randomly scattered points should stay in their position for testing

  ({
    label_col <- "LABEL"
    threshold_value <- 1
    meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
    meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
      "[2018-01-01 00:00:00 CET; 2022-12-12 23:59:59 CET)"

    r1 <- con_limit_deviations( # all limits exist
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", meta_data = meta_data,
      label_col = label_col,
      limits = "HARD"
    )

    meta_data[meta_data$LABEL == "SBP_0", "HARD_LIMITS"] <-
      "[80; )"
    meta_data[meta_data$LABEL == "ITEM_1_0", "HARD_LIMITS"] <-
      "[0; )"
    meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
      "[2018-01-01 00:00:00 CET; )"

    r2 <- con_limit_deviations( # lower limits exist
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", meta_data = meta_data,
      label_col = label_col,
      limits = "HARD"
    )

    meta_data[meta_data$LABEL == "SBP_0", "HARD_LIMITS"] <-
      "(; 180]"
    meta_data[meta_data$LABEL == "ITEM_1_0", "HARD_LIMITS"] <-
      "(; 10]"
    meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
      "(; 2022-12-12 23:59:59 CET]"

    r3 <- con_limit_deviations( # upper limits exist
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
      meta_data = meta_data,
      label_col = label_col,
      limits = "HARD"
    )

    # meta_data[meta_data$LABEL == "SBP_0", "HARD_LIMITS"] <-
    #   NA_character_
    # meta_data[meta_data$LABEL == "ITEM_1_0", "HARD_LIMITS"] <-
    #   NA_character_
    # meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
    #   NA_character_
    #
    # r4 <- con_limit_deviations( # no limits exist
    #   resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
    #   study_data = "study_data", meta_data = meta_data,
    #   label_col = label_col,
    #   limits = "HARD"
    # )

    # > 20 < 20 obs, w/ and w/o limits (upper/lower), date vars .//. others

    expect_doppelganger2("con_limit_deviations all sbp_0",
                                r1$SummaryPlotList$SBP_0)
    expect_lt(lobstr::obj_size(r1$SummaryPlotList$SBP_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations all item",
                                r1$SummaryPlotList$ITEM_1_0)
    expect_lt(lobstr::obj_size(r1$SummaryPlotList$ITEM_1_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations all quest_dt",
                                r1$SummaryPlotList$QUEST_DT_0)
    expect_lt(lobstr::obj_size(r1$SummaryPlotList$QUEST_DT_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations low sbp_0",
                                r2$SummaryPlotList$SBP_0)
    expect_lt(lobstr::obj_size(r2$SummaryPlotList$SBP_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations low item",
                                r2$SummaryPlotList$ITEM_1_0)
    expect_lt(lobstr::obj_size(r2$SummaryPlotList$ITEM_1_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations low quest_dt",
                                r2$SummaryPlotList$QUEST_DT_0)
    expect_lt(lobstr::obj_size(r2$SummaryPlotList$QUEST_DT_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations upp sbp_0",
                                r3$SummaryPlotList$SBP_0)
    expect_lt(lobstr::obj_size(r3$SummaryPlotList$SBP_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations upp item",
                                r3$SummaryPlotList$ITEM_1_0)
    expect_lt(lobstr::obj_size(r3$SummaryPlotList$ITEM_1_0), 15 * 1024 * 1024)

    expect_doppelganger2("con_limit_deviations upp quest_dt",
                                r3$SummaryPlotList$QUEST_DT_0)
    expect_lt(lobstr::obj_size(r3$SummaryPlotList$QUEST_DT_0), 15 * 1024 * 1024)


  })
})

test_that("data type matrix and print.ReportSummaryTable plots work", {
  # int_datatype_matrix.R and print.ReportSummaryTable.R ----
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    label_col <- "LABEL"

    r <- int_datatype_matrix(
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
      label_col = label_col,
      split_segments = TRUE
    )

    expect_doppelganger2("int_datatype_matrix",
                                r$SummaryPlot)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2("int_datatype_matrix segment v50000",
                                r$DataTypePlotList$PART_QUESTIONNAIRE)
    expect_lt(lobstr::obj_size(r$DataTypePlotList$PART_QUESTIONNAIRE),
              15 * 1024 * 1024)

    expect_doppelganger2("int_datatype_matrix ReportSummaryTable",
                                print(r$ReportSummaryTable,
                                      view = FALSE,
                                      dt = FALSE))
    expect_lt(lobstr::obj_size(print(r$ReportSummaryTable,
                                     view = FALSE,
                                     dt = FALSE)),
              15 * 1024 * 1024)

    e <- structure(list(Variables = "CENTER_0", N = 2940L), # continuous vs not continuous; fully empty
              row.names = c(NA,  -1L),
              class = c("ReportSummaryTable", "data.frame"))

    expect_doppelganger2("Empty ReportSummaryTable",
                                print(e,
                                      view = FALSE,
                                      dt = FALSE))
    expect_lt(lobstr::obj_size(print(e,
                                     view = FALSE,
                                     dt = FALSE)),
              15 * 1024 * 1024)

    cont <- data.frame(Variables = letters[1:10L], N = 2940L,
                       a = sin(1:10L),
                       check.names = FALSE,
                       stringsAsFactors = FALSE)

    class(cont) <- c("ReportSummaryTable", "data.frame")

    expect_doppelganger2("ReportSummaryTable cont",
                                print(cont,
                                      view = FALSE,
                                      dt = FALSE))
    expect_lt(lobstr::obj_size(print(cont,
                                     view = FALSE,
                                     dt = FALSE)),
              15 * 1024 * 1024)
  })
})

test_that("pro-applicability matrix plots work", { # pro_applicability_matrix.R ----
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({

    r <- pro_applicability_matrix(
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
      split_segments = FALSE,
      label_col = LABEL,
      meta_data_segment = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|segment_level",
      meta_data_dataframe = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|dataframe_level")

    expect_doppelganger2("pro_applicability_matrix",
                                r$ApplicabilityPlot)
    expect_lt(lobstr::obj_size(r$ApplicabilityPlot), 15 * 1024 * 1024)

    expect_doppelganger2("pro_applicability_matrix segment v50000",
                                r$ApplicabilityPlotList$PART_QUESTIONNAIRE)
    expect_lt(lobstr::obj_size(r$ApplicabilityPlotList$PART_QUESTIONNAIRE),
                               15 * 1024 * 1024)

    expect_doppelganger2("pro_applicability_matrix ReportSummaryTable",
                                print(r$ReportSummaryTable,
                                      view = FALSE,
                                      dt = FALSE))
    expect_lt(lobstr::obj_size(print(r$ReportSummaryTable,
                                     view = FALSE,
                                     dt = FALSE)), 15 * 1024 * 1024)

  })
})

test_that("heatmap with 1 threshold plots work", { # util_heatmap_1th.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")


  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({

    x <- iris
    x$a <- as.integer(x$Species)
    x$Species2 <- rev(x$Species)
    x$b <- as.integer(x$Species2)
    x$c <- x$a + x$b
    x$str <- round(x$Sepal.Width, 0)

    p1 <- util_heatmap_1th(df = x,
                           cat_vars = c("Species", "Species2"),
                           values = "c",
                           threshold = 0,
                           invert = FALSE)

    p2 <- util_heatmap_1th(df = x,
                           cat_vars = c("Species", "Species2"),
                           strata = "str",
                           values = "c",
                           threshold = 0,
                           invert = FALSE)

    p3 <- util_heatmap_1th(df = x,
                           cat_vars = "Species",
                           values = "a",
                           threshold = 0,
                           invert = FALSE)

    expect_doppelganger2("heatmap with 2 cat-vars but w/o strata vars",
                                p1)
    # FIXME: Enable after https://github.com/r-lib/lobstr/issues/48
    # expect_lt(lobstr::obj_size(p1), 15 * 1024 * 1024)

    expect_doppelganger2("heatmap with 2 cat-vars but w/ strata vars",
                                p2)
    # FIXME: Enable after https://github.com/r-lib/lobstr/issues/48
    # expect_lt(lobstr::obj_size(p2), 15 * 1024 * 1024)

    expect_doppelganger2("heatmap with 1 cat-var",
                                p3)
    # FIXME: Enable after https://github.com/r-lib/lobstr/issues/48
    # expect_lt(lobstr::obj_size(p3), 15 * 1024 * 1024)
  })

  # FIXME: @EK: Please add more plot tests for all flavors of plots, namely, distribution, margins, maybe loess (if some path through the function is missing)
})
