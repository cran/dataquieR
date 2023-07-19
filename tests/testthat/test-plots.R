test_that("distribution plot works", { # acc_distributions.R ----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    group_vars <- prep_map_labels("DBP_0", meta_data = "meta_data", from = LABEL,
                                  to = GROUP_VAR_OBSERVER)
    r <-
      acc_distributions(
        resp_vars = "DBP_0",
        study_data = "study_data",
        meta_data = "meta_data",
        group_vars = group_vars,
        label_col = LABEL)

    vdiffr::expect_doppelganger("acc_distributions gr dbp0", r$SummaryPlotList$DBP_0)

    r <-
      acc_distributions(
        resp_vars = "EDUCATION_0",
        study_data = "study_data",
        meta_data = "meta_data",
        label_col = LABEL)

    vdiffr::expect_doppelganger("acc_distributions def edu0", r$SummaryPlotList$EDUCATION_0)

  })
})

test_that("loess plot works", { # acc_loess.R ----
  # testthat::local_reproducible_output()
  # skip_on_travis() # vdiffr fails
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }

  ({
    time_vars <- prep_map_labels("DBP_0", meta_data = "meta_data", from = LABEL,
                                 to = TIME_VAR)
    group_vars <- prep_map_labels("DBP_0", meta_data = "meta_data", from = LABEL,
                                  to = GROUP_VAR_OBSERVER)
    r <-
      acc_loess(  resp_vars = "DBP_0",
                  study_data = "study_data",
                  meta_data = "meta_data",
                  group_vars = group_vars,
                  time_vars = time_vars,
                  label_col = LABEL,
                  co_vars = "AGE_0")

    vdiffr::expect_doppelganger("loess def dbp0", r$SummaryPlotList$DBP_0)

    r <-
      acc_loess(  resp_vars = "EDUCATION_0",
                  study_data = "study_data",
                  meta_data = "meta_data",
                  time_vars = time_vars,
                  group_vars = group_vars,
                  label_col = LABEL,
                  co_vars = "AGE_0")

    vdiffr::expect_doppelganger("loess def edu0", r$SummaryPlotList$EDUCATION_0)

    r <-
      acc_loess(  resp_vars = "DBP_0",
                  study_data = "study_data",
                  meta_data = "meta_data",
                  group_vars = group_vars,
                  time_vars = time_vars,
                  plot_format = "FACETS",
                  label_col = LABEL,
                  co_vars = "AGE_0")

    vdiffr::expect_doppelganger("loess fac dbp0", r$SummaryPlotList$DBP_0)

    r <-
      acc_loess(  resp_vars = "EDUCATION_0",
                  study_data = "study_data",
                  meta_data = "meta_data",
                  time_vars = time_vars,
                  group_vars = group_vars,
                  plot_format = "FACETS",
                  label_col = LABEL,
                  co_vars = "AGE_0")

    vdiffr::expect_doppelganger("loess fac edu0", r$SummaryPlotList$EDUCATION_0)

  })
})

test_that("margins plot works", { # acc_margins.R -----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  ({
    group_vars <- prep_map_labels("DBP_0", meta_data = "meta_data", from = LABEL,
                                  to = GROUP_VAR_OBSERVER)
    r <-
      acc_margins(resp_vars = "DBP_0",
                  study_data = "study_data",
                  meta_data = "meta_data",
                  group_vars = group_vars,
                  label_col = LABEL,
                  co_vars = "AGE_0")

    vdiffr::expect_doppelganger("margins dbp0", r$SummaryPlot)

    r <-
      acc_margins(resp_vars = "EDUCATION_0",
                  study_data = "study_data",
                  meta_data = "meta_data",
                  group_vars = group_vars,
                  label_col = LABEL,
                  co_vars = "AGE_0")

    vdiffr::expect_doppelganger("margins edu0", r$SummaryPlot)
  })
})

test_that("multivariate outlier plot works", { # acc_multivariate_outlier.R ----
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  withr::with_seed(32245253, {
    r <-
      acc_multivariate_outlier(
        variable_group = c("DBP_0", "SBP_0", "AGE_0"),
        label_col = LABEL,
        study_data = "study_data",
        meta_data = "meta_data")

    vdiffr::expect_doppelganger("acc_multivariate_outlier test 1",
                                r$SummaryPlot)

  })
})


test_that("shape or scale plot works", { # acc_shape_or_scale.R -----
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  ({
    r <-
      acc_shape_or_scale(
        resp_vars = "DBP_0",
        study_data = "study_data",
        meta_data = "meta_data",
        label_col = LABEL)

    vdiffr::expect_doppelganger("shape or scale dbp0", r$SummaryPlot)
  })
})

test_that("univariate outlier plot works", { # acc_univariate_outlier.R ----
  skip_on_cran()
  skip_if_not_installed("withr")
  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  withr::with_seed(32245253, {
    r <-
      acc_univariate_outlier(
        resp_vars = c("DBP_0", "DEV_NO_0"),
        label_col = LABEL,
        study_data = "study_data",
        meta_data = "meta_data")

    vdiffr::expect_doppelganger("acc_univariate_outlier.R DBP_0",
                                r$SummaryPlotList$DBP_0)

    vdiffr::expect_doppelganger("acc_univariate_outlier.R DEV_NO_0",
                                r$SummaryPlotList$DEV_NO_0)

  })
})

test_that("old contradiction plots work", { # con_contradictions.R ----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    check_table <- read.csv(system.file("extdata",
        "contradiction_checks.csv",
        package = "dataquieR"
      ),
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
    r <-
      con_contradictions(
        study_data = "study_data", meta_data = "meta_data", label_col =
          label_col,
        threshold_value = threshold_value, check_table = check_table,
        summarize_categories = TRUE
      )
    vdiffr::expect_doppelganger("con_contradictions by tag",
                                r$SummaryPlot)

    vdiffr::expect_doppelganger("con_contradictions logical age checks, only",
                                r$`Logical, Age-Related`$SummaryPlot)

    vdiffr::expect_doppelganger("con_contradictions all checks",
                                r$all_checks$SummaryPlot)

  })
})

test_that("redcap based contradiction plots work", { # con_contradictions_redcap.R ----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    label_col <- "LABEL"
    threshold_value <- 1
    r <- con_contradictions_redcap(
      study_data = "study_data", meta_data = "meta_data", label_col = label_col,
      threshold_value = threshold_value, meta_data_cross_item =
        "meta_data_v2|cross-item_level",
      summarize_categories = TRUE
    )

    vdiffr::expect_doppelganger("con_contradictions rc by tag",
                                r$SummaryPlot)

    vdiffr::expect_doppelganger("con_contradictions rc logical checks, only",
                                r$LOGICAL$SummaryPlot)

    vdiffr::expect_doppelganger("con_contradictions rc all checks",
                                r$all_checks$SummaryPlot)

  })
})

test_that("limit deviation plots work", { # con_limit_deviations.R ----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    label_col <- "LABEL"
    threshold_value <- 1
    meta_data <- prep_get_data_frame("meta_data")
    meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
      "[2018-01-01 00:00:00 CET; 2022-12-12 23:59:59 CET)"

    r1 <- con_limit_deviations( # all limits exist
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "study_data", meta_data = meta_data,
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
      study_data = "study_data", meta_data = meta_data,
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
      study_data = "study_data", meta_data = meta_data,
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

    vdiffr::expect_doppelganger("con_limit_deviations all sbp_0",
                                r1$SummaryPlotList$SBP_0)

    vdiffr::expect_doppelganger("con_limit_deviations all item",
                                r1$SummaryPlotList$ITEM_1_0)

    vdiffr::expect_doppelganger("con_limit_deviations all quest_dt",
                                r1$SummaryPlotList$QUEST_DT_0)

    vdiffr::expect_doppelganger("con_limit_deviations low sbp_0",
                                r2$SummaryPlotList$SBP_0)

    vdiffr::expect_doppelganger("con_limit_deviations low item",
                                r2$SummaryPlotList$ITEM_1_0)

    vdiffr::expect_doppelganger("con_limit_deviations low quest_dt",
                                r2$SummaryPlotList$QUEST_DT_0)

    vdiffr::expect_doppelganger("con_limit_deviations upp sbp_0",
                                r3$SummaryPlotList$SBP_0)

    vdiffr::expect_doppelganger("con_limit_deviations upp item",
                                r3$SummaryPlotList$ITEM_1_0)

    vdiffr::expect_doppelganger("con_limit_deviations upp quest_dt",
                                r3$SummaryPlotList$QUEST_DT_0)


  })
})

test_that("data type matrix and print.ReportSummaryTable plots work", {
  # int_datatype_matrix.R and print.ReportSummaryTable.R ----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    label_col <- "LABEL"

    r <- int_datatype_matrix(
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "study_data", meta_data = "meta_data",
      label_col = label_col,
      split_segments = TRUE
    )

    vdiffr::expect_doppelganger("int_datatype_matrix",
                                r$SummaryPlot)

    vdiffr::expect_doppelganger("int_datatype_matrix segment v50000",
                                r$DataTypePlotList$v50000)

    vdiffr::expect_doppelganger("int_datatype_matrix ReportSummaryTable",
                                print(r$ReportSummaryTable,
                                      view = FALSE,
                                      dt = FALSE))

    e <- structure(list(Variables = "CENTER_0", N = 2940L), # continuous vs not continuous; fully empty
              row.names = c(NA,  -1L),
              class = c("ReportSummaryTable", "data.frame"))

    vdiffr::expect_doppelganger("Empty ReportSummaryTable",
                                print(e,
                                      view = FALSE,
                                      dt = FALSE))

    cont <- data.frame(Variables = letters[1:10L], N = 2940L,
                       a = sin(1:10L),
                       check.names = FALSE,
                       stringsAsFactors = FALSE)

    class(cont) <- c("ReportSummaryTable", "data.frame")

    vdiffr::expect_doppelganger("ReportSummaryTable cont",
                                print(cont,
                                      view = FALSE,
                                      dt = FALSE))

  })
})

test_that("pro-applicability matrix plots work", { # pro_applicability_matrix.R ----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({

    r <- pro_applicability_matrix(
      study_data = "study_data",
      meta_data = "meta_data",
      split_segments = FALSE,
      label_col = LABEL,
      meta_data_segment = "meta_data_v2|segment_level",
      meta_data_dataframe = "meta_data_v2|dataframe_level")

    vdiffr::expect_doppelganger("pro_applicability_matrix",
                                r$ApplicabilityPlot)

    vdiffr::expect_doppelganger("pro_applicability_matrix segment v50000",
                                r$ApplicabilityPlotList$v50000)

    vdiffr::expect_doppelganger("pro_applicability_matrix ReportSummaryTable",
                                print(r$ReportSummaryTable,
                                      view = FALSE,
                                      dt = FALSE))
  })
})

test_that("heatmap with 1 threshold plots work", { # util_heatmap_1th.R ----
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("withr")
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

    vdiffr::expect_doppelganger("heatmap with 2 cat-vars but w/o strata vars",
                                p1)

    vdiffr::expect_doppelganger("heatmap with 2 cat-vars but w/ strata vars",
                                p2)

    vdiffr::expect_doppelganger("heatmap with 1 cat-var",
                                p3)
  })
})
