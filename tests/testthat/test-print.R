test_that("print.dataquieR_resultset2 works", {
  skip_on_cran() # slow
  skip_if_not_installed("stringdist")
  prep_purge_data_frame_cache()
  study_data <- head(
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE),
    50)

  r <- dq_report2(study_data = study_data, resp_vars = "SBP_0",
                            meta_data_v2 =
               "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                            cores = NULL,
                            dimensions = "Integrity")

  expect_warning(
    print(r, cores = 3, view = FALSE, dir = tempfile()),
    regexp = "Internal problem.+should be an integer below.+in the context.+"
  )

  expect_warning(print(r, cores = list(mode = "socket",
                        logging = FALSE,
                        cpus = 2,
                        load.balancing = TRUE),
        view = FALSE, dir = tempfile()),
        regexp = "Internal problem.+should be an integer below.+in the context.+"
  )

  skip_if_not_installed("parallel")
  cl <- parallel::makePSOCKcluster(1)
  withr::defer(parallel::stopCluster(cl))
  expect_warning(print(r, cores = cl, view = FALSE, dir = tempfile()),
                 regexp = "Internal problem.+should be an integer below.+in the context.+"
  )

  r <- dq_report2(study_data = study_data, resp_vars = "SBP_0",
                  meta_data_v2 =
                    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                  filter_indicator_functions = "acc_margins",
                  cores = NULL,
                  dimensions = "Integrity")
  expect_error({
    print(r)
  }, regexp = ".*results at all\\.")

  r <- dq_report2(study_data = study_data, resp_vars = "SBP_0",
                  meta_data_v2 =
                    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx",
                  filter_indicator_functions = "con_inadmissible_categorical",
                  cores = NULL,
                  dimensions = "Consistency")
  expect_error({
    print(r)
  }, regexp = "Report is empty, no results at all.*Applicability")

})

test_that("class ReportSummaryTable", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  sd1 <- study_data
  md1 <- meta_data
  code_labels <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|missing_table")

  md1 <- prep_add_cause_label_df(md1, code_labels)

  item_miss <- com_item_missingness(study_data      = sd1,
                                    meta_data       = md1,
                                    label_col       = 'LABEL',
                                    show_causes     = TRUE,
                                    include_sysmiss = TRUE,
                                    threshold_value = 80,
                                    suppressWarnings = TRUE,
                                    drop_levels = TRUE,
                                    assume_consistent_codes = TRUE,
                                    expand_codes = TRUE
  )
  apm <- pro_applicability_matrix(study_data, meta_data)

  item_missc <- com_item_missingness(study_data      = sd1,
                                     resp_vars       = c("CENTER_0"),
                                     meta_data       = md1,
                                     label_col       = 'LABEL',
                                     show_causes     = TRUE,
                                     include_sysmiss = TRUE,
                                     threshold_value = 80,
                                     suppressWarnings = TRUE,
                                     drop_levels = !TRUE,
                                     assume_consistent_codes = TRUE,
                                     expand_codes = TRUE
  )

  item_misss <- com_item_missingness(study_data      = sd1,
                                     resp_vars       = c("SEX_0"),
                                     meta_data       = md1,
                                     label_col       = 'LABEL',
                                     show_causes     = TRUE,
                                     include_sysmiss = TRUE,
                                     threshold_value = 80,
                                     suppressWarnings = TRUE,
                                     drop_levels = !TRUE,
                                     assume_consistent_codes = TRUE,
                                     expand_codes = TRUE
  )

  item_missa <- com_item_missingness(study_data      = sd1,
                                     resp_vars       = c("AGE_0"),
                                     meta_data       = md1,
                                     label_col       = 'LABEL',
                                     show_causes     = TRUE,
                                     include_sysmiss = TRUE,
                                     threshold_value = 80,
                                     suppressWarnings = TRUE,
                                     drop_levels = !TRUE,
                                     assume_consistent_codes = TRUE,
                                     expand_codes = TRUE
  )

  item_missb <- com_item_missingness(study_data      = sd1,
                                     resp_vars       = c("SBP_0"),
                                     meta_data       = md1,
                                     label_col       = 'LABEL',
                                     show_causes     = TRUE,
                                     include_sysmiss = TRUE,
                                     threshold_value = 80,
                                     suppressWarnings = TRUE,
                                     drop_levels = !TRUE,
                                     assume_consistent_codes = TRUE,
                                     expand_codes = TRUE
  )

  item_miss_combined <- rbind(item_missa$ReportSummaryTable,
                              item_missb$ReportSummaryTable,
                              item_missc$ReportSummaryTable,
                              item_misss$ReportSummaryTable)

  expect_equal(nrow(item_miss_combined), 4)
  expect_equal(nrow(item_miss$ReportSummaryTable), 53)
  expect_s3_class(item_miss_combined, "ReportSummaryTable")
  expect_s3_class(apm$ReportSummaryTable, "ReportSummaryTable")

  item_miss_emtpy <- com_item_missingness(
     study_data      = sd1,
     resp_vars       = c("CENTER_0"),
     meta_data       = md1,
     label_col       = 'LABEL',
     show_causes     = TRUE,
     include_sysmiss = FALSE,
     threshold_value = 80,
     suppressWarnings = TRUE,
     drop_levels = TRUE,
     assume_consistent_codes = TRUE,
     expand_codes = TRUE
  )


  skip_on_cran()

  fkt2 <- function(x, size_min, size_max) {
    old <- options("viewer")
    td <- tempfile()
    stopifnot(dir.create(td))
    on.exit({
      options(old)
      unlink(td, TRUE, TRUE)
    })
    v <- function(x, ...) {
    }
    options(viewer = v)
    w <- print.ReportSummaryTable(x = x, dt = TRUE)
    expect_gt(object.size(w), size_min)
    expect_lt(object.size(w), size_max)
  }
  expect_warning(fkt2(item_miss_emtpy$ReportSummaryTable,
       size_max = 42000, size_min = 0), regexp = "Empty result")
  skip_if_not_installed("DT")
  fkt2(item_miss_combined, size_min = 50000,
       size_max = 80000)
  fkt2(item_missa$ReportSummaryTable, size_min = 40000,
       size_max = 60000)
  fkt2(item_missb$ReportSummaryTable, size_min = 50000,
       size_max = 70000)
  fkt2(item_missc$ReportSummaryTable, size_min = 40000,
       size_max = 60000)
  fkt2(item_misss$ReportSummaryTable, size_min = 40000,
       size_max = 60000)
  fkt2(item_miss$ReportSummaryTable, size_min = 80000,
       size_max = 200000)
  fkt2(apm$ReportSummaryTable, size_min = 50000,
       size_max = 200000)

  g1 <- R.devices::suppressGraphics(print(apm$ReportSummaryTable,
                                          dt = FALSE))
  g2 <- R.devices::suppressGraphics(print(item_miss$ReportSummaryTable,
                                          dt = FALSE))
  g3 <- R.devices::suppressGraphics(print(item_missa$ReportSummaryTable,
                                          dt = FALSE))
  expect_warning(R.devices::suppressGraphics(g4 <- print(item_miss_emtpy$ReportSummaryTable,
                                          dt = FALSE)), regexp = "Empty result")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # TODO: skip_if_not(capabilities()["long.double"])
  expect_doppelganger2("app-ex-repsumtab", g1)
  expect_doppelganger2("im-ex1-repsumtab", g2)
  expect_doppelganger2("im-ex2-repsumtab", g3)
  expect_warning(expect_doppelganger2("im-empty-repsumtab", g4),
    regexp = "Empty result")

})

test_that("print.interval works", {
  skip_on_cran()
  require_english_locale_and_berlin_tz()
  expect_output(
    print(util_parse_interval("(1;)")),
    "(1;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[1;)")),
    "[1;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[1;]")),
    "[1;Inf]",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("(2001-01-01;)")),
    "(2001-01-01;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[2001-01-01;)")),
    "[2001-01-01;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[2001-01-01;]")),
    "[2001-01-01;Inf]",
    fixed = TRUE
  )
})

test_that("print.ReportSummaryTable works when called within the pipeline with duplicated labels for the plot, which are not the primary labels for the report", {
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("DT")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  md0 <- meta_data
  md0$STUDY_SEGMENT <- NULL
  md0$LABEL[26:27] <- "EDUCATION"
  suppressWarningsMatching(
    dq <- dq_report2(study_data = study_data, item_level = md0, cores = NULL,
                     label_col = "LONG_LABEL",
                     filter_indicator_functions = "com_item_miss"),
    "Unique labels are required")
  # check that the VAR_NAMES attribute exists and is correct
  exp_vn <- prep_map_labels(x = c("EDUCATION_0", "EDUCATION_1"),
                            item_level = md0,
                            to = VAR_NAMES, from = LONG_LABEL)
  expect_equal(
    attr(dq$com_item_missingness.EDUCATION_0$ReportSummaryTable, "VAR_NAMES"),
    exp_vn[1]
  )
  # class(dq$com_item_missingness.EDUCATION_0$ReportSummaryTable) # is a ReportSummaryTable
  # class(dq["EDUCATION_0", "com_item_missingness", "ReportSummaryTable"]) # is a dataquieR_result
  # class(dq["EDUCATION_0", "com_item_missingness", "ReportSummaryTable"][[1]]) # is a ReportSummaryTable
  expect_equal(
    attr(dq["EDUCATION_0", "com_item_missingness", "ReportSummaryTable"][[1]], "VAR_NAMES"),
    exp_vn[1]
  )
  expect_equal(
    attr(dq[c("EDUCATION_0", "EDUCATION_1"), "com_item_missingness", "ReportSummaryTable"][[1]], "VAR_NAMES"),
    exp_vn
  )
  # check that the ReportSummaryTable can be printed
  expect_false(
    inherits(try(print(dq["EDUCATION_0", "com_item_missingness", "ReportSummaryTable"])), "try-error")
  )
  expect_false(
    inherits(try(dq[, "com_item_missingness", "ReportSummaryTable"]), "try-error")
  )
  suppressWarningsMatching({
    expect_false(
      inherits(try(print(dq[, "com_item_missingness", "ReportSummaryTable"])),
               "try-error")
    )
  }, regexps = ".*not being part of one of the segments.*")

  dq_rst <- dq[c("EDUCATION_0", "EDUCATION_1"), "com_item_missingness", "ReportSummaryTable"]
  expect_false(
    inherits(try(print(dq_rst)), "try-error")
  )
  expect_false(
    inherits(try(print(dq_rst$ReportSummaryTable)), "try-error")
  )
  expect_false(
    inherits(try(print(dq_rst[[1]])), "try-error")
  )
  # check that the expected axis labels appear in the figure
#  exp_lab <- exp_vn
#  names(exp_lab) <- rep("EDUCATION", 2)
  exp_lab <- c("v00018: EDUCATION", "v01018: EDUCATION")
#  exp_lab <- paste(names(exp_lab), paste0("(", exp_lab, ")"))
  dq_rst_plot <- ggplot_build(print(dq_rst$ReportSummaryTable))
  expect_true(
    identical(dq_rst_plot$layout$panel_params[[1]]$x$get_labels(),
      exp_lab) ||
    identical(dq_rst_plot$layout$panel_params[[1]]$y$get_labels(),
      exp_lab)
  )
})
