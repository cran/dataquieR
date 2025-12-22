test_that("prep_render_pie_chart_from_summaryclasses_ggplot2 works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran()
  skip_if_not_installed("stringdist")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE)
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData")
  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL

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

  suppressWarnings({
    summary <- prep_extract_summary(report)
    classes <- prep_summary_to_classes(summary)
  })

  plot_tab <- classes %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(VAR_NAMES, "call_names")))) %>%
    dplyr::summarise(class =
                       suppressWarnings(
                         util_as_cat(max(util_as_cat(class), na.rm = TRUE))))

  suppressWarnings(sum_plot_tab <- plot_tab %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("class", "call_names")))) %>%
    dplyr::summarise(value = length(VAR_NAMES),
                     note = util_pretty_vector_string(
                       n_max = 3,
                       suppressWarnings(prep_get_labels(VAR_NAMES,
                                                        max_len = 15,
                                                        label_class = "SHORT",
                                                        meta_data =
                                                          this$meta_data)))))



  suppressWarnings(
    r <- prep_render_pie_chart_from_summaryclasses_ggplot2(sum_plot_tab,
                                                      meta_data = md0))
  r <- as.character(r)
  # expect_snapshot_value(r, style = "deparse") snapshots do not work, here. the plots differ (like the label is on the test platform on top locally at the bottom of the big pie-piece)
  # do a very unspecific verification:

  expect_gt(nchar(r), 70000)

})
