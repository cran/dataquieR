test_that("com_item_missingness works", {
  skip_on_cran() # slow and errors will be obvious.
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  md0 <- meta_data
  md0$STUDY_SEGMENT <- NULL
  expect_message(invisible(
    com_item_missingness(study_data, md0, suppressWarnings = TRUE)
  ))

  expect_equal(com_item_missingness(resp_vars = "v00001", study_data, md0,
                                    suppressWarnings = TRUE,
                                    threshold_value = 100,
                                    include_sysmiss = FALSE,
                                    drop_levels = TRUE,
                                    assume_consistent_codes = TRUE,
                                    expand_codes = TRUE,
                                    show_causes = TRUE,
                                    expected_observations =
                                      "ALL")$SummaryTable$GRADING,
               0)

  skip_on_cran() # too many tests make things slow too.

  expect_error(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         include_sysmiss = 1:10)
  ), regexp = "Need excactly one element in argument include_sysmiss")

  w <- capture_warnings(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         include_sysmiss = TRUE)
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl("include_sysmiss", w)))

  w <- capture_warnings(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         include_sysmiss = FALSE)
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl("include_sysmiss", w)))

  expect_message(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE)
  ), regexp =
    paste("The mandatory argument threshold_value was",
          "not defined and is set to the default of 90%."))

  expect_error(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = NA)
  ), regexp = "Argument suppressWarnings must not contain NAs")

  expect_error(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = "xx")
  ), regexp =
    paste("Argument",
          "suppressWarnings must match the predicate .+is.logical.+"))

  expect_error(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = 1:2)
  ), regexp = "Need excactly one element in argument suppressWarnings, got 2")

  w <- capture_warnings(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = TRUE)
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl("Setting suppressWarnings to its default FALSE", w
        )))

  expect_error(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = c(TRUE,
                                                                   FALSE))
  ), regexp = "Need excactly one element in argument suppressWarnings, got 2")


  w <- capture_warnings(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = c(TRUE))
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl("Setting suppressWarnings to its default FALSE",
                         w)))


  expect_error(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = TRUE,
                         cause_label_df = 42, threshold_value = .9)
  ), regexp =
    paste(".+cause_label_df.+ is not a data frame."))

  expect_message(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = TRUE,
                         threshold_value = "XX")
  ), regexp = paste(
    "Could not convert threshold_value .+XX.+ to a number.",
    "Set to default value 90%."), perl = TRUE)

  cause_label_df <- prep_get_data_frame("meta_data_v2|missing_table")

  cause_label_df$CODE_VALUE[16] <-
    cause_label_df$CODE_VALUE[15]
  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = FALSE,
                         cause_label_df = cause_label_df)
  ), regexp =
    "code.* with more than one meaning")

  i1 <- expect_warning(com_item_missingness(study_data, meta_data),
                       regexp =
                         sprintf("%s|%s|%s|%s|%s|%s|%s|%s|%s|%s",
                                 paste("Would use label .+ for all values",
                                       "coded with .+"),
                                 paste("The mandatory argument threshold_value",
                                       "was not defined and is set to the",
                                       "default of 90%."),
                                 paste(".*more than one meaning.*"),
                                 paste("include_sysmiss set to FALSE"),
                                 paste("assume_consistent_codes set to TRUE"),
                                 paste("expand_codes set to TRUE"),
                                 paste("show_causes set to TRUE"),
                                 paste("Setting .+ to",
                                       "its default (FALSE|TRUE)"),
                                 paste("Some code labels or -values",
                                       "are missing from .+meta_data.+."),
                                 paste("There are \\d+ meassurements",
                                       "of .+",
                                       "for participants not being",
                                       "part of one of the segments .+")
                         ),
                       perl = TRUE,
                       all = TRUE)
  expect_lt(suppressWarnings(
    abs(
      sum(as.numeric(as.matrix(i1$SummaryTable)), na.rm = TRUE) -
        154939)), 50)

  code_labels <- prep_get_data_frame("meta_data_v2|missing_table")
  i2 <- expect_warning(com_item_missingness(study_data, meta_data,
                                            cause_label_df = code_labels),
                       regexp =
                         sprintf("%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s",
                                 paste("Would use label .+ for all values",
                                       "coded with .+"),
                                 paste("The mandatory argument threshold_value",
                                       "was not defined and is set to the",
                                       "default of 90%."),
                                 paste(".*more than one meaning.*"),
                                 paste("include_sysmiss set to FALSE"),
                                 paste("assume_consistent_codes set to TRUE"),
                                 paste("expand_codes set to TRUE"),
                                 paste("show_causes set to TRUE"),
                                 paste("Setting .+ to",
                                       "its default (FALSE|TRUE)"),
                                 paste("Some code labels or -values",
                                       "are missing from .+meta_data.+."),
                                 paste("Found jump/missing codes in .+ not",
                                       "mentioned in .+"),
                                 paste("There are \\d+ meassurements",
                                       "of .+",
                                       "for participants not being",
                                       "part of one of the segments .+"),
                                 paste("Expand label .+ for all values",
                                       "coded with .+"),
                                 paste("Combining .+ and assignments in .+ in",
                                       ".+ is discouraged. This may cause",
                                       "errors."),
                                 paste("The argument .+cause_label_df.+ has been",
                                       "deprecated. It",
                                       "will be in a future",
                                       "version be removed.")
                         ),
                       perl = TRUE,
                       all = TRUE)
  expect_lt(suppressWarnings(
    abs(
      sum(as.numeric(as.matrix(i2$SummaryTable)), na.rm = TRUE) -
        154939)), 50)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("item missingness plot without labels ok",
                              i1$SummaryPlot)
  vdiffr::expect_doppelganger("item missingness plot with labels ok",
                              i2$SummaryPlot)
})
