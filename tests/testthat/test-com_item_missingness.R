test_that("com_item_missingness works", {
  load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
         environment())
  load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
         environment())

  md0 <- meta_data
  md0$KEY_STUDY_SEGMENT <- NULL
  expect_warning(invisible(
    com_item_missingness(study_data, md0, suppressWarnings = TRUE)
  ))

  expect_false(any(grepl("show_causes", capture_warnings(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         show_causes = FALSE)
  )))))

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         show_causes = c(TRUE, FALSE))
  ), regexp = "show_causes cannot be a vector. Set it to TRUE")

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE)
  ), regexp = "show_causes set to TRUE")

  skip_on_cran() # too many tests make things slow too.

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data,
                         show_causes = "invalid")
  ), regexp = "Cannot parse show_causes as a logical value. Set it to TRUE")

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         include_sysmiss = 1:10)
  ), regexp = "include_sysmiss cannot be a vector. Set it to FALSE")

  expect_false(any(grepl("include_sysmiss", capture_warnings(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         include_sysmiss = TRUE)
  )))))

  expect_false(any(grepl("include_sysmiss", capture_warnings(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE,
                         include_sysmiss = FALSE)
  )))))

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data,
                         suppressWarnings = TRUE)
  ), regexp = "include_sysmiss set to FALSE")

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = NA)
  ), regexp = "Setting suppressWarnings to its default FALSE")

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = "xx")
  ), regexp = "Setting suppressWarnings to its default FALSE")

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = 1:2)
  ), regexp = "Setting suppressWarnings to its default FALSE")

  expect_false(any(grepl("Setting suppressWarnings to its default FALSE",
        capture_warnings(invisible(
      com_item_missingness(study_data, meta_data, suppressWarnings = TRUE)
    )))))

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = c(TRUE,
                                                                   FALSE))
  ), regexp = "Setting suppressWarnings to its default FALSE")

  expect_false(any(grepl("Setting suppressWarnings to its default FALSE",
                         capture_warnings(invisible(
       com_item_missingness(study_data, meta_data, suppressWarnings = c(TRUE))
    )))))


  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = TRUE,
                         cause_label_df = 42)
  ), regexp =
    "If given, cause_label_df must be a data frame. Ignored the argument.")

  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = TRUE,
                         threshold_value = "XX")
  ), regexp = paste(
    "Could not convert threshold_value .+XX.+ to a number.",
    "Set to default value 90%."), perl = TRUE)

  cause_label_df <- read.csv(
    system.file("extdata", "Missing-Codes-2020.csv", package = "dataquieR"),
    header = TRUE, sep = ";"
  )

  cause_label_df$CODE_VALUE[16] <-
    cause_label_df$CODE_VALUE[15]
  expect_warning(invisible(
    com_item_missingness(study_data, meta_data, suppressWarnings = FALSE,
                         cause_label_df = cause_label_df)
  ), regexp =
    "There are codes used for missings and jumps.")

  i1 <- expect_warning(com_item_missingness(study_data, meta_data),
                       regexp =
                         sprintf("%s|%s|%s|%s",
                                 paste("The mandatory argument threshold_value",
                                       "was not defined and is set to the",
                                       "default of 90%."),
                                 paste("include_sysmiss set to FALSE"),
                                 paste("show_causes set to TRUE"),
                                 paste("Setting suppressWarnings to",
                                       "its default FALSE")
                         ),
                       perl = TRUE,
                       all = TRUE)
  expect_lt(suppressWarnings(
    abs(
      sum(as.numeric(as.matrix(i1$SummaryTable)), na.rm = TRUE) -
        154883)), 50)

  code_labels <- read.csv2(system.file("extdata",
                                       "Missing-Codes-2020.csv",
                                       package = "dataquieR"),
                           stringsAsFactors = FALSE, na.strings = c())
  i2 <- expect_warning(com_item_missingness(study_data, meta_data,
                                            cause_label_df = code_labels),
                       regexp =
                         sprintf("%s|%s|%s|%s",
                                 paste("The mandatory argument threshold_value",
                                       "was not defined and is set to the",
                                       "default of 90%."),
                                 paste("include_sysmiss set to FALSE"),
                                 paste("show_causes set to TRUE"),
                                 paste("Setting suppressWarnings to",
                                       "its default FALSE")
                         ),
                       perl = TRUE,
                       all = TRUE)
  expect_lt(suppressWarnings(
    abs(
      sum(as.numeric(as.matrix(i2$SummaryTable)), na.rm = TRUE) -
        154883)), 50)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("item missingness plot without labels ok",
                              i1$SummaryPlot)
  vdiffr::expect_doppelganger("item missingness plot with labels ok",
                              i2$SummaryPlot)
})
