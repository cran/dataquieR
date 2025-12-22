test_that("default behavior returns list of resultsets", {
  skip_on_cran() # slow
  skip_if_not_installed("stringdist")
  study_data <- data.frame(id = 1:4,
                           CENTER = c("A", "A", "B", "B"),
                           val1 = c(1, NA, 3, 4),
                           val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )

  rep <- dq_report_by(study_data, meta_data, label_col = "LABEL",
                      strata_column = "CENTER", segment_column = NULL, cores = NULL)
  expect_true(is.list(rep))
  expect_named(rep$all_variables, c("Center_A", "Center_B"))
  expect_true(all(sapply(rep$all_variables, inherits, "dataquieR_resultset2")))
})

test_that("segment_column = NULL returns flat structure", {
  skip_on_cran() # slow
  skip_if_not_installed("stringdist")
  study_data <- data.frame(id = 1:4,
                           CENTER = c("A", "A", "B", "B"),
                           val1 = c(1, NA, 3, 4),
                           val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )
  rep2 <- dq_report_by(study_data, meta_data, label_col = "LABEL",
                       strata_column = "CENTER", segment_column = NULL, cores = NULL)
  expect_true(all(sapply(rep2, function(r) is.list(r) && is.null(r$segment))))
})

test_that("disable_plotly disables plotly in HTML output", {
  skip_on_cran() # slow

  skip_if_not_installed("DT")

  study_data <- data.frame(id = 1:4,
                           CENTER = c("A", "A", "B", "B"),
                           val1 = c(1, NA, 3, 4),
                           val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )
  td <- tempfile()
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  result <- dq_report_by(study_data, meta_data, label_col = "LABEL",
                         strata_column = "CENTER",
                         output_dir = td, also_print = TRUE, view = FALSE,
                         disable_plotly = TRUE, segment_column = NULL,
                         cores = NULL)
  html_files <- list.files(td, pattern = "\\.html$", full.names = TRUE,
                           all.files = TRUE,
                           recursive = TRUE)
  expect_gt(length(html_files), 0)
  html_contents <- paste(unlist(lapply(html_files,
    function(f) readLines(f, warn = FALSE))), collapse = "\n")

  expect_false(grepl("plotly",
                     gsub("disable_plotly", "", html_contents, fixed = TRUE),
                     fixed = TRUE))
})

test_that("also_print writes html files and returns a result list", {
  skip_on_cran() # slow

  skip_if_not_installed("DT")
  study_data <- data.frame(id = 1:4,
                           CENTER = c("A", "A", "B", "B"),
                           val1 = c(1, NA, 3, 4),
                           val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )
  td <- tempfile()
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  result <- dq_report_by(study_data, meta_data, label_col = "LABEL",
                         strata_column = "CENTER",
                         output_dir = td, also_print = TRUE, segment_column = NULL, cores = NULL)
  expect_type(result, "list")
  html_files <- list.files(td, pattern = "\\.html$")
  expect_gt(length(html_files), 0)
})

test_that("missing label_col is handled gracefully", {
  skip_on_cran() # slow

  skip_if_not_installed("stringdist")
  study_data <- data.frame(id = 1:4,
                           CENTER = c("A", "A", "B", "B"),
                           val1 = c(1, NA, 3, 4),
                           val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )

  result <- dq_report_by(
    study_data, meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    segment_column = NULL,
    cores = NULL
  )

  expect_type(result, "list")
})
