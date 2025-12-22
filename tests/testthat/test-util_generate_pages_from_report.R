# tests/testthat/test-util_generate_pages_from_report-coverage.R

.get_mini_report <- function() {
  env <- testthat::test_env()

  if (!is.null(env$.mini_report)) {
    return(env$.mini_report)
  }

  skip_on_cran() # slow, errors unlikely
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  dataquieR::prep_load_workbook_like_file(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx"
  )

  withr::defer(
    dataquieR::prep_purge_data_frame_cache(),
    envir = env
  )

  env$.mini_report <- dataquieR::dq_report2(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData",
    resp_vars = c("SEX_0", "AGE_0"),
    dimensions = c("int"),
    label_col = LABEL,
    cores = NULL
  )

  env$.mini_report
}

.call_pages <- function(report, ...) {
  dataquieR:::util_generate_pages_from_report( # nolint
    report = report,
    template = "default",
    disable_plotly = TRUE,

    # required + avoid recursive defaults
    progress = function(pct) invisible(NULL),
    progress_msg = function(...) invisible(NULL),

    block_load_factor = 1,
    dir = tempdir(),
    my_dashboard = NULL,

    ...
  )
}

test_that("util_generate_pages_from_report(): renders pages for a mini report", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  pages <- .call_pages(report)
  expect_true(is.list(pages))
  expect_true(length(pages) >= 1)

  html <- paste(unlist(pages, recursive = TRUE), collapse = "\n")
  expect_true(nchar(html) > 0)
})

test_that("util_generate_pages_from_report(): label modifications section (if present) is rendered", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  pages <- .call_pages(report)
  html <- paste(unlist(pages, recursive = TRUE), collapse = "\n")

  txt <- attr(report, "label_modification_text")
  if (!is.null(txt) && length(txt) == 1L && nchar(txt) > 0) {
    expect_match(html, "Label modifications", fixed = TRUE)
  } else {
    expect_true(TRUE) # branch executed, but nothing to assert
  }
})

test_that("util_generate_pages_from_report(): integrity issues before pipeline (if present) are rendered", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  pages <- .call_pages(report)
  html <- paste(unlist(pages, recursive = TRUE), collapse = "\n")

  issues <- attr(report, "integrity_issues_before_pipeline")
  if (!is.null(issues) && length(issues) > 0) {
    expect_match(html, "Integrity", fixed = FALSE)
  } else {
    expect_true(TRUE)
  }
})

test_that("util_generate_pages_from_report(): falls back when properties are missing", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  # modify a copy to avoid polluting the cached object
  report2 <- report
  attr(report2, "properties") <- NULL

  pages <- .call_pages(report2)
  html <- paste(unlist(pages, recursive = TRUE), collapse = "\n")

  expect_match(html, "No report properties found", fixed = TRUE)
})

test_that("util_generate_pages_from_report(): errors if parallelMap.mode is not local", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()
  withr::local_options(list(parallelMap.mode = "snow"))

  expect_error(
    .call_pages(report),
    "parallel rendering of reports is not supported",
    fixed = TRUE
  )
})
