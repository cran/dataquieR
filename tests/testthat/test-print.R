test_that("print.dataquieR_resultset works", {
  skip_on_cran() # slow, deprecated
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  skip_on_cran() # slow test
  skip_if_not_installed("flexsiteboard")
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report
  # here in the first place
  report <- suppressWarnings(dq_report(study_data, meta_data,
                                       cores = 1,
                                       label_col = LABEL,
                                       dimensions = # for speed, omit Accuracy
                                         c("Completeness",
                                           "Consistency"),
                                       check_table =
                                         read.csv(system.file(
                                           "extdata",
                                           "contradiction_checks.csv",
                                           package = "dataquieR"
                                         ), header = TRUE, sep = "#"),
                                       show_causes = TRUE,
                                       cause_label_df = prep_get_data_frame(
                                         "meta_data_v2|missing_table")
  ))
  env <- new.env(parent = emptyenv())
  v <- function(x, ...) env$file <- x
  fkt <- function(report, v) {
    old <- options("viewer")
    on.exit(options(old))
    options(viewer = v)
    print(report, self_contained = TRUE)
  }
  empty_report <- as.data.frame.dataquieR_resultset(list(long_format = list()))
  class(empty_report) <- "dataquieR_resultset"
  expect_error(print(empty_report),
               regexp = "Report is empty, no results at all.")
  fkt2 <- function(report, v) {
    old <- options("viewer")
    td <- tempfile()
    stopifnot(dir.create(td))
    on.exit({
      options(old)
      unlink(td, TRUE, TRUE)
    })
    options(viewer = v)
    tmpl <- file.path(td, "dummy_report.Rmd")
    cat('---
title: "dataquieR report"
author: "`r Sys.info()[[\'user\']]`"
date: "`r format(Sys.time(), \'%d %B, %Y\')`"
output:
  flexsiteboard::flex_site_board:
    storyboard: true
    vertical_layout: "scroll"
    orientation: "columns"
  html_document:
    toc: true
    toc_depth: 2
params:
  debug: false
---

+--------------+----------------------+
| Template     | `r template`         |
+--------------+----------------------+
| Package Name | pn=`r packageName`   |
+--------------+----------------------+
| Echo         | cc=`r chunk_echo`    |
+--------------+----------------------+
| Warnings     | cw=`r chunk_warning` |
+--------------+----------------------+
| Error        | ce=`r chunk_error`   |
+--------------+----------------------+
| Message      | cm=`r chunk_message` |
+--------------+----------------------+

```{r}
print(sprintf("lllll=%d=lllll", length(report)))
print(sprintf("ccccc=%s=ccccc", class(report)))
```

# Empty

', file = tmpl)
    .a <- readLines(print(report, template = tmpl, self_contained = TRUE))
    .b <- readLines(print(report, chunk_echo = TRUE, template = tmpl,
                          self_contained = TRUE))
    .c <- readLines(print(report, chunk_error = TRUE, template = tmpl,
                          self_contained = TRUE))
    .d <- readLines(print(report, chunk_message = TRUE, template = tmpl,
                          self_contained = TRUE))
    .e <- readLines(print(report, chunk_warning = TRUE, template = tmpl,
                          self_contained = TRUE))
    .f <- readLines(print(report, output_format = NULL, template = tmpl,
                          self_contained = TRUE))
    expect_error(print(report, output_format = NULL, template = 42),
                 regexp = "template must be a character.1.", perl = TRUE)
    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_echo = 42),
                 regexp = "chunk_echo must be a logical.1.", perl = TRUE)
    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_echo = c(TRUE, FALSE, TRUE, FALSE)),
                 regexp = "chunk_echo must be a logical.1.", perl = TRUE)

    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_error = 42),
                 regexp = "chunk_error must be a logical.1.", perl = TRUE)
    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_error = c(TRUE, FALSE, TRUE, FALSE)),
                 regexp = "chunk_error must be a logical.1.", perl = TRUE)

    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_message = 42),
                 regexp = "chunk_message must be a logical.1.", perl = TRUE)
    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_message = c(TRUE, FALSE, TRUE, FALSE)),
                 regexp = "chunk_message must be a logical.1.", perl = TRUE)

    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_warning = 42),
                 regexp = "chunk_warning must be a logical.1.", perl = TRUE)
    expect_error(print(report, output_format = NULL, template = tmpl,
                       chunk_warning = c(TRUE, FALSE, TRUE, FALSE)),
                 regexp = "chunk_warning must be a logical.1.", perl = TRUE)
    expect_true(any(grepl("lllll=9=lllll", .a)))
    expect_true(any(grepl("ccccc=dataquieR_resultset=ccccc", .a)))
    expect_true(any(grepl("pn=dataquieR", .a)))
    expect_true(any(grepl("cc=FALSE", .a)))
    expect_true(any(grepl("ce=FALSE", .a)))
    expect_true(any(grepl("cw=FALSE", .a)))
    expect_true(any(grepl("cm=FALSE", .a)))

    expect_true(any(grepl("lllll=9=lllll", .b)))
    expect_true(any(grepl("ccccc=dataquieR_resultset=ccccc", .b)))
    expect_true(any(grepl("pn=dataquieR", .b)))
    expect_true(any(grepl("cc=TRUE", .b)))
    expect_true(any(grepl("ce=FALSE", .b)))
    expect_true(any(grepl("cw=FALSE", .b)))
    expect_true(any(grepl("cm=FALSE", .b)))

    expect_true(any(grepl("lllll=9=lllll", .c)))
    expect_true(any(grepl("ccccc=dataquieR_resultset=ccccc", .c)))
    expect_true(any(grepl("pn=dataquieR", .c)))
    expect_true(any(grepl("cc=FALSE", .c)))
    expect_true(any(grepl("ce=TRUE", .c)))
    expect_true(any(grepl("cw=FALSE", .c)))
    expect_true(any(grepl("cm=FALSE", .c)))

    expect_true(any(grepl("lllll=9=lllll", .d)))
    expect_true(any(grepl("ccccc=dataquieR_resultset=ccccc", .d)))
    expect_true(any(grepl("pn=dataquieR", .d)))
    expect_true(any(grepl("cc=FALSE", .d)))
    expect_true(any(grepl("ce=FALSE", .d)))
    expect_true(any(grepl("cw=FALSE", .d)))
    expect_true(any(grepl("cm=TRUE", .d)))

    expect_true(any(grepl("lllll=9=lllll", .e)))
    expect_true(any(grepl("ccccc=dataquieR_resultset=ccccc", .e)))
    expect_true(any(grepl("pn=dataquieR", .e)))
    expect_true(any(grepl("cc=FALSE", .e)))
    expect_true(any(grepl("ce=FALSE", .e)))
    expect_true(any(grepl("cw=TRUE", .e)))
    expect_true(any(grepl("cm=FALSE", .e)))

    expect_true(any(grepl("lllll=9=lllll", .f)))
    expect_true(any(grepl("ccccc=dataquieR_resultset=ccccc", .f)))
    expect_true(any(grepl("pn=dataquieR", .f)))
    expect_true(any(grepl("cc=FALSE", .f)))
    expect_true(any(grepl("ce=FALSE", .f)))
    expect_true(any(grepl("cw=FALSE", .f)))
    expect_true(any(grepl("cm=FALSE", .f)))

  }
  expect_warning({
    fkt2(report, v)
    f <- fkt(report, v)
  }, regexp = "(No output directory given.*|self-contained = TRUE.*)")
  expect_equal(f, env$file)
  skip_on_cran()
  expect_gt(file.size(env$file), 5000000)
})

test_that("class ReportSummaryTable", {
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  sd1 <- study_data
  md1 <- meta_data
  code_labels <- prep_get_data_frame(
    "meta_data_v2|missing_table")

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
  fkt2(item_miss_combined, size_min = 50000,
       size_max = 70000)
  fkt2(item_missa$ReportSummaryTable, size_min = 40000,
       size_max = 50000)
  fkt2(item_missb$ReportSummaryTable, size_min = 50000,
       size_max = 70000)
  fkt2(item_missc$ReportSummaryTable, size_min = 40000,
       size_max = 50000)
  fkt2(item_misss$ReportSummaryTable, size_min = 40000,
       size_max = 50000)
  fkt2(item_miss$ReportSummaryTable, size_min = 80000,
       size_max = 200000)
  fkt2(apm$ReportSummaryTable, size_min = 50000,
       size_max = 100000)

  g1 <- R.devices::suppressGraphics(print(apm$ReportSummaryTable,
                                          dt = FALSE))
  g2 <- R.devices::suppressGraphics(print(item_miss$ReportSummaryTable,
                                          dt = FALSE))
  g3 <- R.devices::suppressGraphics(print(item_missa$ReportSummaryTable,
                                          dt = FALSE))
  g4 <- expect_warning(R.devices::suppressGraphics(print(item_miss_emtpy$ReportSummaryTable,
                                          dt = FALSE)), regexp = "Empty result")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(capabilities()["long.double"])
  vdiffr::expect_doppelganger("app-ex-repsumtab", g1)
  vdiffr::expect_doppelganger("im-ex1-repsumtab", g2)
  vdiffr::expect_doppelganger("im-ex2-repsumtab", g3)
  expect_warning(vdiffr::expect_doppelganger("im-empty-repsumtab", g4),
    regexp = "Empty result")

})

test_that("print.interval works", {
  for (i in 1:2) {
    # This command failed in the first try, but worked in the second try for me.
    suppressWarnings(withr::local_locale(c(LC_TIME = "en_US.UTF-8")))
    # Linux, macOS
  }
  if (Sys.getlocale("LC_TIME") != "en_US.UTF-8") {
    withr::local_locale(c(LC_TIME = "English.UTF-8")) # Windows
  }
  withr::local_timezone("Europe/Berlin")
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
