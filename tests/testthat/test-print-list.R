test_that("print.list() falls back via NextMethod() for empty lists", {
  # print.default(list()) returns the object invisibly
  expect_invisible(print(list()))
})

test_that("print.list() falls back via NextMethod() during knitr", {
  withr::local_options(knitr.in.progress = TRUE)

  # If this branch is taken, the heavy code must NOT run; we guard it by mocking
  testthat::local_mocked_bindings(
    util_is_try_error = function(...) stop("should not be called"),
    util_ensure_suggested = function(...) stop("should not be called"),
    .package = "dataquieR"
  )

  expect_invisible(print(list(a = 1)))
})

test_that("print.list() renders single-entity multi-result HTML and respects view=FALSE", {
  # Two dummy "results" that share one entity
  mk_dqr <- function(cn) {
    dqr <- structure(list(), class = "dataquieR_result")
    attr(dqr, "call") <- structure(list(), entity = "AGE_GROUP_0", entity_name = "v00103")
    attr(dqr, "cn") <- cn
    dqr
  }

  x <- list(mk_dqr("A"), mk_dqr(NULL))  # include cn NULL to cover title/cn/nm fallback

  # Keep this branch self-contained and non-interactive.
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(pkg, ...) {
      # Pretend suggests are available so the code continues.
      TRUE
    },
    html_dependency_dataquieR = function(...) {
      # Return a minimal dependency-like object (or NULL is fine for tagList)
      NULL
    },
    html_dependency_jspdf = function(...) NULL,
    util_alias2caption = function(cn, long = TRUE, ...) paste0("CAPTION-", cn),
    util_pretty_print = function(dqr, nm, is_single_var, use_plot_ly, dir, ...) {
      # Minimal HTML output
      htmltools::div(class = "pp", paste("nm:", nm))
    },
    .package = "dataquieR"
  )

  # Avoid launching a browser / viewer
  expect_null(print(x, view = FALSE))
})

test_that("print.list() master_result: warns for stored errors, then falls back via NextMethod()", {
  mk_master <- function(entity, with_err = FALSE) {
    y <- structure(list(), class = c("dataquieR_result", "master_result"))
    attr(y, "call") <- structure(list(), entity = entity, entity_name = "N")
    attr(y, "function_name") <- "acc_dummy"
    attr(y, "error") <- if (with_err) list(simpleWarning("stored warning from result")) else list()
    y
  }

  x <- list(
    mk_master("E1", TRUE),
    mk_master("E2", FALSE)
  )

  testthat::local_mocked_bindings(
    util_stop_if_not = function(...) invisible(TRUE),
    util_combine_res = function(...) stop("should not be called"),
    # IMPORTANT: fallback printing may call this via print.dataquieR_result()
    print.master_result = function(...) invisible(NULL),
    .package = "dataquieR"
  )

  # Reaches warning loop, then NextMethod() triggers default printing.
  expect_warning(
    print(x),
    "stored warning from result"
  )
})
