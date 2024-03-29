test_that("util_warning works", {
  skip_on_cran()
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  expect_warning(util_warning("The one and everything is %d (%s).", 42,
                              "Douglas Adams"),
                 regexp = ".*The one and everything is 42 .Douglas Adams.*",
                 perl = TRUE)

  testenv <- new.env(parent = baseenv())

  testenv$g <- function(...) {
    util_warning(...)
  }
  environment(testenv$g) <- asNamespace("dataquieR")

  f <- function(...) {
    g(...)
  }
  environment(f) <- testenv

  h <- function(...) {
    do.call(g, list(...))
  }
  environment(h) <- testenv

  expect_warning(f("The one and everything is %d (%s).", 42,
                    "Douglas Adams"),
                 regexp =
                   paste(
                     ".*The one and everything is",
                     "42 \\(Douglas Adams\\).*"
                   ),
                 perl = TRUE)
  expect_warning(h("The one and everything is %d (%s).", 42,
                   "Douglas Adams"),
                 regexp =
                   paste(
                     ".*The one and everything is",
                     "42 \\(Douglas Adams\\).*"
                   ),
                 perl = TRUE)
})
test_that("util_warning works", {
  skip_on_cran()
  skip_if_not_installed("withr")
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
                   dataquieR.ERRORS_WITH_CALLER = TRUE,
                   dataquieR.WARNINGS_WITH_CALLER = TRUE,
                   dataquieR.MESSAGES_WITH_CALLER = TRUE)
  expect_warning(util_warning("The one and everything is %d (%s).", 42,
                              "Douglas Adams"),
                 regexp = "The one and everything is 42 (Douglas Adams).",
                 fixed = TRUE)

  g <- function(...) {
    util_warning(...)
  }
  environment(g) <- asNamespace("dataquieR")

  testenv <- new.env(parent = baseenv())
  testenv$gg <- force(g)

  f <- function(...) {
    gg(...)
  }
  environment(f) <- testenv

  h <- function(...) {
    do.call(gg, list(...))
  }
  environment(h) <- testenv

  expect_warning(f("The one and everything is %d (%s).", 42,
          "Douglas Adams"),
          regexp =
            paste(
              ".*The one and everything is",
              "42 \\(Douglas Adams\\).*"
            ),
          perl = TRUE)
  expect_warning(h("The one and everything is %d (%s).", 42,
         "Douglas Adams"),
         regexp =
           paste(
             ".*The one and everything is",
             "42 \\(Douglas Adams\\).*"
           ),
         ,
       perl = TRUE)

  x <- function(m) {
    warning(m)
  }
  w <- function(w) {
    util_warning(w)
    invokeRestart("muffleWarning")
  }
  expect_warning(
    withCallingHandlers(x(""), warning = w),
    regexp = "Warning",
    all = TRUE,
    fixed = TRUE
  )
  expect_warning(
    withCallingHandlers(x("CAVE CANEM"), warning = w),
    regexp = "CAVE CANEM",
    all = TRUE,
    fixed = TRUE
  )

})
