test_that("util_warning works", {
  expect_warning(util_warning("The one and everything is %d (%s).", 42,
                              "Douglas Adams"),
                 regexp = "The one and everything is 42 (Douglas Adams).",
                 fixed = TRUE)

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
                     "(?s)In g: The one and everything is",
                     "42 \\(Douglas Adams\\)\\.\\s+\\>\\s+g\\(\\.\\.\\.\\)"
                   ),
                 perl = TRUE,
                 all = TRUE)
  expect_warning(h("The one and everything is %d (%s).", 42,
                   "Douglas Adams"),
                 regexp =
                   paste(
                     "(?s)In do\\.call \\(g\\): The one and everything is",
                     "42 \\(Douglas Adams\\)\\.\\s+\\>\\s+do\\.call\\(g,",
                     "list\\(\\.\\.\\.\\)\\)"
                   ),
                 perl = TRUE,
                 all = TRUE)
})
test_that("util_warning works", {
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
           "(?s)In gg: The one and everything is",
          "42 \\(Douglas Adams\\)\\.\\s+\\>\\s+gg\\(\\.\\.\\.\\)"
         ),
       perl = TRUE,
       all = TRUE)
  expect_warning(h("The one and everything is %d (%s).", 42,
         "Douglas Adams"),
       regexp =
         paste(
          "(?s)In do\\.call \\(gg\\): The one and everything is",
           "42 \\(Douglas Adams\\)\\.\\s+\\>\\s+do\\.call\\(gg,",
           "list\\(\\.\\.\\.\\)\\)"
         ),
       perl = TRUE,
       all = TRUE)

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
