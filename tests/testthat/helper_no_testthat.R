without_testthat <- function(code) {
  withr::with_envvar(new = c(TESTTHAT = "false"),
                     code = code)
}
