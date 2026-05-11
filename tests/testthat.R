# options("testthat.default_reporter" = testthat::RStudioReporter)

library(dataquieR)

.on_cran <- function() {
  env <- Sys.getenv("NOT_CRAN")
  if (identical(env, "")) {
    !interactive()
  } else {
    !isTRUE(as.logical(env))
  }
}

if (.on_cran()) {
  Sys.setenv(TESTTHAT_CPUS = "1")
}

testthat::test_check("dataquieR")
