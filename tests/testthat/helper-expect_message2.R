expect_message2 <- function(...) {
  withr::local_options(list(dataquieR.testthat_expect_message_active = TRUE))
  testthat::expect_message(...)
}
