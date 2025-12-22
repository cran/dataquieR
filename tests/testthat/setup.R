withr::local_options(
  dataquieR.lazy_plots_gg_compatibility = "FALSE",
  .local_envir = testthat::teardown_env()
)
