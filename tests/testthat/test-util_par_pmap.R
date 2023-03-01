test_that("util_par_pmap works", {
  skip_on_cran() # slow and meant for parallel processing
  meta_data <- prep_get_data_frame("meta_data")
  study_data <- prep_get_data_frame("study_data")
  plan <- dplyr::tribble(
    ~ x, ~ y,
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000)
  )
  r <- unlist(dataquieR:::util_par_pmap(plan, cor, cores = 1),
              recursive = FALSE)
  expect_vector(r, numeric(0), 10)
  expect_equivalent(mapply(plan$x, plan$y, FUN = cor),
                    r)
  myCor <- function(..., meta_data, study_data) {
    testthat::expect_true(exists("meta_data"))
    testthat::expect_true(exists("study_data"))
    cor(...)
  }
  s <- unlist(dataquieR:::util_par_pmap(.l = plan,
                                        .f =  myCor,
                                        meta_data = cars,
                                        study_data = cars,
                                        cores = list(mode = "socket",
                                                     cpus = 1,
                                                     logging = FALSE,
                                                     load.balancing = TRUE),
                                        use_cache = TRUE),
              recursive = FALSE)
  expect_equal(s, r)
})
