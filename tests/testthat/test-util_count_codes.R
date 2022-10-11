test_that("util_count_code works", {
  local({
    load(system.file("extdata/meta_data.RData", package = "dataquieR"),
         envir = environment())
    load(system.file("extdata/study_data.RData", package = "dataquieR"),
         envir = environment())
    vnames <- c("v00027", "v00029")
    subset(meta_data,
           VAR_NAMES %in% vnames,
           c(MISSING_LIST, JUMP_LIST),
           FALSE)
    j <- setNames(
      util_count_codes(study_data, meta_data, vnames,
                       name = VAR_NAMES,
                       list = JUMP_LIST,
                       warn = TRUE),
      nm = c("results", "ncodes"))
    m <- setNames(
      util_count_codes(study_data, meta_data, vnames,
                       name = VAR_NAMES,
                       list = MISSING_LIST,
                       warn = TRUE),
      nm = c("results", "ncodes"))
    expect_identical(j, list(results = c(1113, 1066), ncodes = c(1, 1)))
    expect_identical(m, list(results = c(499, 519), ncodes = c(9, 9)))

    vnames <- c("v00027", "v00028", "v00029")
    subset(meta_data,
           VAR_NAMES %in% vnames,
           c(MISSING_LIST, JUMP_LIST),
           FALSE)
    j <- expect_warning(
      setNames(
        util_count_codes(study_data, meta_data, vnames,
                         name = VAR_NAMES,
                         list = JUMP_LIST,
                         warn = TRUE),
        nm = c("results", "ncodes")),
      regexp = "Found .+NA.+ in JUMP_LIST for .+, which is not numeric")
    m <- setNames(
      util_count_codes(study_data, meta_data, vnames,
                       name = VAR_NAMES,
                       list = MISSING_LIST,
                       warn = TRUE),
      nm = c("results", "ncodes"))
    expect_identical(j, list(results = c(1113, 0, 1066), ncodes = c(1, 0, 1)))
    expect_identical(m, list(results = c(499, 515, 519), ncodes = c(9, 9, 9)))

    j <- expect_silent(
      setNames(
        util_count_codes(study_data, meta_data, vnames,
                         name = VAR_NAMES,
                         list = JUMP_LIST,
                         warn = FALSE),
        nm = c("results", "ncodes")))
    expect_identical(j, list(results = c(1113, 0, 1066), ncodes = c(1, 0, 1)))

  })
})
