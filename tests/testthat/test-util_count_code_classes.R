test_that("util_count_code_classes works", {
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
      util_count_code_classes(study_data, meta_data, vnames,
                            name = VAR_NAMES,
                            list = JUMP_LIST,
                            warn = TRUE),
                          nm = vnames)
    m <- setNames(
      util_count_code_classes(study_data, meta_data, vnames,
                            name = VAR_NAMES,
                            list = MISSING_LIST,
                            warn = TRUE),
                          nm = vnames)
    expect_identical(j, c(v00027 = 1, v00029 = 1))
    expect_identical(m, c(v00027 = 9, v00029 = 9))

    vnames <- c(vnames, "v50000")
    expect_warning(
      j <- setNames(
        util_count_code_classes(study_data, meta_data, vnames,
                              name = VAR_NAMES,
                              list = JUMP_LIST,
                              warn = TRUE),
        nm = vnames),
      regexp = "Found .+NA.+ in JUMP_LIST for .+v50000.+, which is not numeric",
      perl = TRUE,
      all = TRUE)

    expect_identical(j, c(v00027 = 1, v00029 = 1, v50000 = 0))

  })
})
