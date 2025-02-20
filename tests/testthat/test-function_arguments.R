test_that("indic-fkt-args-order", {
  skip_on_cran() # internal conventions

  ind_fkts <- util_all_ind_functions()

  .desired_order <-
    c(
      "resp_vars",
      "group_vars",
      "time_vars",
      "co_vars",
      "study_data",
      "label_col",
      "item_level",
      "meta_data",
      "meta_data_v2"#,
      # ...
    )

  # skip_if_not_installed("devtools")
  # devtools::document()
  # util_load_manual(TRUE)
  mismatch <- !vapply(FUN.VALUE = logical(1),
                     setNames(nm = ind_fkts), function(fkt) {
    desired_order <- .desired_order
    fmls <- names(formals(fkt))
    if ("resp_vars" %in% fmls &&
        which(fmls == "resp_vars") != 1) { # resp_vars can only once in the argument list
      util_message("%s should be the 1st arg in %s",
                   dQuote("resp_vars"),
                   dQuote(fkt))
      return(TRUE)
    } else if (!("resp_vars" %in% fmls)) {
      # if the function does not work on item_level,
      # the _vars arguments are less important
      desired_order <- desired_order[!endsWith(desired_order,
                                               "_vars")]
      desired_order <- setdiff(desired_order, "label_col")
    }
    arg_order <-
      match(fmls, desired_order)
    arg_order <-
      arg_order[!is.na(arg_order)]
    # order of arguments should follow the order in desired_order
    diff_arg_order <- diff(rank(arg_order))
    all_args_in_order <- all(diff_arg_order == 1, na.rm = TRUE)
    if (!all_args_in_order) {
      util_message("args not in order: %s ; Function: %s",
                   prep_deparse_assignments(match(fmls, desired_order),
                                            fmls),
                   dQuote(fkt))
    }
    all_args_in_order
  })
  functions_with_wrong_arg_order <-
    names(which(mismatch))
  expect_equal(functions_with_wrong_arg_order, character(0))
})

test_that("indic-fkt-args-miss", {
  skip_on_cran() # internal conventions

  ind_fkts <- util_all_ind_functions()

  all_or_none <- list(
    c("meta_data", "item_level"),
    c("meta_data_segment", "segment_level"),
    c("meta_data_dataframe", "dataframe_level"),
    c("meta_data_cross_item", "cross-item_level","cross_item_level")
    # TODO: computed items
  )

  names(all_or_none) <- vapply(all_or_none, `[`, 1, FUN.VALUE = character(1))

  mismatch <- vapply(FUN.VALUE = logical(1),
                      setNames(nm = ind_fkts), function(fkt) {
                        fmls <- names(formals(fkt))
                        any(vapply(all_or_none, function(aon) {
                          any(aon %in% fmls) && !all(aon %in% fmls)
                        }, FUN.VALUE = logical(1)))
                      })
  functions_incomplete_arg_list <-
    names(which(mismatch))
  expect_equal(functions_incomplete_arg_list, character(0))
})

test_that("indic-fkt-args-have-mdv2", {
  skip_on_cran() # internal conventions

  ind_fkts <- util_all_ind_functions()

  mismatch <- vapply(FUN.VALUE = logical(1),
                     setNames(nm = ind_fkts), function(fkt) {
                       fmls <- names(formals(fkt))
                       !"meta_data_v2" %in% fmls
                     })
  functions_no_meta_data_v2 <-
    names(which(mismatch))
  expect_equal(functions_no_meta_data_v2, character(0))

  mismatch <- vapply(FUN.VALUE = logical(1),
                     setNames(nm = ind_fkts), function(fkt) {
                       fmls <- names(formals(fkt))
                       if ("meta_data_v2" %in% fmls) {
                         bd <- paste0(as.character(body(fkt)),
                                      collapse = "\n")
                         !grepl("(^|\n)\\s*util_maybe_load_meta_data_v2\\(",
                                bd,
                                perl = TRUE)
                       } else {
                         FALSE
                       }
                     })
  functions_w_meta_data_v2_wo_util_maybe_load_meta_data_v2 <-
    names(which(mismatch))
  expect_equal(functions_w_meta_data_v2_wo_util_maybe_load_meta_data_v2,
               character(0))

})
