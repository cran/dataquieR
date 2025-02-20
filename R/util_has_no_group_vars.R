#' Utility function to check whether a variable has no grouping variable assigned
#'
#' @inheritParams .template_function_indicator
#' @param resp_vars [variable list] the name of a measurement variable
#'
#' @return boolean
#'
util_has_no_group_vars <- function(resp_vars,
                                   label_col = LABEL,
                                   meta_data = "item_level") {
  util_expect_scalar(resp_vars)
  util_expect_data_frame(meta_data)
  columns <- grep("^GROUP_VAR_.*", colnames(meta_data), value = TRUE)
  resp_vars <- util_find_var_by_meta(resp_vars,
                                     meta_data = meta_data,
                                     label_col = label_col)
  if (is.na(resp_vars)) { # nocov start
    return(TRUE) # likely not reachable because of util_expet_scalar, above
  } # nocov end
  rowSums(!util_empty(meta_data[meta_data[["VAR_NAMES"]] == resp_vars,
                                columns, FALSE])) == 0
}
