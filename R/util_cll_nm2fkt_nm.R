#' Get Function called for a Call Name
#'
#' get aliases from report attributes and then replace them by the actual
#' function name
#'
#' @param cll_name [character] then systematic function call name to fetch its
#'                             function name
#' @param report [dataquieR_resultset2] the report
#'
#' @return [character] the function name
#'
#' @keywords internal
util_cll_nm2fkt_nm <- function(cll_name, report) {
  f <- cll_name
  f <- try(subset(attributes(
    attributes(report)$matrix_list)$function_alias_map,
    alias == cll_name, "name", drop = TRUE),
    silent = TRUE)
  if (is.vector(f) && is.character(f) && (length(f) == 1)) {
    fkt_name <- f
  }
  # post-process function names
  if (fkt_name %in% c("con_hard_limits",
                      "con_soft_limits",
                      "con_detection_limits")) {
    fkt_name <- "con_limit_deviations"
  } else if (fkt_name %in% c("acc_robust_univariate_outlier")) {
    fkt_name <- "acc_univariate_outlier"
  }

  return(fkt_name)
}
