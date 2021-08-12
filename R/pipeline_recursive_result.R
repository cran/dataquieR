#' Function to convert a pipeline result data frame to named encapsulated lists
#'
#' @description
#' This function converts a data frame to a recursive list structure based on
#' columns selected for grouping
#'
#' @details
#' The data frame columns for the arguments of a certain row/computation from
#' the calling plan translate to levels in the encapsulated list hierarchy.
#' The order of the levels can be specified in the `result_groups` argument.
#'
#'
#' @param call_plan_with_results [data.frame] result from [pipeline_vectorized]
#' @param result_groups [character] arguments to group by
#'
#' @return a list with:
#'   - first argument's values in `result_groups`, each containing second's
#'     argument's values as a similar list recursively
#'
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' call_plan_with_results <- structure(list(
#'   resp_vars =
#'     c(
#'       "SBP_0", "DBP_0", "VO2_CAPCAT_0",
#'       "BSG_0"
#'     ), group_vars = c(
#'     "USR_BP_0", "USR_BP_0", "USR_VO2_0",
#'     "USR_BP_0"
#'   ), co_vars = list("SEX_0", "SEX_0", "SEX_0", "SEX_0")
#' ),
#' class = "data.frame", row.names = c(
#'   NA,
#'   -4L
#' )
#' )
#' call_plan_with_results[["results"]] <-
#'   list(NA, 2, "Hello", ggplot2::ggplot())
#' result_groups <-
#'   colnames(call_plan_with_results)[2:(ncol(call_plan_with_results) - 1)]
#' pipeline_recursive_result(call_plan_with_results, result_groups)
#' pipeline_recursive_result(call_plan_with_results, rev(result_groups))
pipeline_recursive_result <- function(call_plan_with_results,
                                      result_groups =
                                        setdiff(
                                          colnames(call_plan_with_results),
                                           c(NA, "results", "resp_vars"))
                                        ) {
  if (!is.data.frame(call_plan_with_results)) {
    util_error("call_plan_with_results must be a data frame as returned by %s",
      "pipeline_vectorize(..., result_groups = NULL)")
  }
  if (ncol(call_plan_with_results) == 1) {
    return(call_plan_with_results[[1]])
  }
  if (ncol(call_plan_with_results) < 2) {
    util_error("call_plan_with_results must be a data frame as returned by %s",
      "pipeline_vectorize(..., result_groups = NULL)")
  }
  if (2 > (ncol(call_plan_with_results) - 1)) {
    return(call_plan_with_results)
  }
  if (!length(result_groups) > 0 ||
      !all(is.character(result_groups))) {
    util_error("argument result_groups must be character and length > 0")
  }
  if (!all(result_groups %in% colnames(call_plan_with_results))) {
    util_warning(c(
      "Not all desired result groups correspond to columns in the call_plan.",
      "Remove the unknowns."), applicability_problem = FALSE)
    result_groups <- intersect(result_groups, colnames(call_plan_with_results))
  }
  if (any(result_groups %in% c("resp_vars", "results"))) {
    util_warning(c(
      "resp_vars and results cannot be used as result_groups."),
      applicability_problem = FALSE)
    result_groups <- setdiff(result_groups, c("resp_vars", "results"))
  }
  if (!length(result_groups) > 0 ||
      !all(is.character(result_groups))) {
    util_error("argument result_groups must be character and length > 0")
  }
  k <- result_groups[[1]]
  nas <- call_plan_with_results[, k]
  nas[is.na(nas)] <- "N/A"
  complex_results <- names(nas)[vapply(nas, function(r) length(dim(r)) > 0,
                    FUN.VALUE = logical(1))]
  if (k %in% c("co_vars", complex_results)) {
    # covars = age, sex should not translate into two levels
    # also don't split on stuff like missing-tables, or contradiction rules
    # ususally, this
    res <- list(call_plan_with_results)
    names(res) <- paste(k, paste(unique(unlist(res[[1]][[k]])),
                                 collapse = ", "), sep = " = ")
  } else {
    res <- split(call_plan_with_results, f = nas)
    names(res) <- paste(k, names(res), sep = " = ")
  }
  if (length(result_groups) == 1) {
    return(lapply(res, function(r) {
      if (any(duplicated(r[, 1, TRUE]))) {
        util_warning(paste(
          "Not each parameter has been selected to create a",
          "recursion level. You may miss some results with identical names.",
          "Please check your %s-argument.", collapse = " "),
          sQuote("result_groups"),
          applicability_problem = FALSE
        )
      }
      r <- r[, c(1, ncol(r)), drop = FALSE]
      if (length(r[[2]]) > 0) {
        setNames(r[[2]], nm = paste(colnames(r)[[1]], r[[1]], sep = " = "))
      } else {
        # likely dead code, at least could not reach this line though
        # tried hard.
        NULL # nocov
      }
    }))
  } else {
    return(lapply(res, pipeline_recursive_result, result_groups =
                    result_groups[2:length(result_groups)]))
  }
}
