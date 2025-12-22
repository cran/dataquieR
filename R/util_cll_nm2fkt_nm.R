#' Get Function called for a Call Name
#'
#' get aliases from report attributes and then replace them by the actual
#' function name
#'
#' @param cll_names [character] then systematic function call name to fetch its
#'                             function name
#' @param report [dataquieR_resultset2] the report
#' @param function_alias_map [data.frame] the `function_alias_map` of a report,
#'                           alternative foe passing the report
#'
#' @return [character] the function name
#'
#' @noRd
util_cll_nm2fkt_nm <- function(cll_names, report, function_alias_map) {
  if (missing(report) && missing(function_alias_map)) {
    vapply(cll_names, function(cll_name) {
      fns <- util_all_ind_functions()
      fkt_name <- unique(names(which(vapply(fns, function(fn) {
        util_startsWith_prefix._or_equals_prefix(
          x = cll_name,
          prefix = fn,
          sep = "_")
      }, FUN.VALUE = logical(1)))))
      if (length(fkt_name) != 1) fkt_name <- cll_name
      # post-process function names
      if (fkt_name %in% c("con_hard_limits",
                          "con_soft_limits",
                          "con_detection_limits")) {
        fkt_name <- "con_limit_deviations"
      } else if (fkt_name %in% c("acc_robust_univariate_outlier")) {
        fkt_name <- "acc_univariate_outlier"
      }

      return(fkt_name)
    }, FUN.VALUE = character(1))
  } else {
    if (missing(function_alias_map)) {
      function_alias_map <-
        attributes(attributes(report)$matrix_list)$function_alias_map
    }
    vapply(cll_names,
           function_alias_map = function_alias_map,
           function(cll_name, function_alias_map) {
      fkt_name <- cll_name
      f <- cll_name
      f <- try(subset(
        function_alias_map,
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
    }, FUN.VALUE = character(1))
  }
}
