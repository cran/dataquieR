#' count realizations of missing codes of any class
#'
#' count total numbers of any sort of missing codes (MISSING or JUMP)
#'
#' @param sdf study data
#' @param mdf meta data
#' @param variables variables
#' @param name [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param list [variable attribute] `JUMP_LIST` or `MISSING_LIST`: Count which
#'                                  categories.
#' @param warn [logical] emit warnings on non-numeric missing codes
#'
#' @return a vector with the total number of missings of the class referred by
#'         `list` per `variables`.
#' @importFrom stats aggregate
#'
util_count_codes <- function(sdf, mdf, variables, list, name, warn = TRUE) {
  results <- rep(0, times = length(variables))
  n_codes <- rep(0, times = length(variables))
  warning_list <- vector("character", length = length(variables))
  names(warning_list) <- variables

  for (i in seq_along(variables)) {
    codes <- suppressWarnings(as.numeric(unlist(strsplit(
      as.character(mdf[[list]][mdf[[name]] == variables[i]]), SPLIT_CHAR,
      fixed = TRUE
    ))))
    if (any(is.na(codes))) {
      warning_list[i] <-
        paste0(dQuote(unlist(strsplit(as.character(mdf[[list]][mdf[[name]] ==
                                                                 variables[i]]),
                                      SPLIT_CHAR, fixed = TRUE))[is.na(codes)]),
               collapse = ", ")
    }
    if (!all(is.na(codes))) {
      results[i] <- sum(sdf[[variables[i]]] %in% codes)
      n_codes[i] <- length(codes)
    }
  }

  if (isTRUE(warn) && any(warning_list != "")) {
    warning_list <- aggregate(data.frame(variable = variables),
      data.frame(w = warning_list),
      FUN = function(x) {
        paste0(dQuote(x), collapse = ", ")
      }
    )

    for (w in apply(warning_list[warning_list$w != "", , drop = FALSE], 1,
                    function(row) {
      sprintf("Found %s in %s for %s, which is not numeric\n", row[[1]], list,
              row[[2]])
    })) {
      util_warning(w, applicability_problem = FALSE)
    }
  }

  return(list(results, n_codes))
}
