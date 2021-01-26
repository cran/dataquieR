#' utility function to assign labels to levels
#'
#' function to assign labels to levels of a variable
#'
#' @param variable [vector] vector with values of a study variable
#' @param string_of_levlabs [character] len=1. value labels,
#'                                             e.g. `1 = no | 2 = yes`
#' @param splitchar [character] len=1. splitting character(s) in
#'                                 `string_of_levlabs`, usually [SPLIT_CHAR]
#' @param assignchar [character] len=1. assignment operator character(s) in
#'                                  `string_of_levlabs`, usually ` = ` or `: `
#' @param ordered the function converts `variable` to a [factor], by default to
#'                an [ordered] [factor] assuming LHS of assignments being
#'                meaningful numbers, e.g. `1 = low | 2 = medium | 3 = high`.
#'                If no special order is given, set `ordered` to `FALSE`,
#'                e.g. for `1 = male | 2 = female` or
#'                `1 = low | 2 = high | 3 = medium`.
#'
#' @return a [data.frame] with labels assigned to categorical variables
#'         (if available)
#'
util_assign_levlabs <- function(variable, string_of_levlabs, splitchar,
                                assignchar, ordered = TRUE) {

  # split one string into multiple
  levlab_list <- strsplit(string_of_levlabs, split = splitchar, fixed = TRUE)
  # remove lreading/trailing blanks
  levlab_list <- trimws(unlist(levlab_list), which = "both")

  # get levels
  .levs <- unlist(lapply(levlab_list, function(x)
    unlist(strsplit(x, "=", fixed = TRUE))[1]))
  .levs <- trimws(.levs, which = "both")
  # get labels
  labs <- unlist(lapply(levlab_list, function(x)
    unlist(strsplit(x, "=", fixed = TRUE))[2]))
  labs <- trimws(labs, which = "both")

  # levels to numeric if applicable
  if (suppressWarnings(all(!is.na(as.numeric(.levs))))) {
    levs <- as.numeric(.levs)
  } else {
    levs <- .levs
  }

  # warnings:
  if (length(levs) != length(labs)) {
    # Dead code?
    util_warning("Number of levels does not match number of labels.") # nocov
  }

  if (length(levs) < length(unique(variable[!(is.na(variable))]))) {
    util_warning(
      "Number of levels in variable greater than in character string.")
  }

  if (any(is.na(labs))) {
    util_warning("No labels assigned for some levels, use levels as labels")
    labs[is.na(labs)] <- levs[is.na(labs)]
  }

  # assign levels and labels to vector
  if ((length(levs) == length(labs)) & (length(levs) >=
                                        length(
                                          unique(
                                            variable[!(is.na(variable))])))) {
    variable <- factor(variable, levels = levs,
                       labels = labs, ordered = ordered)
  }

  return(variable)
}
