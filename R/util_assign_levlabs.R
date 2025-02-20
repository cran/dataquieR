#' utility function to assign labels to levels
#'
#' function to assign labels to levels of a variable
#'
#' DEPRECATED from v2.5.0
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
#' @param variable_name [character] the name of the variable being converted
#'                                  for warning messages
#' @param warn_if_inadmissible [logical] warn on [con_inadmissible_categorical]
#'                                       values
#'
#' @return a factor with labels assigned to categorical variables
#'         (if available)
#'
#' @family data_management
#' @concept data_management
#' @keywords internal
util_assign_levlabs <- function(variable, string_of_levlabs, splitchar,
                                assignchar, ordered = TRUE, variable_name = "",
                                warn_if_inadmissible = TRUE) {
  # FIXME: deprecate
  # lifecycle::deprecate_soft("2.5.0",
  #                           what = "util_assign_levlabs()",
  #                           with = "prep_prepare_dataframes()")
  # TODO: handle VALUE_LABELS w/o codes, e.g. male | female
  util_expect_scalar(variable_name, check_type = is.character)
  if (!util_empty(variable_name)) {
    variable_name <- sprintf(" for variable %s", dQuote(variable_name))
  }
  util_expect_scalar(string_of_levlabs, check_type = is.character)
  util_expect_scalar(splitchar, check_type = is.character)
  util_expect_scalar(assignchar, check_type = is.character)
  util_expect_scalar(ordered, check_type = is.logical)
  util_expect_scalar(variable, # not nec. numeric
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     allow_na = TRUE)

  #TODO: util_deparse_assignments
  #TODO: rewrite, user should not need ot lookup metadata, this could be done here
  #TODO: make available as a prep funciton
  # split one string into multiple
  levlab_list <- strsplit(string_of_levlabs, split = splitchar, fixed = TRUE)
  # remove lreading/trailing blanks
  levlab_list <- trimws(unlist(levlab_list), which = "both")

  levlab_list <- levlab_list[!util_empty(levlab_list)]

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
    util_warning("Number of levels does not match number of labels%s.", # nocov
                 variable_name, applicability_problem = TRUE) # nocov
  }

  if (warn_if_inadmissible && length(levs) <
      length(unique(variable[!(is.na(variable))]))) {
    util_warning(
      "Number of levels in variable greater than in character string%s.",
      variable_name, applicability_problem = TRUE)
  }

  if (warn_if_inadmissible && any(is.na(labs))) {
    util_warning("No labels assigned for some levels, use levels as labels%s",
                 variable_name, applicability_problem = TRUE)
    labs[is.na(labs)] <- levs[is.na(labs)]
  }

  # assign levels and labels to vector
  if ((length(levs) == length(labs))) {
    # & (length(levs) >=
    #                                     length(
    #                                       unique(
    #                                         variable[!(is.na(variable))])))) {
    if (warn_if_inadmissible & !all(variable %in% c(NA, levs))) {
      util_warning(
        c("Inadmissible categorical values found, use levels as labels%s"),
                   variable_name, applicability_problem = TRUE)
      inadm <- unique(variable[!(variable %in% c(NA, levs))])
      levs <- c(levs, inadm)
      labs <- c(labs, inadm)
    }
    variable <- factor(variable, levels = levs,
                       labels = labs, ordered = ordered)
  }

  return(variable)
}
