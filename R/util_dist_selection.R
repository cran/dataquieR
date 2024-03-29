#' Utility function to characterize study variables
#'
#' This function summarizes some properties of measurement variables.
#'
#' @param study_data study data, pre-processed with `prep_prepare_dataframes`
#'                   to replace missing value codes by `NA`
#' @param val_lab    matching metadata column containing the `VALUE_LABELS`
#'                   as vector (if available)
#'
#' @return data frame with one row for each variable in the study data and the
#'         following columns:
#' `Variables` contains the names of the variables
#' `IsInteger` contains a check whether the variable contains integer values
#'             only (variables coded as factor will be converted to integers)
#' `IsMultCat` contains a check for variables with integer or string values
#'             whether there are more than two categories
#' `NCategory` contains the number of distinct values for variables with
#'             values coded as integers or strings (excluding `NA` and
#'             empty entries)
#' `AnyNegative` contains a check whether the variable contains any negative
#'             values
#' `NDistinct` contains the number of distinct values
#' `PropZeroes` reports the proportion of zeroes
#'
#'
#' @family metadata_management
#' @concept process
#' @keywords internal


util_dist_selection <- function(study_data, val_lab = NULL) {
  # TODO: discuss function name (does not select a distance), could be named 'util_vars_properties' instead
  .x <- as.data.frame(study_data)
  .r <- data.frame(
    Variables   = colnames(.x),
    IsInteger   = c(rep(NA, length(colnames(.x)))),
    IsMultCat   = c(rep(NA, length(colnames(.x)))),
    NCategory   = c(rep(NA, length(colnames(.x)))),
    AnyNegative = c(rep(NA, length(colnames(.x)))),
    NDistinct   = c(rep(NA, length(colnames(.x)))),
    PropZeroes  = c(rep(NA, length(colnames(.x))))#,
    # HasValueLabels = c(rep(NA, length(colnames(.x))))
  )

  # convert variables coded as factors to integers
  .x[, vapply(FUN.VALUE = logical(1), .x, is.factor)] <-
    vapply(FUN.VALUE = integer(nrow(.x)),
           .x[, vapply(FUN.VALUE = logical(1), .x, is.factor), drop = FALSE],
           as.integer)

  # identify integer values (includes both positive and negative values)
  .r$IsInteger <- vapply(FUN.VALUE = logical(1), .x,
                         function(.y)
                           all(util_is_integer(.y), na.rm = TRUE))

  # identify negative values
  .r$AnyNegative <- vapply(FUN.VALUE = logical(1), .x,
                           function(.y)
                             any(.y < 0, na.rm = TRUE))

  # count the number of distinct values, excluding `NA`s and empty fields
  .r$NDistinct <- vapply(FUN.VALUE = numeric(1), .x,
                         function(.y)
                           length(unique(.y[which(!util_empty(.y))])))

  # calculate the proportion of zeroes
  .r$PropZeroes <- vapply(FUN.VALUE = numeric(1), .x,
                          function(.y)
                            length(which(.y == 0)) /
                            length(.y[which(!util_empty(.y))]))

  # # check whether the variable has (matching) value labels
  # if (!is.null(val_lab)) {
  #   .r$HasValueLabels <-
  #     vapply(FUN.VALUE = logical(1), seq_len(ncol(.x)),
  #            function(i) {
  #                if (all(util_empty(.x[, i]))) {
  #                  !util_empty(val_lab[i]) &&
  #                    length(util_parse_assignments(val_lab[i]),
  #         split_on_any_split_char = TRUE, split_char = c(SPLIT_CHAR, '<')) > 0
  #                } else {
  #                  # return TRUE only if there is at least one matching
  #                  # value label
  #                  !util_empty(val_lab[i]) &&
  #                    length(intersect(
  #                      unique(.x[, i]),
  #                      names(util_parse_assignments(val_lab[i],
  #           split_on_any_split_char = TRUE, split_char = c(SPLIT_CHAR, '<')))
  #                    )) > 0
  #                }
  #            })
  #   }

  is_char <- vapply(FUN.VALUE = logical(1), .x,
                    function(.y)
                      is.character(.y))
  # number of categories (can be coded as integer or as string)
  # ind_get_cat <- which(.r$IsInteger | is_char | .r$HasValueLabels)
  ind_get_cat <- which(.r$IsInteger | is_char)
  .r$IsMultCat[ind_get_cat] <- FALSE
  if (length(ind_get_cat) > 0) {
    .r$NCategory[ind_get_cat] <-
      vapply(FUN.VALUE = numeric(1), .x[, ind_get_cat, drop = FALSE],
             function(.y)
               length(unique(.y[which(!util_empty(.y))])))
    # more than two categories (can be coded as integer or as string)
    .r$IsMultCat[ind_get_cat] <- ifelse(.r$NCategory[ind_get_cat] > 2,
                                        TRUE, FALSE)
  }

  ### Output
  return(Results = .r)
}
