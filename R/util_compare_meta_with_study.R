#' Compares study data data types with the ones expected according to the
#' metadata
#'
#' Utility function to compare data type of study data with those defined
#' in metadata
#'
#' @param sdf the [data.frame] of study data
#' @param mdf the [data.frame] of associated static metadata
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param check_convertible [logical] also try, if a conversion to the
#'                                    declared data type would work.
#' @param check_conversion_stable [logical] do not distinguish convertible
#'                                          from convertible, but with issues
#' @param threshold_value [numeric] from=0 to=100. percentage failing
#'                                  conversions allowed if `check_convertible`
#'                                  is `TRUE`.
#' @param return_percentages [logical] return the percentage of
#'                                mismatches.
#'
#' @return for `return_percentages == FALSE`: if `check_convertible` is `FALSE`,
#'         a binary vector `(0, 1)` if data type applies,
#'         if `check_convertible` is `TRUE``
#'         a vector with the states `0, 1, 2, 3`: 0 = Mismatch, not convertible
#'                                                1 = Match
#'                                                2 = Mismatch, but convertible
#'                                                3 = Mismatch, convertible, but
#'                                                    with issues (e.g., loss
#'                                                    of decimal places)
#'         for `return_percentages == TRUE`: a data frame with percentages of
#'         non-matching datatypes according, each column is a variable, the
#'         rows follow the vectors returned by [util_check_data_type].
#' @importFrom stats setNames
#'
#' @seealso [prep_dq_data_type_of]
#' @seealso [prep_datatype_from_data]
#'
#' @family data_management
#' @concept robustness
#' @keywords internal
#'
util_compare_meta_with_study <- function(sdf, mdf, label_col,
                                         check_convertible = FALSE,
                                         threshold_value = 0,
                                         return_percentages = FALSE,
                                         check_conversion_stable = FALSE
                                         ) {
  if (any(trimws(colnames(sdf)) == "")) {
    sdf[, trimws(colnames(sdf)) == ""] <- paste0("v",
                                                 seq_len(
                                                   sum(trimws(colnames(sdf)) ==
                                                         "")))
    util_warning("Found columns w/o names in %s, using dummy names",
               dQuote(label_col),
               applicability_problem = TRUE)
  }
  if (any(trimws(mdf[[label_col]]) == "")) {
    mdf[trimws(mdf[[label_col]]) == "", label_col] <-
      paste0("v", seq_len(sum(trimws(mdf[[label_col]]) == "")))
    util_warning("Found empty labels in %s, using dummy labels",
               sQuote("study_data"), applicability_problem = TRUE)
  }
  missing_in_study_data <- !(mdf[[label_col]] %in% colnames(sdf))
  sdf <- sdf[, as.character(mdf[[label_col]][!missing_in_study_data]),
             drop = FALSE]
  if (return_percentages) {
    if (!check_conversion_stable) {
      util_error("%s = %s is only allowed, if %s = %s",
                  sQuote("check_conversion_stable"),
                  sQuote("FALSE"),
                  sQuote("return_percentages"),
                  sQuote("FALSE")
                 )
    }
    if (check_convertible)
      res_template <- numeric(4)
    else
      res_template <- numeric(1)
  } else {
    res_template <- integer(1)
  }
  sdf[trimws(sdf) == ""] <- NA # enable robust_na = FALSE option to accelerate
  is_data_type <- vapply(setNames(nm = colnames(sdf)),
                         function(x, check_convertible, threshold_value,
                                  return_percentages, check_conversion_stable) {
    res <- util_check_data_type(sdf[, x, drop = TRUE],
                                mdf$DATA_TYPE[mdf[[label_col]] == x],
                                check_convertible = check_convertible,
                                robust_na = FALSE,
                                threshold_value = threshold_value,
                                return_percentages = return_percentages,
                                check_conversion_stable =
                                  check_conversion_stable)
    if (return_percentages) {
      setNames(as.numeric(res), nm = names(res))
    } else {
      as.integer(res)
    }
  }, FUN.VALUE = res_template,
    check_convertible = check_convertible,
    threshold_value = threshold_value,
    return_percentages = return_percentages,
    check_conversion_stable = check_conversion_stable)
  if (length(res_template) > 1) {
    is_data_type <- as.data.frame(is_data_type)
  }
  return(is_data_type)
}
