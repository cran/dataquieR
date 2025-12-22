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
#'         rows follow the vectors returned by `util_check_data_type`.
#' @importFrom stats setNames
#'
#' @seealso [prep_dq_data_type_of]
#' @seealso [prep_datatype_from_data]
#'
#' @family data_management
#' @concept robustness
#' @noRd
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
  sdf[util_empty(sdf)] <- NA # enable robust_na = FALSE option to accelerate -- util_empty keeps structure, important for nrow or ncol < 2
  is_data_type <- lapply(setNames(nm = colnames(sdf)),
                         function(x, check_convertible, threshold_value,
                                  return_percentages, check_conversion_stable) {
    ..vct <- sdf[, x, drop = TRUE]
    attr(..vct, "..cn") <- x
    res <- util_check_data_type(..vct,
                                mdf$DATA_TYPE[mdf[[label_col]] == x],
                                check_convertible = check_convertible,
                                robust_na = FALSE,
                                threshold_value = threshold_value,
                                return_percentages = return_percentages,
                                check_conversion_stable =
                                  check_conversion_stable,
                                vname = x)
    bitsToInt<-function(x) { # https://stackoverflow.com/a/25411493
      packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
    }
    which_vec <- NULL
    try({
      which_vec <- # loosing this on vapply. also: this is too slow.
        apply(as.matrix(do.call(cbind.data.frame, attr(res, "which"))) * 1, 1,
              bitsToInt)
    }, silent = TRUE)

    if (return_percentages) {
      util_attach_attr(setNames(as.numeric(res), nm = names(res)),
                       which_vec = which_vec)
    } else {
      util_attach_attr(as.integer(res), which_vec = which_vec)
    }
  },
  check_convertible = check_convertible,
  threshold_value = threshold_value,
  return_percentages = return_percentages,
  check_conversion_stable = check_conversion_stable)
  which_vec <- NULL
  if (length(res_template) > 1) {
    which_vec <- lapply(is_data_type, attr, "which_vec")
  }
  is_data_type <- vapply(is_data_type, identity, FUN.VALUE = res_template)
  if (length(res_template) > 1) {
    is_data_type <- as.data.frame(is_data_type)
  }
  attr(is_data_type, "which_vec") <- which_vec
  return(is_data_type)
}
