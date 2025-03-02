#' utility function applicability of multivariate outlier detection
#'
#' Test for applicability of multivariate outlier detection
#'
#' @param x [data.frame] metadata
#' @param dta [logical] vector, 1=matching data type,
#'                  0 = non-matching data type
#'
#' @return [factor] 0-3 for each variable in metadata
#' - 0 data type mismatch and not applicable
#' - 1 data type mismatches but applicable
#' - 2 data type matches but not applicable
#' - 3 data type matches and applicable
#' - 4 not applicable because of not suitable data type
#'
#' @seealso [pro_applicability_matrix]
#' @keywords internal

util_app_mol <- function(x, dta) {
  aa <- paste0(dta, 1)
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1,
                             "10" = 2, "11" = 3))
  if (is.null(x[[VALUE_LABELS]])) {
    x[[VALUE_LABELS]] <- NA
  }
  if (is.null(x[[VALUE_LABEL_TABLE]])) {
    x[[VALUE_LABEL_TABLE]] <- NA
  }
  if (is.null(x[[STANDARDIZED_VOCABULARY_TABLE]])) {
    x[[STANDARDIZED_VOCABULARY_TABLE]] <- NA
  }
  score <- ifelse(x[["DATA_TYPE"]] == DATA_TYPES$FLOAT |
                    (x[["DATA_TYPE"]] == DATA_TYPES$INTEGER &
    (is.null(x[[VALUE_LABELS]]) | util_empty(x[[VALUE_LABELS]])) &
      (is.null(x[[VALUE_LABEL_TABLE]]) | util_empty(x[[VALUE_LABEL_TABLE]])) &
      (is.null(x[[STANDARDIZED_VOCABULARY_TABLE]]) |
         util_empty(x[[STANDARDIZED_VOCABULARY_TABLE]]))),
    score, 4)
  score <- as.factor(score)
  return(score)
}
