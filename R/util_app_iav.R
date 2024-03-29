#' utility function for the applicability of numeric admissibility
#'
#' Test for applicability of numeric admissibility
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

util_app_iav <- function(x, dta) {
  c1 <- rep(0, times = dim(x)[1])
  if (HARD_LIMITS %in% names(x))
    c1 <- c1 | !is.na(x[[HARD_LIMITS]])
  if (SOFT_LIMITS %in% names(x))
    c1 <- c1 | !is.na(x[[SOFT_LIMITS]])

  aa <- paste0(dta, as.numeric(c1))
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1,
                             "10" = 2, "11" = 3))
  score <- ifelse(x[["DATA_TYPE"]] %in% c("float", "integer", "datetime"), score, 4)
  score <- as.factor(score)
  return(score)
}
