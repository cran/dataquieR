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
util_app_iav <- function(x, dta) {
  if ("HARD_LIMITS" %in% names(x)) {
    c1 <- ifelse(is.na(x[["HARD_LIMITS"]]), 0, 1)
  } else {
    c1 <- rep(0, times = dim(x)[1])
  }

  aa <- paste0(dta, c1)
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1,
                             "10" = 2, "11" = 3))
  score <- ifelse(x[["DATA_TYPE"]] %in% c("float", "integer"), score, 4)
  score <- as.factor(score)
  return(score)
}
