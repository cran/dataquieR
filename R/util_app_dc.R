#' utility function for the applicability of of distribution plots
#'
#' Test for applicability of distribution plots
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
util_app_dc <- function(x, dta) {
  aa <- paste0(dta, 1)
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1,
                             "10" = 2, "11" = 3))
  score <- ifelse(x[["DATA_TYPE"]] %in% c("integer", "float"), score, 4)
  score <- as.factor(score)
  return(score)
}
