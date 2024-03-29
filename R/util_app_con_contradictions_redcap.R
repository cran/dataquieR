#' utility function for the applicability of contradiction checks
#'
#' Test for applicability of contradiction checks
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
#'
#' @seealso [pro_applicability_matrix]
#' @keywords internal

util_app_con_contradictions_redcap <- function(x, dta) { # TODO: would need meta_data_cross_item
  c1 <- rep(1, times = dim(x)[1])

  aa <- paste0(dta, c1)
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01"
                             = 1, "10" = 2, "11" = 3))
  score <- as.factor(score)
  return(score)
}
