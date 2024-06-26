#' utility function to test for applicability of marginal means plots
#'
#' Test for applicability of detection limits checks
#'
#' @param x [data.frame] metadata
#' @param dta [logical] vector, 1 = matching data type,
#'                            0 = non-matching data type
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

util_app_mar <- function(x, dta) {
  c1 <- rowSums(!is.na(x[, grep("^GROUP_VAR_", colnames(x),
         perl = TRUE, value = TRUE), drop = FALSE])) > 0
  aa <- paste0(dta, as.integer(c1))
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1, "10" = 2,
                             "11" = 3))
  score <- ifelse(x[["DATA_TYPE"]] %in% c("integer", "float"), score, 4)
  score <- as.factor(score)
  return(score)
}
