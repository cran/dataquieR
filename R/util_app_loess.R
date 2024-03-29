#' utility function for applicability of LOESS smoothed time course plots
#'
#' Test for applicability of LOESS smoothed time course plots
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

util_app_loess <- function(x, dta) {
  c1 <- rowSums(!is.na(x[, grep("^GROUP_VAR_", colnames(x),
                                        perl = TRUE, value = TRUE),
                         drop = FALSE])) > 0

  if (TIME_VAR %in% names(x)) {
    c3 <- ifelse(is.na(x[[TIME_VAR]]), 0, 1)
  } else {
    c3 <- rep(0, times = dim(x)[1])
  }

  c4 <- pmax(c1, c3)
  aa <- paste0(dta, c4)
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1,
                             "10" = 2, "11" = 3))
  score <- ifelse(x[[DATA_TYPE]] %in% c(DATA_TYPES$INTEGER,
                                        DATA_TYPES$FLOAT), score, 4)
  score <- as.factor(score)
  return(score)
}
