#' utility applicability variance components
#'
#' Test for applicability of ICC
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
util_app_vc <- function(x, dta) {
  if ("KEY_OBSERVER" %in% names(x)) {
    c1 <- ifelse(is.na(x[["KEY_OBSERVER"]]), 0, 1)
  } else {
    c1 <- rep(0, times = dim(x)[1])
  }

  if ("KEY_DEVICE" %in% names(x)) {
    c2 <- ifelse(is.na(x[["KEY_DEVICE"]]), 0, 1)
  } else {
    c2 <- rep(0, times = dim(x)[1])
  }

  c3 <- pmax(c1, c2)
  aa <- paste0(dta, c3)
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1,
                             "10" = 2, "11" = 3))
  score <- ifelse(x[["DATA_TYPE"]] %in% c("integer", "float"), score, 4)
  score <- as.factor(score)
  return(score)
}
