#' utility function applicability of item missingness
#'
#' Test for applicability of item missingness
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
#'
#' @importFrom dplyr recode
#'
util_app_im <- function(x, dta) {
  if (MISSING_LIST %in% names(x)) {
    c1 <- ifelse(is.na(x[[MISSING_LIST]]) |
                   "" == trimws(gsub(SPLIT_CHAR, "", x[[MISSING_LIST]],
                               fixed = TRUE
                               )), 0, 1)
  } else {
    c1 <- rep(0, times = dim(x)[1])
  }

  if (JUMP_LIST %in% names(x)) {
    c2 <- ifelse(is.na(x[[JUMP_LIST]]) |
                   "" == trimws(gsub(SPLIT_CHAR, "", x[[JUMP_LIST]],
                                     fixed = TRUE
                   )), 0, 1)
  } else {
    c2 <- rep(0, times = dim(x)[1])
  }

  mda <- c1 | c2
  aa <- paste0(dta, as.numeric(mda))
  score <- as.numeric(recode(as.factor(aa), "00" = 0, "01" = 1,
                             "10" = 2, "11" = 3))
  score <- as.factor(score)
  return(score)
}
