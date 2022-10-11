#' Utility function distribution-selection
#'
#' This function differentiates the type of measurement variables.
#'
#' @param measurements study data
#' @param meta_data meta data, not yet used
#'
#' @return data frame with one column for each variable in study data giving
#'                                    `IsInteger`, `IsMultCat` and `IsNCategory`
#'
#' `IsInteger` contains a guess, if the variable contains integer values or is a
#'                             factor
#'
#' `IsMultCat` contains a guess, if the variable has more than two categories,
#'                            if  it is categorical or ordinal
#'
#' `NCategory` contains the number of distinct values detected for the variable
#'
#'
util_dist_selection <- function(measurements, meta_data) {
  .x <- as.data.frame(measurements)
  .r <- data.frame(
    Variables = colnames(.x),
    IsInteger = c(rep(NA, length(colnames(.x)))),
    IsMultCat = c(rep(NA, length(colnames(.x)))),
    NCategory = c(rep(NA, length(colnames(.x))))
  )

  .x[, vapply(FUN.VALUE = logical(1), .x, is.factor)] <-
    vapply(FUN.VALUE = integer(nrow(.x)), .x[, vapply(FUN.VALUE = logical(1),
                                                      .x, is.factor),
                                             drop = FALSE], as.integer)

  # identify integer and negative values
  .r$IsInteger <- vapply(FUN.VALUE = logical(1), .x, function(.y) all(
    util_is_integer(.y), na.rm = TRUE))

  # integer and more than two categories
  multcat <- function(x) {
    ifelse(length(unique(x)) > 2, 1, 0)
  }
  ncat <- function(x) {
    length(unique(x))
  }

  if (dim(.x)[2] == 1) {
    .r$IsMultCat[.r$IsInteger == 1] <-
      ifelse(length(unique(.x[, .r[, "IsInteger"]])) > 2, 1, 0)
    .r$NCategory[.r$IsInteger == 1] <- length(unique(.x[, .r[, "IsInteger"]]))
  } else {
    .r$IsMultCat[.r$IsInteger == 1] <-
      apply(.x[, .r[, "IsInteger"]], 2, multcat)
    .r$NCategory[.r$IsInteger == 1] <- apply(.x[, .r[, "IsInteger"]], 2, ncat)
  }

  .r$IsMultCat[.r$IsInteger == 0] <- 0

  ### Output
  return(Results = .r)
}
