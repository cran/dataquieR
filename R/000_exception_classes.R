#' An exception class assigned for exceptions caused by long
#' variable labels
#' @noRd
LONG_LABEL_EXCEPTION <- "LONG_LABEL_EXCEPTION"
# FIXME: Use some exception class everywhere with some hierarchical concept for these classes

#' An exception class assigned for exceptions caused by trying to apply
#' a non-applicable indicator function, which is not caused by deficient
#' metadata
#'
#' Also amending meta data could not make the function running, e.g., a test
#' for numbers applied to a character.
#'
#' @export
dataquieR.intrinsic_applicability_problem <-
  "dataquieR.intrinsic_applicability_problem"

#' An exception class assigned for exceptions caused by trying to apply
#' a non-applicable indicator function
#'
#' Amending metadata could make the function running, e.g., a test
#' for missingness without any declared missing codes
#'
#' @export
dataquieR.applicability_problem <-
  "dataquieR.applicability_problem"
