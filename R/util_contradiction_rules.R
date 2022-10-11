##### Prefixes:
# cr_vv = variable 1 value <cmp> variable 2 value OR variable 1 available
#                                                   <logic> variable 2 available
# cr_lc = variable 1 in levels <logic> variable 2 <cmp> constant value
# cr_ll = variable 1 in levels <logic> variable 1 in levels
# All functions should carry an description attribute for displaying them

######################################################################
# Detect abnormalities help functions
#
# 2 variables:
#
#    if A != B
#    if A > B
#    if A >= B
#    if A & is.na(B)
#    if A & !(is.na(B))
#    if A & B %in% {set of levels}
#    if A %in% {set of levels} & B >  value
#    if A %in% {set of levels} & B == value
#    if A %in% {set of levels} & B <  value
#    if A %in% {set of levels} & B %in% {set of levels}
#    if A %in% {set of levels} & !(B %in% {set of levels})
#
#############################
# List of generic functions #
#############################
A_not_equal_B_vv <- function(study_data, A, B, A_levels, B_levels,
                             A_value, B_value) {
  X <- study_data

  grading <- ifelse(X[[A]] != X[[B]], 1, 0)
  return(grading)
}
attr(A_not_equal_B_vv, "description") <- "A \u2260 B"

A_less_than_B_vv <- function(study_data, A, B, A_levels, B_levels,
                             A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] < X[[B]], 1, 0)
  return(grading)
}
attr(A_less_than_B_vv, "description") <- "A < B"

A_less_equal_B_vv <- function(study_data, A, B, A_levels, B_levels,
                              A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] <= X[[B]], 1, 0)
  return(grading)
}
attr(A_less_equal_B_vv, "description") <- "A \u2264 B"

A_greater_than_B_vv <- function(study_data, A, B, A_levels, B_levels,
                                A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] > X[[B]], 1, 0)
  return(grading)
}
attr(A_greater_than_B_vv, "description") <- "A > B"

A_greater_equal_B_vv <- function(study_data, A, B, A_levels, B_levels,
                                 A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] >= X[[B]], 1, 0)
  return(grading)
}
attr(A_greater_equal_B_vv, "description") <- "A \u2265 B"

A_present_not_B_vv <- function(study_data, A, B, A_levels, B_levels,
                               A_value, B_value) {
  X <- study_data
  grading <- ifelse(!is.na(X[[A]]) & is.na(X[[B]]), 1, 0)
  return(grading)
}
attr(A_present_not_B_vv, "description") <- "\u2203 A \u2227 \u2204 B"

A_present_and_B_vv <- function(study_data, A, B, A_levels, B_levels,
                               A_value, B_value) {
  X <- study_data
  grading <- ifelse(!is.na(X[[A]]) & !(is.na(X[[B]])), 1, 0)
  return(grading)
}
attr(A_present_and_B_vv, "description") <- "\u2203 A \u2227 \u2203 B"

A_present_and_B_levels_vl <- function(study_data, A, B, A_levels, B_levels,
                                      A_value, B_value) {
  X <- study_data
  grading <- ifelse((!is.na(X[[A]])) & X[[B]] %in% B_levels, 1, 0)
  return(grading)
}
attr(A_present_and_B_levels_vl, "description") <-
  "\u2203 A \u2227 B \u2208 L\u2082"

A_levels_and_B_levels_ll <- function(study_data, A, B, A_levels, B_levels,
                                     A_value, B_value) {
  X <- study_data
  grading <- ifelse(X[[A]] %in% A_levels & X[[B]] %in% B_levels, 1, 0)
  return(grading)
}
attr(A_levels_and_B_levels_ll, "description") <-
  "A \u2208 L\u2081 \u2227 B \u2208 L\u2082"

A_levels_and_B_gt_value_lc <- function(study_data, A, B, A_levels, B_levels,
                                       A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }
  grading <- ifelse(X[[A]] %in% A_levels & X[[B]] > B_value, 1, 0)
  return(grading)
}
attr(A_levels_and_B_gt_value_lc, "description") <- "A \u2208 L \u2227 B > c"

A_levels_and_B_lt_value_lc <- function(study_data, A, B, A_levels, B_levels,
                                       A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }
  grading <- ifelse(X[[A]] %in% A_levels & X[[B]] < B_value, 1, 0)
  return(grading)
}
attr(A_levels_and_B_lt_value_lc, "description") <- "A \u2208 L \u2227 B < c"

#' contradiction_functions
#'
#' Detect abnormalities help functions
#'
#' 2 variables:
#'  - `A_not_equal_B`, if `A != B`
#'  - `A_greater_equal_B`, if `A >= B`
#'  - `A_greater_than_B`, if `A > B`
#'  - `A_less_than_B`, if `A < B`
#'  - `A_less_equal_B`, if `A <= B`
#'  - `A_present_not_B`, if `A & is.na(B)`
#'  - `A_present_and_B`, if `A & !(is.na(B))`
#'  - `A_present_and_B_levels`, if `A & B  %in% {set of levels}`
#'  - `A_levels_and_B_gt_value`, if `A %in% {set of levels} & B >  value`
#'  - `A_levels_and_B_lt_value`, if `A %in% {set of levels} & B <  value`
#'  - `A_levels_and_B_levels`, if
#'                             `A %in% {set of levels} & B %in% {set of levels}`
#'
#'
#' @export
contradiction_functions <- objects(pattern = ".*")
contradiction_functions <- mget(contradiction_functions)
contradiction_functions <- contradiction_functions[
  vapply(contradiction_functions, is.function, TRUE)]
contradiction_functions <- contradiction_functions[
  vapply(contradiction_functions, function(object) {
  !is.null(attr(object, "description"))
}, TRUE)]

#' description of the contradiction functions
#' @export
contradiction_functions_descriptions <- lapply(contradiction_functions, attr,
                                               "description")

######################################################################
#
#    TBD
#
#    > 2 Variablen
#
#    if B == level & A1 < A2
#    if A == level & sum(!(is.na(B))) > 0
#    if A == level & sum(!(is.na(B))) == 0
#    if A == level & sum(B) == 0
#    if abs(sum(sign(A-B))) == length(A)
#    if A1 %in% {set of levels} & sum(c(B) %in% {set of levels}) > 0
#
