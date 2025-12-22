#' Produce an error message with a useful short stack trace. Then it stops the
#' execution.
#'
#' @param m error message or a [condition]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] `TRUE`, if an applicability issue,
#'                                        that is, the information for
#'                                        computation is missing
#'                                        (that is, an error that indicates
#'                                        missing metadata) or an error because
#'                                        the requirements of
#'                                        the stopped function were not met,
#'                                        e.g., a `barplot` was called for
#'                                        metric data. We can have logical
#'                                        or empirical applicability problems.
#'                                        empirical is the default, if the
#'                                        argument
#'                                        `intrinsic_applicability_problem` is
#'                                        left unset or set to `FALSE`.
#' @param intrinsic_applicability_problem [logical] `TRUE`, if this is a
#'                                        logical applicability issue, that is,
#'                                        the computation makes no sense
#'                                        (for example, an error of unsuitable
#'                                        `resp_vars`). Intrinsic/logical
#'                                        applicability problems are also
#'                                        applicability problems. Non-logical
#'                                        applicability problems are called
#'                                        empirical applicability problems.
#' @param integrity_indicator [character] the message is an integrity problem,
#'                                        here is the indicator abbreviation..
#' @param level [integer] level of the error message (defaults to 0). Higher
#'                        levels are more severe.
#' @param immediate [logical] not used.
#' @param additional_classes [character] additional classes the
#'                                       thrown condition object should inherit
#'                                       from, first.
#'
#' @return nothing, its purpose is to stop.
#'
#' @family condition_functions
#' @concept process
#' @noRd
util_error <- util_condition_constructor_factory("error")
