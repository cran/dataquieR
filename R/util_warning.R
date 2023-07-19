#' Produce a warning message with a useful short stack trace.
#'
#' @param m warning message or a [condition]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] error indicates missing metadata
#' @param intrinsic_applicability_problem [logical] error unsuitable resp_vars
#' @param integrity_indicator [character] the warning is an integrity problem,
#'                                        here is the indicator abbreviation..
#'
#' @param level [integer] level of the warning message (defaults to 0). Higher
#'                        levels are more severe.
#' @param immediate [logical] Display the warning immediately, not only, when
#'                            the interactive session comes back.
#'
#' @return [condition] the condition object, if the execution is not stopped
#'
util_warning <- util_condition_constructor_factory("warning")
