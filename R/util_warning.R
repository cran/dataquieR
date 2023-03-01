#' Produce a warning message with a useful short stack trace.
#'
#' @param m warning message or a [condition]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] warning indicates unsuitable resp_vars
#' @param integrity_indicator [character] the warning is an integrity problem,
#'                                        here is the indicator abbreviation..
#'
#' @param level [integer] level of the warning message (defaults to 0). Higher
#'                        levels are more severe.
#'
#' @return [condition] the condition object, if the execution is not stopped
#'
util_warning <- util_condition_constructor_factory("warning")
