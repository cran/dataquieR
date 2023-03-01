#' Produce an error message with a useful short stack trace. Then it stops the
#' execution.
#'
#' @param m error message or a [condition]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] error indicates unsuitable resp_vars
#' @param integrity_indicator [character] the message is an integrity problem,
#'                                        here is the indicator abbreviation..
#' @param level [integer] level of the error message (defaults to 0). Higher
#'                        levels are more severe.
#'
#' @return nothing, its purpose is to stop.
#'
util_error <- util_condition_constructor_factory("error")
