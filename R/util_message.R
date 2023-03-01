#' Produce a condition message with a useful short stack trace.
#'
#' @param m a message or a [condition]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] message indicates unsuitable resp_vars
#' @param integrity_indicator [character] the message is an integrity problem,
#'                                        here is the indicator abbreviation..
#' @param level [integer] level of the message (defaults to 0). Higher
#'                        levels are more severe.
#'
#' @return [condition] the condition object, if the execution is not stopped
#'
util_message <- util_condition_constructor_factory("message")
