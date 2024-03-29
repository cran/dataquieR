#' Produce a condition message with a useful short stack trace.
#'
#' @param m a message or a [condition]
#' @param ... arguments for [sprintf] on m, if m is a character
#' @param applicability_problem [logical] error indicates missing metadata
#' @param intrinsic_applicability_problem [logical] error unsuitable resp_vars
#' @param integrity_indicator [character] the message is an integrity problem,
#'                                        here is the indicator abbreviation..
#' @param level [integer] level of the message (defaults to 0). Higher
#'                        levels are more severe.
#' @param immediate [logical] not used.
#'
#' @return [condition] the condition object, if the execution is not stopped
#'
#' @family condition_functions
#' @concept process
#' @keywords internal
util_message <- util_condition_constructor_factory("message")
