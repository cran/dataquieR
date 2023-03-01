#' Utility function to parse intervals
#'
#' @param int an interval as string, e.g., "[0;Inf)"
#'
#' @return the parsed interval with elements `inc_l` (Is the lower limit
#' included?), `low` (the value of the lower limit), `inc_u` (Is the upper
#' limit included?), `upp` (the value of the upper limit)

util_parse_interval <- function(int) {
  if (is.na(int)) {
    return(NA)
  }
  if (exists(int, .interval_cache)) {
    return(get(int, .interval_cache))
  }
  r <- util_eval_rule(util_parse_redcap_rule(int,
                                             entry_pred = "interval",
                                             must_eof = TRUE))
  if (!inherits(r, "interval") || r$low > r$upp) {
    r <- NA
  }
  assign(int, r, .interval_cache)
  r
}

.interval_cache <- new.env(parent = emptyenv())
