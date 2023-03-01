#' Return the color for a result
#'
#' messages do not cause any coloring, warnings are yellow, errors are red
#'
#' @param result a `dataquieR_resultset2` result
#' @param aspect an aspect/problem category of results
#'               (error, applicability error or data quality issue)
#' @param ... not used
#'
#' @return a color as a grading
util_get_color_for_result <- function(result, aspect =
                                        c("applicability", "error", "issue"
                                          ), ...) {

  ##### preps -----
  util_ensure_suggested("htmltools", "Generating nice tables")

  red <- "red"
  green <- "green"
  yellow <- "yellow"
  grey <- "grey"
  reddishgrey <- "reddishgrey"
  white <- "white"

  aspect <- util_match_arg(aspect, several_ok = FALSE)

  if (aspect == "issue") { # TODO: Fix "error" must be added in squarereportrenderer
    expected_a_result <- (Recall(result, "applicability") %in% c("green", "yellow")) # TODO: Fix error must be added in squarereportrenderer
    if (length(result) == 0 || inherits(result, "dataquieR_NULL")) {
      if (expected_a_result)
        return(reddishgrey)
      else
        return(grey)
    }
  }
  if (aspect == "issue") { ### issue -----

    if (is.data.frame(result$SummaryTable)) {
      if (!is.null(result$SummaryTable$GRADING)) {
        if (all(is.na(result$SummaryTable$GRADING))) {
          return(reddishgrey)
        } else if (any(as.numeric(result$SummaryTable$GRADING) == 1,
                       na.rm = TRUE)) {
          return(red)
        } else {
          return(green)
        }
      } else {
        return(white)
      }
      return(grey)
    }
    return(grey)
  } else { ##### error or applicability -----
    if (length(attr(result, "error")) > 0) { # some error occurred ----
      util_stop_if_not(length(attr(result, "error")) == 1)
      cnd <- attr(result, "error")[[1]]
      applicability_problem <- attr(cnd, "applicability_problem")
      if (is.null(applicability_problem) || is.na(applicability_problem)) {
        applicability_problem <- FALSE
      }
      if (aspect == "applicability" && applicability_problem) {
        return(red) # applicability error was asked and occurred
      } else if (aspect == "error"  && !applicability_problem) {
        return(red) # other error was asked and occurred
      }
      return(grey) # error of the other class (not asked now) occurred
    }
    if (length(attr(result, "warning")) > 0) { # some warning occurred ----
      for (w in attr(result, "warning")) {
        applicability_problem <- attr(w, "applicability_problem")
        if (is.null(applicability_problem) || is.na(applicability_problem)) {
          applicability_problem <- FALSE
        }
        if (aspect == "applicability") {
          if (applicability_problem) {
            return(yellow)
          }
        } else if (aspect == "error") {
          if (!applicability_problem) {
            return(yellow)
          }
        } else util_error("internal error in get color for result: %s",
                          dQuote(aspect))
      }
    }
    return(green)
  }
}
