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
                                        c("applicability", "error", "issue",
                                          "anamat", "indicator_or_descriptor"),
                                      ...) {

  ##### preps -----
  util_ensure_suggested("htmltools", "Generating nice tables")

  red <- "red"
  green <- "green"
  yellow <- "yellow"
  grey <- "grey"
  reddishgrey <- "reddishgrey"
  washedoutgreen <- "washedoutgreen"
  white <- "white"

  aspect <- util_match_arg(aspect, several_ok = FALSE)

  ### issue -----
  if (aspect %in% c("issue", "indicator_or_descriptor")) {
    # TODO: Fix "error" must be added in squarereportrenderer
    expected_a_result <-
      (Recall(result, "applicability") %in% c(green, yellow))
    # TODO: Fix error must be added in squarereportrenderer
    expected_a_result <-
      expected_a_result && (Recall(result, "anamat") != grey)
    if (length(result) == 0 || inherits(result, "dataquieR_NULL")) {
      if (expected_a_result)
        return(reddishgrey) # result missing for unknown reasons
      else
        return(grey) # result missing, but because of n.a. (intrinsic or due to missing metadata)
    }

    if (is.data.frame(result$SummaryTable)) {
      if (!is.null(result$SummaryTable$GRADING)) {
        if (all(is.na(result$SummaryTable$GRADING))) {
          return(reddishgrey) # grading column exists but is empty
        } else if (aspect == "issue") {
          if (any(as.numeric(result$SummaryTable$GRADING) == 1, # TODO: what if not only 1/0
                  na.rm = TRUE)) {
            return(red) # issue detected
          } else {
            return(green) # no issue detected
          }
        } else {
          return(green) # It's a data quality indicator
        }
      } else {
        # no grading column provided
        if (aspect == "issue") {
          return(grey) # but, in case we have a SummaryTable, we *should* have a column named GRADING TODO: find a way of reporting this back to the developers
        } else {
          return(yellow) # It's a descriptor
        }
      }
      return(grey) # TODO: should be a line with dead code, unreachable?
    }
    if (aspect == "issue") {
      return(grey) # no data table SummaryTable returned
    } else {
      return(yellow) # It's a descriptor
    }
  } else { ##### not issue, so error or applicability or anamat -----
    if (length(attr(result, "error")) > 0) { # some error occurred ----
      util_stop_if_not(length(attr(result, "error")) == 1)
      cnd <- attr(result, "error")[[1]]
      applicability_problem <- attr(cnd, "applicability_problem")
      if (is.null(applicability_problem) || is.na(applicability_problem)) {
        applicability_problem <- FALSE
      }
      intrinsic_applicability_problem <- attr(cnd,
                                              "intrinsic_applicability_problem")
      if (is.null(intrinsic_applicability_problem) ||
          is.na(intrinsic_applicability_problem)) {
        intrinsic_applicability_problem <- FALSE
      }
      if (aspect == "anamat" && applicability_problem &&
          intrinsic_applicability_problem) {
        return(grey)
      } else if (aspect == "anamat" && applicability_problem &&
          !intrinsic_applicability_problem) {
        return(green)
      } else if (aspect == "anamat" && !applicability_problem) {
        return(green)
      } else if (aspect == "applicability" && applicability_problem &&
                 !intrinsic_applicability_problem) {
        return(red) # applicability error was asked and occurred
      } else if (aspect == "error"  && !applicability_problem) {
        return(red) # other error was asked and occurred
      }
      return(grey) # error of the other class (not asked now) occurred
    }
    # If we have warnings, we cannot return, we have to go over all warnings and return yellow, if we find a warning of the aspect's class
    res <- green
    if (length(attr(result, "warning")) > 0) { # some warning occurred ----
      for (w in attr(result, "warning")) {
        applicability_problem <- attr(w, "applicability_problem")
        if (is.null(applicability_problem) || is.na(applicability_problem)) {
          applicability_problem <- FALSE
        }
        intrinsic_applicability_problem <- attr(w,
                                                "intrinsic_applicability_problem")
        if (is.null(intrinsic_applicability_problem) ||
            is.na(intrinsic_applicability_problem)) {
          intrinsic_applicability_problem <- FALSE
        }
        if (aspect == "anamat" && applicability_problem &&
            intrinsic_applicability_problem) {
          # if (res != yellow)
          #   res <- washedoutgreen # maybe no example possible?
        } else if (aspect == "anamat" && applicability_problem &&
                   !intrinsic_applicability_problem) {
          # if (res != yellow)
          #   res <- green
        } else if (aspect == "anamat" && !applicability_problem) {
          # if (res != yellow)
          #   res <- green
        } else if (aspect == "applicability") {
          if (applicability_problem && !intrinsic_applicability_problem) {
            # res <- yellow
            return(yellow)
          }
        } else if (aspect == "error") {
          if (!applicability_problem) {
            # res <- yellow
            return(yellow)
          }
        } else util_error("internal error in get color for result: %s",
                          dQuote(aspect))
      }
    }
    return(green)
  }
}
