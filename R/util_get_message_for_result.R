#' Return messages/warnings/notes/error messages for a result
#'
#' @param result a `dataquieR_resultset2` result
#' @param aspect an aspect/problem category of results
#' @param collapse either a lambda function or a separator for combining
#'                 multiple messages for the same result
#' @param ... not used
#'
#' @return hover texts for results with data quality issues,
#'         run-time errors, warnings or notes (aka messages)
util_get_message_for_result <- function(result,
                                   aspect = c("applicability", "error",
                                              "issue", "anamat", "indicator_or_descriptor"),
                                   collapse = "\n<br />\n", ...) {
  aspect <- util_match_arg(aspect, several_ok = FALSE)
  if (!inherits(result, "dataquieR_result")) {
    return("No results computed")
  }
  if (!(aspect %in% c("applicability", "anamat"))) {
    expected_a_result <-
      (util_get_color_for_result(result, "applicability") %in%
         c("green", "yellow")) # TODO: Fix error must be added in squarereportrenderer
    expected_a_result <-
      expected_a_result && (util_get_color_for_result(result, "anamat") !=
                              "red")

  } else {
    expected_a_result <- NA
  }
  if (aspect == "issue") { # issue ----
    if (length(result) == 0 || inherits(result, "dataquieR_NULL")) {
      return("No result available")
    }
    if (is.data.frame(result$SummaryTable)) { # TODO: Make also work for variable groups and so on...
      if (!is.null(result$SummaryTable$GRADING)) {
        if (all(is.na(result$SummaryTable$GRADING))) {
          if (expected_a_result) {
            return("Classification missing")
          } else {
            return("No classification available")
          }
        } else if (any(as.numeric(result$SummaryTable$GRADING) == 1,
                       na.rm = TRUE)) {
          return("Problem detected")
        } else {
          return("No problem detected")
        }
      } else {
        return(sprintf("%s does not comprise a classification.",
                       dQuote(attr(result, "function_name"))))
      }
    }
    if (expected_a_result) {
      return("Classification missing")
    } else {
      return("No classification available")
    }
    # return("") # TODO: add some text describing the issue
  }
  msgs <- character(0)
  if (length(attr(result, "warning")) > 0) {
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
      if (aspect == "applicability") {
        if (applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        }
      } else if (aspect == "anamat") {
        if (applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        }
      } else {
        if (!applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        }
      }
    }
  }
  if (length(attr(result, "message")) > 0) {
    for (w in attr(result, "message")) {
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
      if (aspect %in% c("applicability", "anamat")) {
        if (aspect == "applicability" &&
            applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        } else if ((aspect == "anamat") &&
                    applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        }
      } else {
        if (!applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        }
      }
    }
  }
  if (length(attr(result, "error")) > 0) {
    for (w in attr(result, "error")) {
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
      if (aspect %in% c("applicability", "anamat")) {
        if (aspect == "applicability" &&
            applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        } else if ((aspect == "anamat") &&
                    applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        }
      } else {
        if (!applicability_problem) {
          msgs <- c(msgs, gsub("\n>.*$", "", gsub("^.*?: ", "",
                                                  conditionMessage(w))))
        }
      }
    }
  }
  if (is.function(collapse)) {
    collapse(msgs)
  } else {
    paste0(msgs, collapse = collapse)
  }
}
